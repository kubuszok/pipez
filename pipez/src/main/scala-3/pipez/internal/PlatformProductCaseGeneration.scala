package pipez.internal

import pipez.internal.Definitions.{ Context, Result }

import scala.collection.immutable.ListMap
import scala.util.chaining.*
import scala.quoted.{ Type as _, * }

private[internal] trait PlatformProductCaseGeneration[Pipe[_, _], In, Out]
    extends ProductCaseGeneration[Pipe, In, Out] {
  self: PlatformDefinitions[Pipe, In, Out] & PlatformGenerators[Pipe, In, Out] =>

  import quotes.*
  import quotes.reflect.*

  final def isTuple[A: Type]: Boolean =
    TypeRepr.of[A].typeSymbol.fullName.startsWith("scala.Tuple")

  final def isCaseClass[A: Type]: Boolean =
    val sym = TypeRepr.of[A].typeSymbol
    sym.isClassDef && sym.flags.is(Flags.Case) && !sym.flags.is(Flags.Abstract) && isPublic(sym.primaryConstructor)

  final def isCaseObject[A: Type]: Boolean =
    val sym          = TypeRepr.of[A].typeSymbol
    def isScala2Enum = sym.flags.is(Flags.Case | Flags.Module)
    def isScala3Enum = sym.flags.is(Flags.Case | Flags.Enum | Flags.JavaStatic)
    (isScala2Enum || isScala3Enum) && isPublic(sym)

  final def isJavaBean[A: Type]: Boolean =
    val sym = TypeRepr.of[A].typeSymbol
    sym.isClassDef && !sym.flags.is(Flags.Abstract) && sym.declarations.exists(
      isDefaultConstructor
    ) && (sym.declaredMethods.exists(isJavaSetter) || sym.declaredMethods.exists(isVar))

  private def isPublic(sym: Symbol): Boolean =
    !sym.flags.is(Flags.Private) && !sym.flags.is(Flags.PrivateLocal) && !sym.flags.is(Flags.Protected)

  private def isDefaultConstructor(ctor: Symbol): Boolean =
    ctor.isClassConstructor && ctor.paramSymss.filterNot(_.exists(_.isType)).flatten.isEmpty && isPublic(ctor)

  private def isJavaGetter(getter: Symbol): Boolean =
    ProductCaseGeneration.isGetterName(getter.name) && getter.paramSymss.flatten.isEmpty && isPublic(getter)

  private def isJavaSetter(setter: Symbol): Boolean =
    ProductCaseGeneration.isSetterName(setter.name) && setter.paramSymss.flatten.size == 1 && isPublic(setter)

  private def isVar(setter: Symbol): Boolean =
    setter.isValDef && setter.flags.is(Flags.Mutable)

  final def extractProductInData(settings: Settings): DerivationResult[ProductInData] = {
    val sym = TypeRepr.of[In].typeSymbol
    // apparently each case field is duplicated: "a" and "a ", "_1" and "_1" o_0 - the first is method, the other val
    // the exceptions are cases in Scala 3 enum: they only have vals
    (sym.caseFields.filter(if (sym.flags.is(Flags.Enum)) _.isValDef else _.isDefDef) ++ sym.declaredMethods.filter(
      isJavaGetter
    )).map { method =>
      val name = method.name.toString
      name -> ProductInData.Getter[Any](
        name = name,
        tpe = returnType[Any](TypeRepr.of[In].memberType(method)),
        get =
          if (method.paramSymss.isEmpty) (in: Expr[In]) => in.asTerm.select(method).appliedToArgss(Nil).asExpr
          else (in: Expr[In]) => in.asTerm.select(method).appliedToNone.asExpr,
        path = Path.Field(Path.Root, name)
      )
    }.to(ListMap)
      .pipe(ProductInData(_))
      .pipe(DerivationResult.pure)
      .logSuccess(data => s"Resolved input: $data")
  }

  final def extractProductOutData(settings: Settings): DerivationResult[ProductOutData] =
    if (isJavaBean[Out]) {
      // Java Bean case

      val sym = TypeRepr.of[Out].typeSymbol

      val defaultConstructor = DerivationResult.fromOption(
        sym.declarations.find(isDefaultConstructor).map { ctor =>
          ctor.paramSymss match {
            // new Bean[...]
            case typeArgs :: Nil if typeArgs.exists(_.isType) =>
              New(TypeTree.of[Out])
                .select(ctor)
                .appliedToTypes(TypeRepr.of[Out].typeArgs)
                .appliedToArgss(Nil)
                .asExpr
                .asExprOf[Out]
            // new Bean[...]()
            case typeArgs :: Nil :: Nil if typeArgs.exists(_.isType) =>
              New(TypeTree.of[Out])
                .select(ctor)
                .appliedToTypes(TypeRepr.of[Out].typeArgs)
                .appliedToNone
                .asExpr
                .asExprOf[Out]
            // new Bean
            case Nil =>
              New(TypeTree.of[Out]).select(ctor).appliedToArgss(Nil).asExpr.asExprOf[Out]
            // new Bean()
            case Nil :: Nil =>
              New(TypeTree.of[Out]).select(ctor).appliedToNone.asExpr.asExprOf[Out]
            case _ =>
              ??? // should never happen due to isDefaultConstructor filtering
          }
        }
      )(DerivationError.MissingPublicConstructor)

      val setters = sym.declaredMethods
        .filter(s => isJavaSetter(s) || isVar(s))
        .map { setter =>
          val name = setter.name
          name -> ProductOutData.Setter[Any](
            name = name,
            tpe = {
              val MethodType(_, List(tpe), _) = TypeRepr.of[Out].memberType(setter): @unchecked
              tpe.asType.asInstanceOf[Type[Any]]
            },
            set =
              // if (isVar(setter)) (out: Expr[Out], value: Expr[Any]) => ???
              // else
              (out: Expr[Out], value: Expr[Any]) =>
                out.asTerm.select(setter).appliedTo(value.asTerm).asExpr.asExprOf[Unit]
          )
        }
        .to(ListMap)
        .pipe(DerivationResult.pure(_))

      defaultConstructor
        .map2(setters)(ProductOutData.JavaBean(_, _))
        .logSuccess(data => s"Resolved Java Bean output: $data")
    } else if (isCaseObject[Out]) {
      // case object case

      val sym = TypeRepr.of[Out].typeSymbol

      ProductOutData
        .CaseClass(
          if (sym.flags.is(Flags.Case | Flags.Enum | Flags.JavaStatic)) {
            // Scala 3 case object (enum's case without parameters)
            params => Ref(sym).asExpr.asExprOf[Out]
          } else {
            // Scala 2 case object
            params => Ref(sym.companionModule).asExpr.asExprOf[Out]
          },
          List.empty
        )
        .pipe(DerivationResult.pure(_))
        .logSuccess(data => s"Resolved case object output: $data")
    } else {
      // case class case

      (for {
        primaryConstructor <- DerivationResult.pure(TypeRepr.of[Out].typeSymbol.primaryConstructor)
        pair <- resolveTypeArgsForMethodArguments(TypeRepr.of[Out], primaryConstructor)
        (typeByName, typeParams) = pair
        // default value for case class field n (1 indexed) is obtained from Companion.apply$default$n
        defaults = primaryConstructor.paramSymss
          .pipe(if (typeParams.nonEmpty) ps => ps.tail else ps => ps)
          .headOption
          .toList
          .flatten
          .zipWithIndex
          .collect {
            case (param, idx) if param.flags.is(Flags.HasDefault) =>
              val mod = TypeRepr.of[Out].typeSymbol.companionModule
              val sym = mod.declaredMethod("apply$default$" + (idx + 1)).head
              param.name -> Ref(mod).select(sym).asExpr.asInstanceOf[Expr[Any]]
          }
          .toMap
      } yield ProductOutData.CaseClass(
        params =>
          New(TypeTree.of[Out])
            .select(primaryConstructor)
            .pipe(if (typeParams.nonEmpty) tree => tree.appliedToTypes(typeParams) else tree => tree)
            .appliedToArgss(params.map(_.map(_.asTerm)))
            .asExpr
            .asExprOf[Out],
        primaryConstructor.paramSymss.pipe(if (typeParams.nonEmpty) ps => ps.tail else ps => ps).map { params =>
          params
            .map { param =>
              param.name -> ProductOutData.ConstructorParam(
                name = param.name,
                tpe = typeByName(param.name).asType.asInstanceOf[Type[Any]],
                default = defaults.get(param.name)
              )
            }
            .to(ListMap)
        }
      )).logSuccess(data => s"Resolved case class output: $data")
    }

  final def generateProductCode(generatorData: ProductGeneratorData): DerivationResult[Expr[Pipe[In, Out]]] =
    generatorData match {
      case ProductGeneratorData.CaseClass(constructor, results)       => generateCaseClass(constructor, results)
      case ProductGeneratorData.JavaBean(defaultConstructor, results) => generateJavaBean(defaultConstructor, results)
    }

  private def generateCaseClass(
    constructor:          Constructor,
    outputParameterLists: List[List[ProductGeneratorData.OutputValue]]
  ): DerivationResult[Expr[Pipe[In, Out]]] = {
    val paramToIdx: Map[ProductGeneratorData.OutputValue.Result[?], Expr[Int]] = outputParameterLists.flatten
      .collect { case result: ProductGeneratorData.OutputValue.Result[?] => result }
      .zipWithIndex
      .map { case (result, idx) => result -> Literal(IntConstant(idx)).asExpr.asExprOf[Int] }
      .toMap

    def constructorParams(in: Expr[In], ctx: Expr[Context], arr: Expr[Array[Any]]): List[List[Expr[?]]] =
      outputParameterLists.map(
        _.map {
          case ProductGeneratorData.OutputValue.Pure(_, caller) =>
            caller(in, ctx)
          case r @ ProductGeneratorData.OutputValue.Result(tpe, _) =>
            implicit val tpee: Type[Any] = tpe.asInstanceOf[Type[Any]]
            '{ $arr(${ paramToIdx(r) }).asInstanceOf[tpee.Underlying] }
        }
      )

    val arrSize = Literal(IntConstant(paramToIdx.size)).asExpr.asExprOf[Int]
    val initialValue: Expr[Result[Array[Any]]] = pureResult('{ scala.Array.ofDim[scala.Any]($arrSize) })

    @scala.annotation.tailrec
    def generateBody(
      in:          Expr[In],
      ctx:         Expr[Context],
      arrayResult: Expr[Result[Array[Any]]],
      params:      List[(ProductGeneratorData.OutputValue.Result[?], Expr[Int])]
    ): Expr[Result[Out]] =
      params match {
        // all values are taken directly from input and wrapped in Result
        case Nil =>
          pureResult(constructor(constructorParams(in, ctx, null)))

        // last param - after adding the last value to array we extract all values from it into constructor
        case (param, idx) :: Nil =>
          val rightCode = param.caller(in, ctx).asInstanceOf[Expr[Result[Any]]]
          implicit val rightType: Type[Any] = param.tpe.asInstanceOf[Type[Any]]

          val fun: Expr[(Array[Any], Any) => Out] =
            '{ (left: Array[Any], right: rightType.Underlying) =>
              left($idx) = right
              ${ constructor(constructorParams(in, ctx, '{ left })) }
            }

          mergeResults[Array[Any], Any, Out](ctx, arrayResult, rightCode, fun)

        // we combine Array's Result with a param's Result, store param in array and iterate further
        case (param, idx) :: tail =>
          val rightCode = param.caller(in, ctx).asInstanceOf[Expr[Result[Any]]]
          implicit val rightType: Type[Any] = param.tpe.asInstanceOf[Type[Any]]

          val fun: Expr[(Array[Any], Any) => Array[Any]] =
            '{ (left: Array[Any], right: rightType.Underlying) =>
              left($idx) = right
              left
            }

          generateBody(in, ctx, mergeResults[Array[Any], Any, Array[Any]](ctx, arrayResult, rightCode, fun), tail)
      }

    val body: Expr[Pipe[In, Out]] = lift[In, Out]('{ (in: In, ctx: Context) =>
      ${ generateBody('{ in }, '{ ctx }, initialValue, paramToIdx.toList) }
    })

    DerivationResult
      .pure(body)
      .log(s"Case class derivation, constructor params: $outputParameterLists")
      .logSuccess(code => s"Generated code: ${previewCode(code)}")
  }

  private def generateJavaBean(
    defaultConstructor: Expr[Out],
    outputSettersList:  List[(ProductGeneratorData.OutputValue, ProductOutData.Setter[?])]
  ): DerivationResult[Expr[Pipe[In, Out]]] = {
    def pureValues(in: Expr[In], ctx: Expr[Context], result: Expr[Out]): List[Expr[Unit]] =
      outputSettersList.collect { case (ProductGeneratorData.OutputValue.Pure(_, caller), setter) =>
        setter.asInstanceOf[ProductOutData.Setter[Any]].set(result, caller(in, ctx).asInstanceOf[Expr[Unit]])
      }

    val resultValues: List[(ProductGeneratorData.OutputValue.Result[?], ProductOutData.Setter[?])] =
      outputSettersList.collect { case (r: ProductGeneratorData.OutputValue.Result[?], s: ProductOutData.Setter[?]) =>
        r -> s
      }

    def initialValue(in: Expr[In], ctx: Expr[Context]): Expr[Result[Out]] = pureResult(
      '{
        val result: Out = ${ defaultConstructor }
        ${ pureValues(in, ctx, '{ result }).fold[Expr[Unit]]('{ () })((a, b) => '{ ${ a }; ${ b } }) }
        result
      }
    )

    @scala.annotation.tailrec
    def generateBody(
      in:        Expr[In],
      ctx:       Expr[Context],
      outResult: Expr[Result[Out]],
      params:    List[(ProductGeneratorData.OutputValue.Result[?], ProductOutData.Setter[?])]
    ): Expr[Result[Out]] =
      params match {
        // all values are taken directly from input and wrapped in Result
        case Nil =>
          outResult

        // we have Out object on left and value to put into setter on right
        case (param, setter) :: tail =>
          type Right
          val rightCode = param.caller(in, ctx).asInstanceOf[Expr[Result[Right]]]
          implicit val paramTpe: Type[Right] = param.tpe.asInstanceOf[Type[Right]]

          val fun: Expr[(Out, Any) => Out] =
            '{ (left: Out, right: paramTpe.Underlying) =>
              ${ setter.asInstanceOf[ProductOutData.Setter[Right]].set('{ left }, '{ right }) }
              left
            }.asInstanceOf[Expr[(Out, Any) => Out]]

          generateBody(in, ctx, mergeResults(ctx, outResult, rightCode, fun), tail)
      }

    val body: Expr[Pipe[In, Out]] = lift[In, Out](
      '{ (in: In, ctx: Context) =>
        ${ generateBody('{ in }, '{ ctx }, initialValue('{ in }, '{ ctx }), resultValues) }
      }
    )

    DerivationResult
      .pure(body)
      .log(s"Java Beans derivation, setters: $outputSettersList")
      .logSuccess(code => s"Generated code: ${previewCode(code)}")
  }

  private val resolveTypeArgsForMethodArguments = (tpe: TypeRepr, method: Symbol) =>
    tpe.memberType(method) match {
      // monomorphic
      case MethodType(names, types, _) =>
        val typeArgs:           List[TypeRepr]        = Nil
        val typeArgumentByName: Map[String, TypeRepr] = names.zip(types).toMap
        DerivationResult.pure(typeArgumentByName -> typeArgs)
      // polymorphic
      case PolyType(_, _, MethodType(names, types, AppliedType(_, typeRefs))) =>
        // TODO: check if types of constructor match types passed to Out
        val typeArgs: List[TypeRepr] = TypeRepr.of[Out].typeArgs
        val typeArgumentByAlias = typeRefs.zip(typeArgs).toMap
        val typeArgumentByName: Map[String, TypeRepr] = names
          .zip(types)
          .toMap
          .view
          .mapValues { tpe =>
            typeArgumentByAlias.getOrElse(tpe, tpe)
          }
          .toMap
        DerivationResult.pure(typeArgumentByName -> typeArgs)
      // unknown
      case tpe =>
        DerivationResult.fail(
          DerivationError.NotYetImplemented(
            s"Constructor of ${previewType[Out]} has unrecognized/unsupported format of type: ${tpe}"
          )
        )
    }
}
