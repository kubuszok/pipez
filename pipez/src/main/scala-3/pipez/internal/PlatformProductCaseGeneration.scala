package pipez.internal

import pipez.internal.Definitions.{ Context, Result }
import pipez.internal.ProductCaseGeneration.inputNameMatchesOutputName

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
    isPublic(sym) && (isScala2Enum || isScala3Enum)

  final def isJavaBean[A: Type]: Boolean =
    val sym = TypeRepr.of[A].typeSymbol
    val mem = sym.declarations
    sym.isClassDef && !sym.flags.is(Flags.Abstract) && mem.exists(isDefaultConstructor) && mem.exists(isJavaSetterOrVar)

  private val nonPrivateFlags = Flags.Private | Flags.PrivateLocal | Flags.Protected
  private def isPublic(sym: Symbol): Boolean =
    (sym.flags & nonPrivateFlags).is(Flags.EmptyFlags)

  private def isDefaultConstructor(ctor: Symbol): Boolean =
    isPublic(ctor) && ctor.isClassConstructor && ctor.paramSymss.filterNot(_.exists(_.isType)).flatten.isEmpty

  private def isJavaGetter(getter: Symbol): Boolean =
    getter.isDefDef &&
      isPublic(getter) &&
      getter.paramSymss.flatten.isEmpty &&
      ProductCaseGeneration.isGetterName(getter.name)

  private def isJavaSetter(setter: Symbol): Boolean =
    isPublic(setter) &&
      setter.isDefDef &&
      setter.paramSymss.flatten.size == 1 &&
      ProductCaseGeneration.isSetterName(setter.name)

  private def isVar(setter: Symbol): Boolean =
    isPublic(setter) && setter.isValDef && setter.flags.is(Flags.Mutable)

  private def isJavaSetterOrVar(setter: Symbol): Boolean =
    isJavaSetter(setter) || isVar(setter)

  final private case class Getter[Extracted, ExtractedField](
    tpe: Type[ExtractedField],
    get: Expr[Extracted] => Expr[ExtractedField]
  )
  private def extractGetters[Extracted: Type]: ListMap[String, Getter[Extracted, Any]] = {
    val sym = TypeRepr.of[Extracted].typeSymbol
    // case class fields appear once in sym.caseFields as vals and once in sym.declaredMethods as methods
    // additionally sometimes they appear twice! once as "val name" and once as "method name " (notice space at the end
    // of name). This breaks matching by order (tuples) but has to be fixed in a way that doesn't filter out fields
    // for normal cases.
    val caseFields = sym.caseFields.zipWithIndex
      .groupBy(_._1.name.trim)
      .view
      .map {
        case (_, Seq(fieldIdx, _)) if fieldIdx._1.isDefDef => fieldIdx
        case (_, Seq(_, fieldIdx)) if fieldIdx._1.isDefDef => fieldIdx
        case (_, fieldIdxs)                                => fieldIdxs.head
      }
      .toList
      .sortBy(_._2)
      .map(_._1)
      .toList
    val caseFieldNames               = caseFields.map(_.name.trim).toSet
    def isCaseFieldName(sym: Symbol) = caseFieldNames(sym.name)
    val javaGetters = sym.declaredMethods.filterNot(isGarbage).filterNot(isCaseFieldName).filter(isJavaGetter)
    (caseFields ++ javaGetters)
      .map { method =>
        val name = method.name
        name -> Getter[Extracted, Any](
          tpe = returnType[Any](TypeRepr.of[In].memberType(method)),
          get =
            // macros distinct obj.method and obj.method()
            if (method.paramSymss.isEmpty) (in: Expr[Extracted]) => in.asTerm.select(method).appliedToArgss(Nil).asExpr
            else (in: Expr[Extracted]) => in.asTerm.select(method).appliedToNone.asExpr
        )
      }
      .to(ListMap)
  }

  final def extractProductInData(settings: Settings): DerivationResult[ProductInData] =
    extractGetters[In]
      .map { case (name, Getter(tpe, get)) =>
        name -> ProductInData.Getter(name, tpe, get, Path.Field(Path.Root, name))
      }
      .pipe(ProductInData(_))
      .pipe(DerivationResult.pure)
      .logSuccess(data => s"Resolved input: $data")

  private def providedFallbackValueByFields(settings: Settings): Map[String, Vector[FieldFallback[?]]] =
    settings.providedFallbackValues
      .collect { case ConfigEntry.AddFallbackValue(fallbackType, fallbackValue) =>
        extractGetters(fallbackType).map { case (name, Getter(fallbackFieldType, extractFallbackValue)) =>
          name -> FieldFallback.Value(fallbackFieldType, extractFallbackValue(fallbackValue))
        }.toVector
      }
      .flatten
      .groupMapReduce(_._1)(p => Vector(p._2))(_ ++ _)

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

      val fallbackValues = providedFallbackValueByFields(settings) // Java Beans don't have fallbacks to defaults

      val setters = sym.declaredMethods
        .filterNot(isGarbage)
        .filter(s => isJavaSetter(s) || isVar(s))
        .map { setter =>
          val name = setter.name
          name -> ProductOutData.Setter[Any](
            name = name,
            tpe = {
              val MethodType(_, List(tpe), _) = TypeRepr.of[Out].memberType(setter): @unchecked
              tpe.asType.asInstanceOf[Type[Any]]
            },
            set = (out: Expr[Out], value: Expr[Any]) =>
              out.asTerm.select(setter).appliedTo(value.asTerm).asExpr.asExprOf[Unit],
            fallback = fallbackValues.view
              .collect {
                case (fallbackName, fallback)
                    if inputNameMatchesOutputName(fallbackName, name, settings.isFieldCaseInsensitive) =>
                  fallback
              }
              .flatten
              .toVector
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
        primaryConstructor <- DerivationResult.fromOption(
          Option(TypeRepr.of[Out].typeSymbol.primaryConstructor).filter(s => !s.isNoSymbol).filter(isPublic)
        )(DerivationError.MissingPublicConstructor)
        pair <- resolveTypeArgsForMethodArguments(TypeRepr.of[Out], primaryConstructor)
        (typeByName, typeParams) = pair
        providedFallbackValues = providedFallbackValueByFields(settings)
          .asInstanceOf[Map[String, Vector[FieldFallback[Any]]]]
        // default value for case class field n (1 indexed) is obtained from Companion.apply$default$n
        defaultFallbackValues = primaryConstructor.paramSymss
          .pipe(if (typeParams.nonEmpty) ps => ps.tail else ps => ps)
          .headOption
          .toList
          .flatten
          .zipWithIndex
          .collect {
            case (param, idx) if param.flags.is(Flags.HasDefault) =>
              val mod = TypeRepr.of[Out].typeSymbol.companionModule
              val sym = mod.declaredMethod("apply$default$" + (idx + 1)).head
              param.name -> FieldFallback.Default(Ref(mod).select(sym).asExpr.asInstanceOf[Expr[Any]])
          }
          .toMap
        fallbackValues = (providedFallbackValues.keySet ++ defaultFallbackValues.keySet).map { fieldName =>
          // we want defaults to have lower priority than provided values
          fieldName -> (providedFallbackValues
            .getOrElse(fieldName, Vector.empty) ++ defaultFallbackValues.get(fieldName).toVector)
        }
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
              val name = param.name
              param.name -> ProductOutData.ConstructorParam(
                name = name,
                tpe = typeByName(name).asType.asInstanceOf[Type[Any]],
                fallback = fallbackValues.view
                  .collect {
                    case (fallbackName, fallback)
                        if inputNameMatchesOutputName(fallbackName, name, settings.isFieldCaseInsensitive) =>
                      fallback
                  }
                  .flatten
                  .toVector
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
}
