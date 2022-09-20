package pipez.internal

import pipez.internal.Definitions.{ Context, Result }

import scala.collection.immutable.ListMap
import scala.util.chaining.*
import scala.quoted.{ Type as _, * }

trait PlatformProductCaseGeneration[Pipe[_, _], In, Out] extends ProductCaseGeneration[Pipe, In, Out] {
  self: PlatformDefinitions[Pipe, In, Out] & PlatformGenerators[Pipe, In, Out] =>

  import quotes.*
  import quotes.reflect.*

  final def isTuple[A: Type]: Boolean =
    TypeRepr.of[A].typeSymbol.fullName.startsWith("scala.Tuple")

  final def isCaseClass[A: Type]: Boolean =
    val sym = TypeRepr.of[A].typeSymbol
    sym.isClassDef && sym.flags.is(Flags.Case)

  final def isCaseObject[A: Type]: Boolean =
    TypeRepr.of[A].typeSymbol.flags.is(Flags.Module) && isCaseClass[A]

  final def isJavaBean[A: Type]: Boolean =
    val sym = TypeRepr.of[A].typeSymbol
    sym.isClassDef &&
    sym.declaredMethods.exists(m => m.name.toLowerCase.startsWith("set")) &&
    sym.declarations.exists(m => m.isClassConstructor && m.paramSymss.flatten.isEmpty) // TODO: check for public?

  final def isInstantiable[A: Type]: Boolean =
    val sym = TypeRepr.of[A].typeSymbol
    !sym.flags.is(Flags.Abstract) && sym.primaryConstructor != Symbol.noSymbol // TODO: check for public?

  final def extractProductInData(settings: Settings): DerivationResult[ProductInData] = {
    val sym = TypeRepr.of[In].typeSymbol
    (sym.caseFields ++ sym.declaredMethods.filter { m =>
      val n = m.name.toLowerCase
      (n.startsWith("is") || n.startsWith("get")) && m.paramSymss.flatten.isEmpty
    }).map { method =>
      method.name.toString -> ProductInData.Getter[Any](
        name = method.name.toString,
        tpe = returnType[Any](TypeRepr.of[In].memberType(method)),
        get =
          if (method.paramSymss.isEmpty) (in: Expr[In]) => in.asTerm.select(method).appliedToArgss(Nil).asExpr
          else (in: Expr[In]) => in.asTerm.select(method).appliedToNone.asExpr,
        path = Path.Field(Path.Root, method.name.toString)
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
        sym.declarations.collectFirst {
          case member if member.isClassConstructor && member.paramSymss.flatten.isEmpty =>
            if (member.paramSymss.isEmpty) New(TypeTree.of[Out]).select(member).appliedToArgss(Nil).asExpr.asExprOf[Out]
            else New(TypeTree.of[Out]).select(member).appliedToNone.asExpr.asExprOf[Out]
        }
      )(DerivationError.MissingPublicConstructor)

      val setters = sym.declaredMethods
        .collect {
          case method if method.name.toLowerCase.startsWith("set") && method.paramSymss.flatten.size == 1 =>
            method.name -> ProductOutData.Setter[Any](
              name = method.name.toString,
              tpe = {
                val MethodType(_, List(tpe), _) = TypeRepr.of[Out].memberType(method): @unchecked
                tpe.asType.asInstanceOf[Type[Any]]
              },
              set = (out: Expr[Out], value: Expr[Any]) =>
                out.asTerm.select(method).appliedTo(value.asTerm).asExpr.asExprOf[Unit]
            )
        }
        .to(ListMap)
        .pipe(DerivationResult.pure(_))

      defaultConstructor
        .map2(setters)(ProductOutData.JavaBean(_, _))
        .logSuccess(data => s"Resolved Java Bean output: $data")
    } else if (isCaseObject[Out]) {
      // case object case

      ProductOutData
        .CaseClass(
          params => Ref(TypeRepr.of[Out].typeSymbol.companionModule).asExpr.asExprOf[Out],
          List.empty
        )
        .pipe(DerivationResult.pure(_))
        .logSuccess(data => s"Resolved case object output: $data")
    } else {
      // case class case

      val sym = TypeRepr.of[Out].typeSymbol

      ProductOutData
        .CaseClass(
          params =>
            New(TypeTree.of[Out])
              .select(sym.primaryConstructor)
              .appliedToArgss(params.map(_.map(_.asTerm)))
              .asExpr
              .asExprOf[Out],
          sym.primaryConstructor.paramSymss.map { params =>
            val MethodType(names, types, _) = TypeRepr.of[Out].memberType(sym.primaryConstructor): @unchecked
            val typeByName                  = names.zip(types).toMap
            params
              .map { param =>
                param.name.toString -> ProductOutData.ConstructorParam(
                  name = param.name,
                  tpe = typeByName(param.name).asType.asInstanceOf[Type[Any]]
                )
              }
              .to(ListMap)
          }
        )
        .pipe(DerivationResult.pure(_))
        .logSuccess(data => s"Resolved case class output: $data")
    }

  final def generateProductCode(generatorData: ProductGeneratorData): DerivationResult[Expr[Pipe[In, Out]]] =
    generatorData match {
      case ProductGeneratorData.CaseClass(constructor, results)       => generateCaseClass(constructor, results)
      case ProductGeneratorData.JavaBean(defaultConstructor, results) => generateJavaBean(defaultConstructor, results)
    }

  private def generateCaseClass(
    constructor:          Constructor,
    outputParameterLists: List[List[ProductGeneratorData.OutputValue]]
  ) = {
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
    val initialValue: Expr[Result[Array[Any]]] = pureResult('{ scala.Array[scala.Any]($arrSize) })

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
  ) = {
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
