package pipez.internal

import pipez.internal.Definitions.{ Context, Result }

import scala.annotation.nowarn
import scala.collection.immutable.ListMap
import scala.util.chaining.*

import scala.language.existentials

@nowarn("msg=The outer reference in this type test cannot be checked at run time.")
trait PlatformProductCaseGeneration[Pipe[_, _], In, Out] extends ProductCaseGeneration[Pipe, In, Out] {
  self: PlatformDefinitions[Pipe, In, Out] & PlatformGenerators[Pipe, In, Out] =>

  import c.universe.*

  final def isTuple[A: Type]: Boolean =
    typeOf[A].typeSymbol.fullName.startsWith("scala.Tuple")

  final def isCaseClass[A: Type]: Boolean = {
    val sym = typeOf[A].typeSymbol
    sym.isClass && sym.asClass.isCaseClass
  }
  final def isCaseObject[A: Type]: Boolean = {
    val sym = typeOf[A].typeSymbol
    sym.isModuleClass && sym.asClass.isCaseClass
  }
  final def isJavaBean[A: Type]: Boolean =
    typeOf[A].typeSymbol.isClass &&
      typeOf[A].members.exists(m => m.isPublic && m.isMethod && m.asMethod.isSetter) &&
      typeOf[A].members.exists(m => m.isPublic && m.isConstructor && m.asMethod.paramLists.flatten.isEmpty)
  final def isInstantiable[A: Type]: Boolean =
    !typeOf[A].typeSymbol.isAbstract && typeOf[A].members.exists(m => m.isPublic && m.isConstructor)

  final def extractProductInData(settings: Settings): DerivationResult[ProductInData] =
    In.members // we fetch ALL members, even those that might have been inherited
      .to(List)
      .collect {
        case member
            if member.isMethod && (member.asMethod.isGetter || member.name.toString.toLowerCase.startsWith(
              "is"
            ) || member.name.toString.toLowerCase.startsWith("get")) =>
          member.name.toString -> ProductInData.Getter[Any](
            name = member.name.toString,
            tpe = member.asMethod.returnType.asInstanceOf[Type[Any]],
            get =
              if (member.asMethod.paramLists.isEmpty)
                (in: Expr[In]) => c.Expr[Any](q"$in.${member.asMethod.name.toTermName}")
              else (in: Expr[In]) => c.Expr[Any](q"$in.${member.asMethod.name.toTermName}()"),
            path = Path.Field(Path.Root, member.name.toString)
          )
      }
      .to(ListMap)
      .pipe(ProductInData(_))
      .pipe(DerivationResult.pure)
      .logSuccess(data => s"Resolved input: $data")

  final def extractProductOutData(settings: Settings): DerivationResult[ProductOutData] =
    if (isJavaBean[Out]) {
      // Java Bean case

      val defaultConstructor = DerivationResult.fromOption(
        Out.decls.collectFirst {
          case member if member.isPublic && member.isConstructor && member.asMethod.paramLists.flatten.isEmpty =>
            c.Expr[Out](q"new $Out()")
        }
      )(DerivationError.MissingPublicConstructor)

      val setters = Out.decls
        .to(List)
        .collect {
          case method
              if method.isPublic && method.isMethod && method.name.toString.toLowerCase.startsWith(
                "set"
              ) && method.asMethod.paramLists.flatten.size == 1 =>
            method.name.toString -> ProductOutData.Setter(
              name = method.name.toString,
              tpe = method.asMethod.paramLists.flatten.head.typeSignature.asInstanceOf[Type[Any]],
              set =
                (out: Expr[Out], value: Expr[Any]) => c.Expr[Unit](q"$out.${method.asMethod.name.toTermName}($value)")
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
          params => c.Expr(q"${Out.typeSymbol.asClass.module}"),
          List.empty
        )
        .pipe(DerivationResult.pure(_))
        .logSuccess(data => s"Resolved case object output: $data")
    } else {
      // case class case

      Out.decls
        .to(List)
        .collectFirst {
          case member if member.isPublic && member.isConstructor =>
            ProductOutData.CaseClass(
              params => c.Expr(q"new $Out(...$params)"),
              member.asMethod.paramLists.map { params =>
                params
                  .map { param =>
                    param.name.toString -> ProductOutData.ConstructorParam(
                      name = param.name.toString,
                      tpe = param.typeSignature.asInstanceOf[Type[Any]]
                    )
                  }
                  .to(ListMap)
              }
            )
        }
        .get
        .pipe(DerivationResult.pure(_))
        .logSuccess(data => s"Resolved case class output: $data")
    }

  final def generateProductCode(generatorData: ProductGeneratorData): DerivationResult[Expr[Pipe[In, Out]]] =
    generatorData match {
      case ProductGeneratorData.CaseClass(caller, results)            => generateCaseClass(caller, results)
      case ProductGeneratorData.JavaBean(defaultConstructor, results) => generateJavaBean(defaultConstructor, results)
    }

  private def generateCaseClass(
    constructor:          Constructor,
    outputParameterLists: List[List[ProductGeneratorData.OutputValue]]
  ) = {
    val paramToIdx: Map[ProductGeneratorData.OutputValue.Result[?], Constant] = outputParameterLists.flatten
      .collect { case result: ProductGeneratorData.OutputValue.Result[?] => result }
      .zipWithIndex
      .map { case (result, idx) => result -> Constant(idx) }
      .toMap

    def constructorParams(in: Expr[In], ctx: Expr[Context], arr: Expr[Array[Any]]): List[List[Expr[?]]] =
      outputParameterLists.map(
        _.map {
          case ProductGeneratorData.OutputValue.Pure(_, caller) =>
            caller(in, ctx)
          case r @ ProductGeneratorData.OutputValue.Result(tpe, _) =>
            c.Expr(q"""$arr(${paramToIdx(r)}).asInstanceOf[$tpe]""")
        }
      )

    val arrSize = Constant(paramToIdx.size)
    val initialValue: Expr[Result[Array[Any]]] = pureResult(c.Expr(q"scala.Array[scala.Any]($arrSize)"))

    @scala.annotation.tailrec
    def generateBody(
      in:          Expr[In],
      ctx:         Expr[Context],
      arrayResult: Expr[Result[Array[Any]]],
      params:      List[(ProductGeneratorData.OutputValue.Result[?], Constant)]
    ): Expr[Result[Out]] =
      params match {
        // all values are taken directly from input and wrapped in Result
        case Nil =>
          pureResult(constructor(constructorParams(in, ctx, null)))

        // last param - after adding the last value to array we extract all values from it into constructor
        case (param, idx) :: Nil =>
          val rightCode = param.caller(in, ctx).asInstanceOf[Expr[Result[Any]]]

          val left  = c.freshName(TermName("left"))
          val right = c.freshName(TermName("right"))
          val fun: Expr[(Array[Any], Any) => Out] = c.Expr[(Array[Any], Any) => Out](
            q"""
             ($left : scala.Array[scala.Any], $right : ${param.tpe}) => {
               $left($idx) = $right
               ${constructor(constructorParams(in, ctx, c.Expr[Array[Any]](q"$left")))}
             }
             """
          )

          mergeResults(ctx, arrayResult, rightCode, fun)

        // we combine Array's Result with a param's Result, store param in array and iterate further
        case (param, idx) :: tail =>
          val rightCode = param.caller(in, ctx).asInstanceOf[Expr[Result[Any]]]

          val left  = c.freshName(TermName("left"))
          val right = c.freshName(TermName("right"))
          val fun = c.Expr[(Array[Any], Any) => Array[Any]](
            q"""
            ($left : scala.Array[scala.Any], $right : ${param.tpe}) => {
              $left($idx) = $right
              $left
            }
            """
          )

          generateBody(in, ctx, mergeResults(ctx, arrayResult, rightCode, fun), tail)
      }

    val body: Expr[Pipe[In, Out]] = {
      val in  = c.freshName(TermName("in"))
      val ctx = c.freshName(TermName("ctx"))
      lift[In, Out](
        c.Expr[(In, Context) => Result[Out]](
          q"""
          ($in : $In, $ctx : $Context) =>
            ${generateBody(c.Expr[In](q"$in"), c.Expr[Context](q"$ctx"), initialValue, paramToIdx.toList)}
          """
        )
      )
    }

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
        setter.asInstanceOf[ProductOutData.Setter[Any]].set(result, caller(in, ctx))
      }

    val resultValues: List[(ProductGeneratorData.OutputValue.Result[?], ProductOutData.Setter[?])] =
      outputSettersList.collect { case (r: ProductGeneratorData.OutputValue.Result[?], s: ProductOutData.Setter[?]) =>
        r -> s
      }

    def initialValue(in: Expr[In], ctx: Expr[Context]): Expr[Result[Out]] = {
      val result = c.freshName(TermName("result"))
      pureResult(
        c.Expr[Out](
          q"""
          {
            val $result: $Out = $defaultConstructor
            ..${pureValues(in, ctx, c.Expr[Out](q"$result"))}
            $result
          }
          """
        )
      )
    }

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
          val rightCode = param.caller(in, ctx).asInstanceOf[Expr[Result[Any]]]

          val left  = c.freshName(TermName("left"))
          val right = c.freshName(TermName("right"))
          val fun = c.Expr[(Out, Any) => Out](
            q"""
            ($left : $Out, $right : ${param.tpe}) => {
              ${setter.asInstanceOf[ProductOutData.Setter[Any]].set(c.Expr(q"$left"), c.Expr(q"$right"))}
              $left
            }
            """
          )

          generateBody(in, ctx, mergeResults(ctx, outResult, rightCode, fun), tail)
      }

    val body: Expr[Pipe[In, Out]] = {
      val in  = c.freshName(TermName("in"))
      val ctx = c.freshName(TermName("ctx"))
      lift[In, Out](
        c.Expr[(In, Context) => Result[Out]](
          q"""
          ($in : $In, $ctx : $Context) =>
            ${generateBody(c.Expr[In](q"$in"),
                           c.Expr[Context](q"$ctx"),
                           initialValue(c.Expr[In](q"$in"), c.Expr[Context](q"$ctx")),
                           resultValues
            )}
          """
        )
      )
    }

    DerivationResult
      .pure(body)
      .log(s"Java Beans derivation, setters: $outputSettersList")
      .logSuccess(code => s"Generated code: ${previewCode(code)}")
  }
}
