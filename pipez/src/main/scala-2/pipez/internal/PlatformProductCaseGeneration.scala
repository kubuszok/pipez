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
    val mem = typeOf[A].members
    sym.isClass && sym.asClass.isCaseClass && !sym.isAbstract && mem.exists(m => m.isConstructor && m.isPublic)
  }
  final def isCaseObject[A: Type]: Boolean = {
    val sym = typeOf[A].typeSymbol
    sym.isModuleClass && sym.asClass.isCaseClass && sym.isPublic
  }
  final def isJavaBean[A: Type]: Boolean = {
    val sym = typeOf[A].typeSymbol
    val mem = typeOf[A].members
    sym.isClass && !sym.isAbstract && mem.exists(isDefaultConstructor) && mem.exists(isJavaSetter)
  }

  private def isDefaultConstructor(ctor: Symbol): Boolean =
    ctor.isConstructor && ctor.asMethod.paramLists.flatten.isEmpty && ctor.isPublic

  private def isCaseClassField(field: Symbol): Boolean =
    field.isMethod && field.asMethod.isGetter

  private def isJavaGetter(getter: Symbol): Boolean =
    getter.isMethod &&
      ProductCaseGeneration.isGetterName(getter.asMethod.name.toString) &&
      getter.asMethod.paramLists.flatten.isEmpty &&
      getter.isPublic

  private def isJavaSetter(setter: Symbol): Boolean =
    setter.isMethod &&
      ProductCaseGeneration.isSetterName(setter.asMethod.name.toString) &&
      setter.asMethod.paramLists.flatten.size == 1 &&
      setter.isPublic

  final def extractProductInData(settings: Settings): DerivationResult[ProductInData] =
    In.members
      .to(List)
      .filterNot(isGarbage)
      .filter(m => isCaseClassField(m) || isJavaGetter(m))
      .map { getter =>
        getter.name.toString -> ProductInData.Getter[Any](
          name = getter.name.toString,
          tpe = returnTypeOf(In, getter).asInstanceOf[Type[Any]],
          get =
            if (getter.asMethod.paramLists.isEmpty)
              (in: Expr[In]) => c.Expr[Any](q"$in.${getter.asMethod.name.toTermName}")
            else (in: Expr[In]) => c.Expr[Any](q"$in.${getter.asMethod.name.toTermName}()"),
          path = Path.Field(Path.Root, getter.name.toString)
        )
      }
      .to(ListMap)
      .pipe(ProductInData(_))
      .pipe(DerivationResult.pure(_))
      .logSuccess(data => s"Resolved input: $data")

  final def extractProductOutData(settings: Settings): DerivationResult[ProductOutData] =
    if (isJavaBean[Out]) {
      // Java Bean case

      val defaultConstructor = DerivationResult.fromOption(
        Out.decls.filterNot(isGarbage).find(isDefaultConstructor).map(_ => c.Expr[Out](q"new $Out()"))
      )(DerivationError.MissingPublicConstructor)

      val setters = Out.decls
        .to(List)
        .filterNot(isGarbage)
        .filter(isJavaSetter)
        .map { setter =>
          setter.name.toString -> ProductOutData.Setter(
            name = setter.name.toString,
            tpe = paramListsOf(Out, setter).flatten.head.typeSignature.asInstanceOf[Type[Any]],
            set = (out: Expr[Out], value: Expr[Any]) => c.Expr[Unit](q"$out.${setter.asMethod.name.toTermName}($value)")
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
          caller = params => c.Expr(q"${Out.typeSymbol.asClass.module}"),
          params = List.empty
        )
        .pipe(DerivationResult.pure(_))
        .logSuccess(data => s"Resolved case object output: $data")
    } else {
      // case class case

      (for {
        primaryConstructor <- DerivationResult.fromOption(
          Out.decls.to(List).filterNot(isGarbage).find(m => m.isPublic && m.isConstructor)
        )(DerivationError.MissingPublicConstructor)
      } yield ProductOutData.CaseClass(
        caller = params => c.Expr(q"new $Out(...$params)"),
        params = paramListsOf(Out, primaryConstructor).map { params =>
          params
            .map { param =>
              param.name.toString -> ProductOutData.ConstructorParam(
                name = param.name.toString,
                tpe = param.typeSignature.asInstanceOf[Type[Any]]
              )
            }
            .to(ListMap)
        }
      )).logSuccess(data => s"Resolved case class output: $data")
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
    val initialValue: Expr[Result[Array[Any]]] = pureResult(c.Expr(q"scala.Array.ofDim[scala.Any]($arrSize)"))

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

  private val garbage = Set(
    // product methods
    "productElementName",
    "productIterator",
    "canEqual",
    "productElement",
    "productArity",
    "productPrefix",
    "productElementNames",
    // object methods
    "synchronized",
    "wait",
    "equals",
    "hashCode",
    "getClass",
    "asInstanceOf",
    "isInstanceOf"
  )
  private val isGarbage: Symbol => Boolean = s => garbage(s.name.toString)

  /** Applies type arguments obtained from tpe to the type parameters in method's parameters' types */
  private val paramListsOf = (tpe: c.Type, method: c.Symbol) => method.asMethod.typeSignatureIn(tpe).paramLists

  /** Applies type arguments obtained from tpe to the type parameters in method's return type */
  private val returnTypeOf = (tpe: c.Type, method: c.Symbol) => method.typeSignatureIn(tpe).finalResultType
}
