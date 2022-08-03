package pipez.internal

import pipez.PipeDerivation

import scala.annotation.nowarn
import scala.collection.immutable.ListMap
import scala.util.chaining._

import scala.language.existentials

@nowarn("msg=The outer reference in this type test cannot be checked at run time.")
trait PlatformProductCaseGeneration[Pipe[_, _], In, Out] extends ProductCaseGeneration[Pipe, In, Out] {
  self: PlatformDefinitions[Pipe, In, Out] with PlatformGenerators[Pipe, In, Out] =>

  import c.universe._

  final def isSubtype[A, B](lower: Type[A], higher: Type[B]): Boolean =
    lower <:< higher

  final def isCaseClass[A](tpe: Type[A]): Boolean =
    tpe.typeSymbol.isClass &&
      tpe.typeSymbol.asClass.isCaseClass
  final def isJavaBean[A](tpe: Type[A]): Boolean =
    tpe.typeSymbol.isClass &&
      tpe.members.exists(m => m.isPublic && m.isMethod && m.asMethod.isSetter) &&
      tpe.members.exists(m => m.isPublic && m.isConstructor && m.asMethod.paramLists.flatten.isEmpty)
  final def isInstantiable[A](tpe: Type[A]): Boolean =
    !tpe.typeSymbol.isAbstract && tpe.members.exists(m => m.isPublic && m.isConstructor)

  object ProductTypeConversion extends ProductTypeConversion {
    // TODO: implement abstract members

    override def extractInData(settings: Settings): DerivationResult[ProductInData] =
      inType.members // we fetch ALL members, even those that might have been inherited
        .to(List)
        .collect {
          case member if member.isMethod && member.asMethod.isGetter =>
            member.name.toString -> ProductInData.Getter[Any](
              name = member.name.toString, // TODO
              tpe = member.asMethod.returnType,
              get = (arg: Argument[In]) => c.Expr[Any](q"$arg.${member.asMethod.name.toTermName}")
            )
        }
        .to(ListMap)
        .pipe(ProductInData(_))
        .pipe(DerivationResult.pure)
        .logSuccess(data => s"Resolved inputs: $data")

    override def extractOutData(settings: Settings): DerivationResult[ProductOutData] =
      if (isJavaBean(outType)) {
        // Java Bean case

        val defaultConstructor = outType.decls.collectFirst {
          case member if member.isPublic && member.isConstructor && member.asMethod.paramLists.flatten.isEmpty =>
            c.Expr[Out](q"new ${outType}()")
        } match {
          case Some(value) => DerivationResult.pure(value)
          case None        => DerivationResult.fail(DerivationError.MissingPublicConstructor)
        }

        val setters = outType.decls
          .to(List)
          .collect {
            case member if member.isPublic && member.isMethod && member.asMethod.isSetter =>
              member.asMethod.paramLists.flatten.map { param =>
                param.name.toString -> ProductOutData.Setter(
                  name = param.name.toString,
                  tpe = param.typeSignature,
                  set = (_: Argument[In], _: CodeOf[Any]) => c.Expr[Unit](q"()")
                )
              }
          }
          .flatten
          .to(ListMap)
          .pipe(DerivationResult.pure(_))

        defaultConstructor.map2(setters)(ProductOutData.JavaBean(_, _)).logSuccess(data => s"Resolved Java Bean output: $data")
      } else {
        // case class case

        outType.decls
          .to(List)
          .collectFirst {
            case member if member.isPublic && member.isConstructor =>
              ProductOutData.CaseClass(
                params => c.Expr(q"new $outType(...$params)"),
                member.asMethod.paramLists.map { params =>
                  params
                    .map { param =>
                      param.name.toString -> ProductOutData.ConstructorParam(
                        name = param.name.toString,
                        tpe = param.typeSignature
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

    override def generateCode(generatorData: ProductGeneratorData): DerivationResult[CodeOf[Pipe[In, Out]]] =
      generatorData match {
        case ProductGeneratorData.CaseClass(caller, pipes) => generateCaseClass(caller, pipes)
        case ProductGeneratorData.JavaBean()               => generateJavaBean()
      }

    private def generateCaseClass(
      constructor:          Constructor,
      outputParameterLists: List[List[ProductGeneratorData.ConstructorParam]]
    ) = {
      val in:  Argument[In]               = c.freshName(TermName("in"))
      val ctx: Argument[ArbitraryContext] = c.freshName(TermName("ctx"))

      val paramToIdx: Map[ProductGeneratorData.ConstructorParam.Result[_], Constant] = outputParameterLists.flatten
        .collect { case result: ProductGeneratorData.ConstructorParam.Result[_] => result }
        .zipWithIndex
        .map { case (result, idx) => result -> Constant(idx) }
        .toMap

      def constructorParams(arr: TermName): List[List[CodeOf[_]]] = outputParameterLists.map(
        _.map {
          case ProductGeneratorData.ConstructorParam.Pure(tpe, caller) =>
            caller(in, ctx)
          case r @ ProductGeneratorData.ConstructorParam.Result(tpe, caller) =>
            c.Expr(q"""$arr(${paramToIdx(r)}).asInstanceOf[$tpe]""")
        }
      )

      @scala.annotation.tailrec
      def generateBody(
        arr:    CodeOf[ArbitraryResult[Array[Any]]],
        params: List[(ProductGeneratorData.ConstructorParam.Result[_], Constant)]
      ): CodeOf[ArbitraryResult[Out]] =
        params match {
          // all values are taken directly from input and wrapped in Result
          case Nil =>
            pureResult(constructor(constructorParams(TermName("notused"))))

          // last param - after adding the last value to array we extract all values from it into constructor
          case (param, idx) :: Nil =>
            val rightCode = param.caller(in, ctx)

            val left  = c.freshName(TermName("left"))
            val right = c.freshName(TermName("right"))
            val fun = c.Expr[(Array[Any], Any) => Out](
              q"""
                (${Ident(left)} : scala.Array[scala.Any], ${Ident(right)} : ${param.tpe}) => {
                  $left($idx) = $right
                  ${constructor(constructorParams(left))}
                }
               """
            )

            mergeResults(arr, rightCode, fun)

          // we combine Array's Result with a param's Result, store param in array and iterate further
          case (param, idx) :: tail =>
            val rightCode = param.caller(in, ctx)

            val left  = c.freshName(TermName("left"))
            val right = c.freshName(TermName("right"))
            val fun = c.Expr[(Array[Any], Any) => Array[Any]](
              q"""
                (${Ident(left)} : scala.Array[scala.Any], ${Ident(right)} : ${param.tpe}) => {
                  $left($idx) = $right
                  $left
                }
               """
            )

            generateBody(mergeResults(arr, rightCode, fun), tail)
        }

      val arrSize = Constant(paramToIdx.size)
      val body: c.universe.Expr[ArbitraryResult[Out]] =
        generateBody(pureResult(c.Expr(q"scala.Array[scala.Any]($arrSize)")), paramToIdx.toList)

      DerivationResult
        .pure(
          lift[In, Out](
            c.Expr[(In, ArbitraryContext) => ArbitraryResult[Out]](q"""
            (${Ident(in)}: $inType, ${Ident(ctx)}: ${pipeDerivation}.Context) => $body
           """)
          )
        )
        .log(s"Case class derivation, constructor params: $outputParameterLists")
        .logSuccess(code => s"Generated code: ${previewCode(code)}")
    }

    private def generateJavaBean() = {
      val in:  Argument[In]               = c.freshName(TermName("in"))
      val ctx: Argument[ArbitraryContext] = c.freshName(TermName("ctx"))
      DerivationResult
        .pure(
          lift[In, Out](
            c.Expr[(In, ArbitraryContext) => ArbitraryResult[Out]](q"""
                (${Ident(in)}: $inType, ${Ident(ctx)}: ${pipeDerivation}.Context) => ???
               """)
          )
        )
        .log(s"Java Bean output derivation")
        .logSuccess(code => s"Generated code: ${previewCode(code)}")
    }
  }
}
