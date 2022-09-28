package pipez.internal

import pipez.internal.Definitions.{ Context, Result }

import scala.annotation.nowarn
import scala.util.chaining.*
import scala.language.existentials

private[internal] trait PlatformAnyValCaseGeneration[Pipe[_, _], In, Out] extends AnyValCaseGeneration[Pipe, In, Out] {
  self: PlatformDefinitions[Pipe, In, Out] & PlatformGenerators[Pipe, In, Out] =>

  import quotes.*
  import quotes.reflect.*

  final def isAnyVal[A: Type]: Boolean =
    isSubtype[A, AnyVal]

  final def isPrimitive[A: Type]: Boolean =
    isSubtype[A, Boolean] || isSubtype[A, Byte] || isSubtype[A, Char] || isSubtype[A, Int] || isSubtype[A, Long] ||
      isSubtype[A, Short] || isSubtype[A, Double] || isSubtype[A, Float] || isSubtype[A, String]

  final def extractAnyValInData(settings: Settings): DerivationResult[AnyValInData[AnyValType]] =
    if (isAnyVal[In])
      DerivationResult.fromOption(
        // I am relying of fact that the single argument of AnyVal should be first on the list
        TypeRepr.of[In].typeSymbol.declarations.headOption.map { getter =>
          val name = getter.name
          AnyValInData.InAnyVal(
            tpe = returnType[AnyValType](TypeRepr.of[In].memberType(getter)),
            get = (in: Expr[In]) => in.asTerm.select(getter).appliedToArgss(Nil).asExpr.asInstanceOf[Expr[AnyValType]],
            path = Path.Field(Path.Root, name)
          )
        }
      )(DerivationError.InvalidInput("Couldn't found a valid getter to extract value from AnyVal"))
    else
      DerivationResult.pure(AnyValInData.InPrimitive(typeOf[In].asInstanceOf[Type[AnyValType]]))

  final def extractAnyValOutData(settings: Settings): DerivationResult[AnyValOutData[AnyValType]] =
    if (isAnyVal[Out])
      for {
        primaryConstructor <- DerivationResult.fromOption(
          Option(TypeRepr.of[Out].typeSymbol.primaryConstructor).filter(_.isClassConstructor) // handle NoSymbol
        )(DerivationError.MissingPublicConstructor)
        pair <- resolveTypeArgsForMethodArguments(TypeRepr.of[Out], primaryConstructor)
        (typeByName, typeParams) = pair
        arg <- primaryConstructor.paramSymss.pipe(if (typeParams.nonEmpty) ps => ps.tail else ps => ps).flatten match {
          case arg :: Nil => DerivationResult.pure(arg)
          case _          => DerivationResult.fail(DerivationError.MissingPublicConstructor)
        }
      } yield AnyValOutData.OutAnyVal(
        tpe = typeByName(arg.name).asType.asInstanceOf[Type[AnyValType]],
        put = (value: Expr[AnyValType]) =>
          New(TypeTree.of[Out])
            .select(primaryConstructor)
            .pipe(if (typeParams.nonEmpty) tree => tree.appliedToTypes(typeParams) else tree => tree)
            .appliedToArgss(List(List(value.asTerm)))
            .asExpr
            .asExprOf[Out],
      )
    else DerivationResult.pure(AnyValOutData.OutPrimitive(typeOf[Out].asInstanceOf[Type[AnyValType]]))

  final def generateAnyValCode(generatorData: AnyValGeneratorData): DerivationResult[Expr[Pipe[In, Out]]] =
    DerivationResult.pure(
      lift[In, Out]('{ (in: In, _: Context) => ${ pureResult(generatorData.repack('{ in })) } })
    )
}
