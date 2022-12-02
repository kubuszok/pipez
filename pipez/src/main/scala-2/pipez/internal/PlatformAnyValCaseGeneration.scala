package pipez.internal

import scala.language.existentials

private[internal] trait PlatformAnyValCaseGeneration[Pipe[_, _], In, Out] extends AnyValCaseGeneration[Pipe, In, Out] {
  self: PlatformDefinitions[Pipe, In, Out] & PlatformGenerators[Pipe, In, Out] =>

  import c.universe.*

  final def isAnyVal[A: Type]: Boolean =
    isSubtype[A, AnyVal]

  final def isPrimitive[A: Type]: Boolean =
    isSubtype[A, Boolean] || isSubtype[A, Byte] || isSubtype[A, Char] || isSubtype[A, Int] || isSubtype[A, Long] ||
      isSubtype[A, Short] || isSubtype[A, Double] || isSubtype[A, Float] || isSubtype[A, String]

  final def extractAnyValInData(settings: Settings): DerivationResult[AnyValInData[AnyValType]] =
    if (isAnyVal[In])
      DerivationResult.fromOption(
        In.decls.to(List).filterNot(isGarbage).find(field => field.isMethod && field.asMethod.isGetter).map { getter =>
          val name     = getter.name.toString
          val termName = getter.asMethod.name.toTermName
          AnyValInData.InAnyVal(
            tpe = returnTypeOf(In, getter).asInstanceOf[Type[AnyValType]],
            get =
              if (getter.asMethod.paramLists.isEmpty) (in: Expr[In]) => c.Expr[AnyValType](q"$in.$termName")
              else (in: Expr[In]) => c.Expr[AnyValType](q"$in.$termName()"),
            path = Path.Field(Path.Root, name)
          )
        }
      )(DerivationError.InvalidInput("Couldn't found a valid getter to extract value from AnyVal"))
    else
      DerivationResult.pure(AnyValInData.InPrimitive(typeOf[In].asInstanceOf[Type[AnyValType]]))

  final def extractAnyValOutData(settings: Settings): DerivationResult[AnyValOutData[AnyValType]] =
    if (isAnyVal[Out])
      DerivationResult
        .fromOption(
          Out.decls
            .to(List)
            .filterNot(isGarbage)
            .find(m => m.isPublic && m.isConstructor && m.asMethod.paramLists.flatten.size == 1)
        )(DerivationError.MissingPublicConstructor)
        .map { primaryConstructor =>
          val arg = primaryConstructor.asMethod.paramLists.flatten.head
          AnyValOutData.OutAnyVal(
            tpe = arg.typeSignature.asInstanceOf[Type[AnyValType]],
            put = (value: Expr[AnyValType]) => c.Expr[Out](q"new $Out($value)")
          )
        }
    else
      DerivationResult.pure(AnyValOutData.OutPrimitive(typeOf[Out].asInstanceOf[Type[AnyValType]]))

  final def generateAnyValCode(generatorData: AnyValGeneratorData): DerivationResult[Expr[Pipe[In, Out]]] = {
    val in = c.freshName(TermName("in"))
    DerivationResult.pure(
      lift[In, Out](c.Expr(q"""($in: $In, _: $Context) => ${pureResult(generatorData.repack(c.Expr(q"$in")))}"""))
    )
  }
}
