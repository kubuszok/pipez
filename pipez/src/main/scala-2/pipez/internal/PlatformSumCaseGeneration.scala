package pipez.internal

trait PlatformSumCaseGeneration[Pipe[_, _], In, Out] extends SumCaseGeneration[Pipe, In, Out] {
  self: PlatformDefinitions[Pipe, In, Out] & PlatformGenerators[Pipe, In, Out] =>

  import c.universe.*

  final def isADT[A](tpe: Type[A]): Boolean =
    tpe.typeSymbol.isClass && tpe.typeSymbol.asClass.isSealed
  final def isJavaEnum[A](tpe: Type[A]): Boolean =
    tpe.typeSymbol.isJavaEnum

  final def extractEnumInData(settings: Settings): DerivationResult[EnumData[In]] =
    DerivationResult.fail(DerivationError.NotYetImplemented("Parsing enum input data"))

  final def extractEnumOutData(settings: Settings): DerivationResult[EnumData[Out]] =
    DerivationResult.fail(DerivationError.NotYetImplemented("Parsing enum output data"))

  private def extractEnumData[A](tpe: Type[A]): DerivationResult[EnumData[A]] =
    if (isADT(tpe)) {
      def extractSubclasses(t: TypeSymbol): List[TypeSymbol] =
        if (t.asClass.isSealed) t.asClass.knownDirectSubclasses.toList.map(_.asType).flatMap(extractSubclasses)
        else List(t)
      DerivationResult.unsafe[EnumData[A]](
        EnumData.SumType(
          extractSubclasses(tpe.typeSymbol.asType).map { subtypeType =>
            EnumData.SumType.Case(subtypeType.name.toString,
                                  subtypeType.toType,
                                  isCaseObject = subtypeType.asClass.isModule
            )
          }
        )
      )(_ => DerivationError.InvalidConfiguration(s"$tpe seem like an ADT but cannot extract its subtypes"))
    } else DerivationResult.fail(DerivationError.NotYetImplemented("Java Enum parsing"))

  final def generateEnumCode(generatorData: EnumGeneratorData): DerivationResult[CodeOf[Pipe[In, Out]]] =
    DerivationResult.fail(DerivationError.NotYetImplemented("Enum codec code"))
}
