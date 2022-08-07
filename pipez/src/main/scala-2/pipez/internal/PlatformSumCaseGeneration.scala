package pipez.internal

trait PlatformSumCaseGeneration[Pipe[_, _], In, Out] extends SumCaseGeneration[Pipe, In, Out] {
  self: PlatformDefinitions[Pipe, In, Out] & PlatformGenerators[Pipe, In, Out] =>

  import c.universe.*
  
  final def isADT[A](tpe: Type[A]): Boolean =
    (tpe.typeSymbol.isClass && tpe.typeSymbol.asClass.isSealed)
  final def isJavaEnum[A](tpe: Type[A]): Boolean =
    tpe.typeSymbol.isJavaEnum

  final def extractEnumInData(settings: Settings): DerivationResult[EnumData[In]] =
    DerivationResult.fail(DerivationError.NotYetImplemented("Parsing enum input data"))
  final def extractEnumOutData(settings: Settings): DerivationResult[EnumData[Out]] =
    DerivationResult.fail(DerivationError.NotYetImplemented("Parsing enum output data"))

  final def generateEnumCode(generatorData: EnumGeneratorData): DerivationResult[CodeOf[Pipe[In, Out]]] =
    DerivationResult.fail(DerivationError.NotYetImplemented("Enum codec code"))
}
