package pipez.internal

trait PlatformSumCaseGeneration[Pipe[_, _], In, Out] extends SumCaseGeneration[Pipe, In, Out] {
  self: PlatformDefinitions[Pipe, In, Out] & PlatformGenerators[Pipe, In, Out] =>

  final def isADT[A](tpe: Type[A]): Boolean = ???

  final def isJavaEnum[A](tpe: Type[A]): Boolean = ???

  final def areSubtypesEqual[A, B](typeA: Type[A], typeB: Type[B]): Boolean = ???

  final def extractEnumInData: DerivationResult[EnumData[In]] = ???

  final def extractEnumOutData: DerivationResult[EnumData[Out]] = ???

  final def generateEnumCode(generatorData: EnumGeneratorData): DerivationResult[CodeOf[Pipe[In, Out]]] = ???
}
