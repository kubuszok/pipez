package pipez.internal

trait PlatformProductCaseGeneration[Pipe[_, _], In, Out] extends ProductCaseGeneration[Pipe, In, Out] {
  self: PlatformDefinitions[Pipe, In, Out] & PlatformGenerators[Pipe, In, Out] =>

  final def isCaseClass[A](tpe: Type[A]): Boolean = ???

  final def isCaseObject[A](tpe: Type[A]): Boolean = ???

  final def isJavaBean[A](tpe: Type[A]): Boolean = ???

  final def isInstantiable[A](tpe: Type[A]): Boolean = ???

  final def extractProductInData(settings: Settings): DerivationResult[ProductInData] = ???

  final def extractProductOutData(settings: Settings): DerivationResult[ProductOutData] = ???

  final def generateProductCode(generatorData: ProductGeneratorData): DerivationResult[CodeOf[Pipe[In, Out]]] = ???
}
