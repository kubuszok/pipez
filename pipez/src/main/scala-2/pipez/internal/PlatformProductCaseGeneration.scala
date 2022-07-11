package pipez.internal

trait PlatformProductCaseGeneration extends ProductCaseGeneration { self: PlatformDefinitions with Dispatchers =>

  final def isUsableAsProductInput(tpe: Type): Boolean =
    isCaseClass(tpe) || isJavaBean(tpe) // TODO: possibly reconsider
  final def isUsableAsProductOutput(tpe: Type): Boolean =
    (isCaseClass(tpe) || isJavaBean(tpe)) && !tpe.typeSymbol.isAbstract
  final def isCaseClass(tpe: Type): Boolean =
    tpe.typeSymbol.isClass && tpe.typeSymbol.asClass.isCaseClass
  final def isJavaBean(tpe: Type): Boolean =
    tpe.typeSymbol.isClass // TODO


  object ProductTypeConversion extends ProductTypeConversion {
    // TODO: implement abstract members
  }
}
