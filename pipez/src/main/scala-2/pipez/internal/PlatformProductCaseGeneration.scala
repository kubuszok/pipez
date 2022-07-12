package pipez.internal

trait PlatformProductCaseGeneration extends ProductCaseGeneration { self: PlatformDefinitions with Dispatchers =>

  final def isUsableAsProductInput[In](tpe: Type[In]): Boolean =
    isCaseClass(tpe) || isJavaBean(tpe) // TODO: possibly reconsider
  final def isUsableAsProductOutput[Out](tpe: Type[Out]): Boolean =
    (isCaseClass(tpe) || isJavaBean(tpe)) && !tpe.tpe.typeSymbol.isAbstract
  final def isCaseClass[A](tpe: Type[A]): Boolean =
    tpe.tpe.typeSymbol.isClass && tpe.tpe.typeSymbol.asClass.isCaseClass
  final def isJavaBean[A](tpe: Type[A]): Boolean =
    tpe.tpe.typeSymbol.isClass // TODO: check getters/setters

  object ProductTypeConversion extends ProductTypeConversion {
    // TODO: implement abstract members

    override def inputData[In](inType: Type[In], settings: Settings): DerivationResult[InData] = {
      // TODO: create Map[GetterName, (Identifier => CodeOf[A], Type[A])]

      println(s"Info about input ${inType.tpe}")
      for {
        member <- inType.tpe.members.to(List)
        if member.isMethod && member.asMethod.isGetter

      } println(s"container member ${member.fullName} ${member.info}")

      DerivationResult.fail(DerivationError.InvalidConfiguration("work in progress"))
    }

    override def outputData[Out](outType: Type[Out], settings: Settings): DerivationResult[OutData] = {
      // TODO: create a List[ListMap[ArgumentName, Type[A]]]

      println(s"Info about output ${outType.tpe}")
      for {
        member <- outType.tpe.decls.to(List)
        // if member.isMethod && member.asMethod.isSetter // TODO: for JavaBeans
        if member.isConstructor
        _ = println(s"container member ${member.fullName} ${member.info}")
        c <- member.asMethod.paramLists
      } println(s"param list: ${c.mkString(", ")}")

      DerivationResult.fail(DerivationError.InvalidConfiguration("work in progress"))
    }
  }
}
