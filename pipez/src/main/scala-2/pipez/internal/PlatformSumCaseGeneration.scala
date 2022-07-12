package pipez.internal

trait PlatformSumCaseGeneration extends SumCaseGeneration { self: PlatformDefinitions with Dispatchers =>

  import c.universe._

  object SumTypeConversion extends SumTypeConversion {
    // TODO: implement abstract members
  }
}
