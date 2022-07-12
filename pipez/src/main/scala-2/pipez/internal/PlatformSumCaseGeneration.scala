package pipez.internal

trait PlatformSumCaseGeneration extends SumCaseGeneration { self: PlatformDefinitions with Generators =>

  import c.universe._

  object SumTypeConversion extends SumTypeConversion {
    // TODO: implement abstract members
  }
}
