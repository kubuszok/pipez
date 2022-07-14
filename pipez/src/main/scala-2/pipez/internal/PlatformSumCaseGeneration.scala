package pipez.internal

trait PlatformSumCaseGeneration extends SumCaseGeneration { self: PlatformDefinitions with PlatformGenerators =>

  import c.universe._

  object SumTypeConversion extends SumTypeConversion {
    // TODO: implement abstract members
  }
}
