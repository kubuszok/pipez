package pipez.internal

trait PlatformSumCaseGeneration[Pipe[_, _], In, Out] extends SumCaseGeneration[Pipe, In, Out] { self: PlatformDefinitions[Pipe, In, Out] with PlatformGenerators[Pipe, In, Out] =>

  import c.universe._

  object SumTypeConversion extends SumTypeConversion {
    // TODO: implement abstract members
  }
}
