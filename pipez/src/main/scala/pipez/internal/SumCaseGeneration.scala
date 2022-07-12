package pipez.internal

trait SumCaseGeneration { self: Definitions with Generators =>

  trait SumTypeConversion extends CodeGeneratorExtractor {

    final def unapply[Pipe[_, _], In, Out](
      configuration: Configuration[Pipe, In, Out]
    ): Option[DerivationResult[CodeOf[Pipe[In, Out]]]] = None
  }
  val SumTypeConversion: SumTypeConversion
}
