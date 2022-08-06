package pipez.internal

trait SumCaseGeneration[Pipe[_, _], In, Out] { self: Definitions[Pipe, In, Out] & Generators[Pipe, In, Out] =>

  object SumTypeConversion extends CodeGeneratorExtractor {

    final def unapply(settings: Settings): Option[DerivationResult[CodeOf[Pipe[In, Out]]]] = None
  }
}
