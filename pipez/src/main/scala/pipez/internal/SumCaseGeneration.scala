package pipez.internal

trait SumCaseGeneration { self: Definitions with Dispatchers =>

  trait SumTypeConversion extends RendererExtractor {

    final def unapply[Pipe[_, _], In, Out](
      configuration: Configuration[Pipe, In, Out]
    ): Option[CodeGenerator[Pipe, In, Out]] = None
  }
  val SumTypeConversion: SumTypeConversion
}
