package pipez.internal

import pipez.PipeDerivation

import scala.annotation.nowarn

@nowarn("msg=The outer reference in this type test cannot be checked at run time.")
trait Dispatchers extends ProductCaseGeneration with SumCaseGeneration { self: Definitions =>

  final case class Configuration[Pipe[_, _], In, Out](
    inType:         Type[In],
    outType:        Type[Out],
    settings:       Settings,
    pipeDerivation: CodeOf[PipeDerivation[Pipe]]
  )

  trait CodeGeneratorExtractor {

    def unapply[Pipe[_, _], In, Out](
      configuration: Configuration[Pipe, In, Out]
    ): Option[DerivationResult[CodeOf[Pipe[In, Out]]]]
  }

  // TODO: other cases
  def resolveConversion[Pipe[_, _], In, Out]: Configuration[Pipe, In, Out] => DerivationResult[
    CodeOf[Pipe[In, Out]]
  ] = {
    case ProductTypeConversion(generator) => generator
    case SumTypeConversion(generator)     => generator
    case Configuration(inType, outType, _, _) =>
      DerivationResult.fail(
        DerivationError.NotSupportedCase(s"Pipe from $inType to $outType was not recognized as any of supported cases")
      )
  }
}
