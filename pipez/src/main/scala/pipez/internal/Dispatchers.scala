package pipez.internal

import pipez.PipeDerivation

import scala.annotation.nowarn

@nowarn("msg=The outer reference in this type test cannot be checked at run time.")
trait Dispatchers extends ProductCaseGeneration { self: Definitions =>

  final case class Configuration[Pipe[_, _], In, Out](
    inType:         Type,
    outType:        Type,
    settings:       Settings,
    pipeDerivation: CodeOf[PipeDerivation[Pipe]]
  )

  trait CodeGenerator[Pipe[_, _], In, Out] {
    def generate: DerivationResult[CodeOf[Pipe[In, Out]]]
  }
  object CodeGenerator {

    final class Failing[Pipe[_, _], In, Out](error: DerivationError) extends CodeGenerator[Pipe, In, Out] {

      override def generate: DerivationResult[CodeOf[Pipe[In, Out]]] = DerivationResult.fail(error)
    }
  }

  trait RendererExtractor {
    def unapply[Pipe[_, _], In, Out](configuration: Configuration[Pipe, In, Out]): Option[CodeGenerator[Pipe, In, Out]]
  }

  val SumTypeConversion: RendererExtractor
  // TODO: other cases
  def resolveConversion[Pipe[_, _], In, Out]: Configuration[Pipe, In, Out] => DerivationResult[
    CodeGenerator[Pipe, In, Out]
  ] = {
    case ProductTypeConversion(generator) => DerivationResult.pure(generator)
    case SumTypeConversion(generator)     => DerivationResult.pure(generator)
    case _                                => DerivationResult.fail(DerivationError.NoSupportedCase)
  }
}
