package pipez.internal

import pipez.PipeDerivation

import scala.annotation.nowarn

@nowarn("msg=The outer reference in this type test cannot be checked at run time.")
trait Generators extends ProductCaseGeneration with SumCaseGeneration { self: Definitions =>

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

  def resolveConversion[Pipe[_, _], In, Out]: Configuration[Pipe, In, Out] => DerivationResult[
    CodeOf[Pipe[In, Out]]
  ] = {
    case ProductTypeConversion(generator) => generator
    case SumTypeConversion(generator)     => generator
    case Configuration(inType, outType, _, _) =>
      DerivationResult.fail(DerivationError.NotSupportedConversion(inType, outType))
  }

  type ArbitraryContext
  type ArbitraryResult[Out]

  /** Should generate code `pd.unlift(pipe)(in, ctx)` */
  def unlift[Pipe[_, _], In, Out](
    pipeDerivation: CodeOf[PipeDerivation[Pipe]],
    pipe:           CodeOf[Pipe[In, Out]],
    in:             CodeOf[In],
    ctx:            CodeOf[ArbitraryContext]
  ): CodeOf[ArbitraryResult[Out]]

  /** Should generate code `pd.lift { (in, ctx) => ... }` */
  def lift[Pipe[_, _], In, Out](
    pipeDerivation: CodeOf[PipeDerivation[Pipe]],
    call:           CodeOf[(In, ArbitraryContext) => ArbitraryResult[Out]]
  ): CodeOf[Pipe[In, Out]]

  /** Should generate code `pd.updateContext(ctx, path)` */
  def updateContext[Pipe[_, _]](
    pipeDerivation: CodeOf[PipeDerivation[Pipe]],
    context:        CodeOf[ArbitraryContext],
    path:           CodeOf[Path]
  ): CodeOf[ArbitraryContext]

  /** Should generate code `pd.pureResult(a)` */
  def pureResult[Pipe[_, _], A](pipeDerivation: CodeOf[PipeDerivation[Pipe]], a: A): CodeOf[ArbitraryResult[A]]

  /** Should generate code `pd.mergeResults(ra, rb) { (a, b) => ... }` */
  def mergeResults[Pipe[_, _], A, B, C](
    pipeDerivation: CodeOf[PipeDerivation[Pipe]],
    ra:             CodeOf[ArbitraryResult[A]],
    rb:             CodeOf[ArbitraryResult[B]],
    f:              CodeOf[(A, B) => C]
  ): CodeOf[ArbitraryResult[C]]
}
