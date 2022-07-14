package pipez.internal

import pipez.PipeDerivation

import scala.annotation.nowarn

@nowarn("msg=The outer reference in this type test cannot be checked at run time.")
trait Generators[Pipe[_, _], In, Out]
    extends ProductCaseGeneration[Pipe, In, Out]
    with SumCaseGeneration[Pipe, In, Out] { self: Definitions[Pipe, In, Out] =>

  final case class Configuration(
    inType:         Type[In],
    outType:        Type[Out],
    settings:       Settings,
    pipeDerivation: CodeOf[PipeDerivation[Pipe]]
  )

  trait CodeGeneratorExtractor {

    def unapply(configuration: Configuration): Option[DerivationResult[CodeOf[Pipe[In, Out]]]]
  }

  val resolveConversion: Configuration => DerivationResult[CodeOf[Pipe[In, Out]]] = {
    case ProductTypeConversion(generator) => generator
    case SumTypeConversion(generator)     => generator
    case Configuration(inType, outType, _, _) =>
      DerivationResult.fail(DerivationError.NotYetSupported) // TODO: better error message
  }

  /** Can be used instead of pd.Context to avoid path-dependent types */
  type ArbitraryContext

  /** Can be used instead of pd.Context to avoid path-dependent types */
  type ArbitraryResult[Out]

  /** Should generate code `pd.unlift(pipe)(in, ctx)` */
  def unlift[I, O](
    pipeDerivation: CodeOf[PipeDerivation[Pipe]],
    pipe:           CodeOf[Pipe[I, O]],
    in:             CodeOf[I],
    ctx:            CodeOf[ArbitraryContext]
  ): CodeOf[ArbitraryResult[O]]

  /** Should generate code `pd.lift { (in, ctx) => ... }` */
  def lift[I, O](
    pipeDerivation: CodeOf[PipeDerivation[Pipe]],
    call:           CodeOf[(I, ArbitraryContext) => ArbitraryResult[O]]
  ): CodeOf[Pipe[I, O]]

  /** Should generate code `pd.updateContext(ctx, path)` */
  def updateContext(
    pipeDerivation: CodeOf[PipeDerivation[Pipe]],
    context:        CodeOf[ArbitraryContext],
    path:           CodeOf[Path]
  ): CodeOf[ArbitraryContext]

  /** Should generate code `pd.pureResult(a)` */
  def pureResult[A](pipeDerivation: CodeOf[PipeDerivation[Pipe]], a: A): CodeOf[ArbitraryResult[A]]

  /** Should generate code `pd.mergeResults(ra, rb) { (a, b) => ... }` */
  def mergeResults[A, B, C](
    pipeDerivation: CodeOf[PipeDerivation[Pipe]],
    ra:             CodeOf[ArbitraryResult[A]],
    rb:             CodeOf[ArbitraryResult[B]],
    f:              CodeOf[(A, B) => C]
  ): CodeOf[ArbitraryResult[C]]
}
