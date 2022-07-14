package pipez.internal

import pipez.PipeDerivation

trait PlatformGenerators extends Generators { self: PlatformDefinitions =>

  final def unlift[Pipe[_, _], In, Out](
    pipeDerivation: CodeOf[PipeDerivation[Pipe]],
    pipe:           CodeOf[Pipe[In, Out]],
    in:             CodeOf[In],
    ctx:            CodeOf[ArbitraryContext]
  ): CodeOf[ArbitraryResult[Out]] = ???

  final def lift[Pipe[_, _], In, Out](
    pipeDerivation: CodeOf[PipeDerivation[Pipe]],
    call:           CodeOf[(In, ArbitraryContext) => ArbitraryResult[Out]]
  ): CodeOf[Pipe[In, Out]] = ???

  final def updateContext[Pipe[_, _]](
    pipeDerivation: CodeOf[PipeDerivation[Pipe]],
    context:        CodeOf[ArbitraryContext],
    path:           CodeOf[Path]
  ): CodeOf[ArbitraryContext] = ???

  final def pureResult[Pipe[_, _], A](pipeDerivation: CodeOf[PipeDerivation[Pipe]], a: A): CodeOf[ArbitraryResult[A]] =
    ???

  /** Should generate code `pd.mergeResults(ra, rb) { (a, b) => ... }` */
  final def mergeResults[Pipe[_, _], A, B, C](
    pipeDerivation: CodeOf[PipeDerivation[Pipe]],
    ra:             CodeOf[ArbitraryResult[A]],
    rb:             CodeOf[ArbitraryResult[B]],
    f:              CodeOf[(A, B) => C]
  ): CodeOf[ArbitraryResult[C]] = ???
}
