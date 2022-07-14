package pipez.internal

import pipez.PipeDerivation

trait PlatformGenerators[Pipe[_, _], In, Out] extends Generators[Pipe, In, Out] {
  self: PlatformDefinitions[Pipe, In, Out] =>

  final def unlift[I, O](
    pipeDerivation: CodeOf[PipeDerivation[Pipe]],
    pipe:           CodeOf[Pipe[I, O]],
    in:             CodeOf[I],
    ctx:            CodeOf[ArbitraryContext]
  ): CodeOf[ArbitraryResult[O]] = ???

  final def lift[I, O](
    pipeDerivation: CodeOf[PipeDerivation[Pipe]],
    call:           CodeOf[(I, ArbitraryContext) => ArbitraryResult[O]]
  ): CodeOf[Pipe[I, O]] = ???

  final def updateContext(
    pipeDerivation: CodeOf[PipeDerivation[Pipe]],
    context:        CodeOf[ArbitraryContext],
    path:           CodeOf[Path]
  ): CodeOf[ArbitraryContext] = ???

  final def pureResult[A](pipeDerivation: CodeOf[PipeDerivation[Pipe]], a: A): CodeOf[ArbitraryResult[A]] = ???

  final def mergeResults[A, B, C](
    pipeDerivation: CodeOf[PipeDerivation[Pipe]],
    ra:             CodeOf[ArbitraryResult[A]],
    rb:             CodeOf[ArbitraryResult[B]],
    f:              CodeOf[(A, B) => C]
  ): CodeOf[ArbitraryResult[C]] = ???
}
