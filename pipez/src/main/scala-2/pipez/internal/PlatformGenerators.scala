package pipez.internal

import pipez.PipeDerivation

trait PlatformGenerators[Pipe[_, _], In, Out] extends Generators[Pipe, In, Out] {
  self: PlatformDefinitions[Pipe, In, Out] =>

  // TODO: use pd passed as implicit

  final def unlift[I, O](
    pipe: CodeOf[Pipe[I, O]],
    in:   CodeOf[I],
    ctx:  Argument[ArbitraryContext]
  ): CodeOf[ArbitraryResult[O]] = ???

  final def lift[I, O](
    call: CodeOf[(I, ArbitraryContext) => ArbitraryResult[O]]
  ): CodeOf[Pipe[I, O]] = ???

  final def updateContext(
    context: Argument[ArbitraryContext],
    path:    CodeOf[Path]
  ): CodeOf[ArbitraryContext] = ???

  final def pureResult[A](a: CodeOf[A]): CodeOf[ArbitraryResult[A]] = ???

  final def mergeResults[A, B, C](
    ra: CodeOf[ArbitraryResult[A]],
    rb: CodeOf[ArbitraryResult[B]],
    f:  CodeOf[(A, B) => C]
  ): CodeOf[ArbitraryResult[C]] = ???
}
