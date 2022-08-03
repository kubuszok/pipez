package pipez.internal

import pipez.PipeDerivation

trait PlatformGenerators[Pipe[_, _], In, Out] extends Generators[Pipe, In, Out] {
  self: PlatformDefinitions[Pipe, In, Out] =>

  import c.universe._

  final def lift[I, O](
    call: CodeOf[(I, ArbitraryContext) => ArbitraryResult[O]]
  ): CodeOf[Pipe[I, O]] = c.Expr[Pipe[I, O]](q"""$pipeDerivation.lift($call)""")

  final def unlift[I, O](
    pipe: CodeOf[Pipe[I, O]],
    in:   CodeOf[I],
    ctx:  Argument[ArbitraryContext]
  ): CodeOf[ArbitraryResult[O]] = c.Expr[ArbitraryResult[O]](q"""$pipeDerivation.unlift($pipe)($in, $ctx)""")

  final def updateContext(
    ctx:  Argument[ArbitraryContext],
    path: CodeOf[Path]
  ): CodeOf[ArbitraryContext] = c.Expr[ArbitraryContext](q"""$pipeDerivation.updateContext($ctx, $path)""")

  final def pureResult[A](a: CodeOf[A]): CodeOf[ArbitraryResult[A]] =
    c.Expr[ArbitraryResult[A]](q"""$pipeDerivation.pureResult($a)""")

  final def mergeResults[A, B, C](
    ra: CodeOf[ArbitraryResult[A]],
    rb: CodeOf[ArbitraryResult[B]],
    f:  CodeOf[(A, B) => C]
  ): CodeOf[ArbitraryResult[C]] = c.Expr[ArbitraryResult[C]](q"""$pipeDerivation.mergeResults($ra, $rb)($f)""")
}
