package pipez.internal

trait PlatformGenerators[Pipe[_, _], In, Out]
    extends Generators[Pipe, In, Out]
    with ProductCaseGeneration[Pipe, In, Out]
    with SumCaseGeneration[Pipe, In, Out] {
  self: PlatformDefinitions[Pipe, In, Out] =>

  import c.universe.*

  final def isSubtype[A, B](lower: Type[A], higher: Type[B]): Boolean =
    lower <:< higher

  def reportDiagnostics[A](result: DerivationResult[A]): Unit =
    c.echo(c.enclosingPosition, diagnosticsMessage(result))

  def reportError(errors: List[DerivationError]): Nothing =
    c.abort(c.enclosingPosition, errorMessage(errors))

  final def lift[I, O](
    call: CodeOf[(I, ArbitraryContext) => ArbitraryResult[O]]
  ): CodeOf[Pipe[I, O]] = c.Expr[Pipe[I, O]](q"""$pipeDerivation.lift($call)""")

  final def unlift[I, O](
    pipe: CodeOf[Pipe[I, O]],
    in:   CodeOf[I],
    ctx:  CodeOf[ArbitraryContext]
  ): CodeOf[ArbitraryResult[O]] = c.Expr[ArbitraryResult[O]](q"""$pipeDerivation.unlift($pipe, $in, $ctx)""")

  final def updateContext(
    ctx:  CodeOf[ArbitraryContext],
    path: CodeOf[pipez.Path]
  ): CodeOf[ArbitraryContext] = c.Expr[ArbitraryContext](q"""$pipeDerivation.updateContext($ctx, $path)""")

  final def pureResult[A](a: CodeOf[A]): CodeOf[ArbitraryResult[A]] =
    c.Expr[ArbitraryResult[A]](q"""$pipeDerivation.pureResult($a)""")

  final def mergeResults[A, B, C](
    ra: CodeOf[ArbitraryResult[A]],
    rb: CodeOf[ArbitraryResult[B]],
    f:  CodeOf[(A, B) => C]
  ): CodeOf[ArbitraryResult[C]] = c.Expr[ArbitraryResult[C]](q"""$pipeDerivation.mergeResults($ra, $rb, $f)""")
}
