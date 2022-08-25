package pipez.internal

import pipez.internal.Definitions.{ Context, Result }

trait PlatformGenerators[Pipe[_, _], In, Out]
    extends Generators[Pipe, In, Out]
    with PlatformProductCaseGeneration[Pipe, In, Out]
    with PlatformSumCaseGeneration[Pipe, In, Out] {
  self: PlatformDefinitions[Pipe, In, Out] =>

  import c.universe.*

  final def isSubtype[A: Type, B: Type]: Boolean =
    typeOf[A] <:< typeOf[B]

  def reportDiagnostics[A](result: DerivationResult[A]): Unit =
    c.echo(c.enclosingPosition, diagnosticsMessage(result))

  def reportError(errors: List[DerivationError]): Nothing =
    c.abort(c.enclosingPosition, errorMessage(errors))

  final def lift[I: Type, O: Type](
    call: CodeOf[(I, Context) => Result[O]]
  ): CodeOf[Pipe[I, O]] = c.Expr[Pipe[I, O]](q"""$pipeDerivation.lift($call)""")

  final def unlift[I: Type, O: Type](
    pipe: CodeOf[Pipe[I, O]],
    in:   CodeOf[I],
    ctx:  CodeOf[Context]
  ): CodeOf[Result[O]] = c.Expr[Result[O]](q"""$pipeDerivation.unlift($pipe, $in, $ctx)""")

  final def updateContext(
    ctx:  CodeOf[Context],
    path: CodeOf[pipez.Path]
  ): CodeOf[Context] = c.Expr[Context](q"""$pipeDerivation.updateContext($ctx, $path)""")

  final def pureResult[A: Type](a: CodeOf[A]): CodeOf[Result[A]] =
    c.Expr[Result[A]](q"""$pipeDerivation.pureResult($a)""")

  final def mergeResults[A: Type, B: Type, C: Type](
    ra: CodeOf[Result[A]],
    rb: CodeOf[Result[B]],
    f:  CodeOf[(A, B) => C]
  ): CodeOf[Result[C]] = c.Expr[Result[C]](q"""$pipeDerivation.mergeResults($ra, $rb, $f)""")
}
