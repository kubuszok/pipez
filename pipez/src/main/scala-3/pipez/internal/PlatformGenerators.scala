package pipez.internal

trait PlatformGenerators[Pipe[_, _], In, Out](using quotes: scala.quoted.Quotes)
    extends Generators[Pipe, In, Out]
    with PlatformProductCaseGeneration[Pipe, In, Out]
    with PlatformSumCaseGeneration[Pipe, In, Out] {
  self: PlatformDefinitions[Pipe, In, Out] =>

  import quotes.*
  import quotes.reflect.*

  final def isSubtype[A: Type, B: Type]: Boolean =
    TypeRepr.of[A] <:< TypeRepr.of[B]

  def reportDiagnostics[A](result: DerivationResult[A]): Unit =
    report.info(diagnosticsMessage(result))

  def reportError(errors: List[DerivationError]): Nothing =
    report.errorAndAbort(errorMessage(errors))

  final def lift[I: Type, O: Type](
    call: CodeOf[(I, ArbitraryContext) => ArbitraryResult[O]]
  ): CodeOf[Pipe[I, O]] = '{ $pipeDerivation.lift($call) }

  final def unlift[I: Type, O: Type](
    pipe: CodeOf[Pipe[I, O]],
    in:   CodeOf[I],
    ctx:  Argument[ArbitraryContext]
  ): CodeOf[ArbitraryResult[O]] = '{ $pipeDerivation.unlift($pipe, $in, $ctx) }

  final def updateContext(
    ctx:  Argument[ArbitraryContext],
    path: CodeOf[pipez.Path]
  ): CodeOf[ArbitraryContext] = '{ $pipeDerivation.updateContext($ctx, $path) }

  final def pureResult[A: Type](a: CodeOf[A]): CodeOf[ArbitraryResult[A]] = '{ $pipeDerivation.pureResult($a) }

  final def mergeResults[A: Type, B: Type, C: Type](
    ra: CodeOf[ArbitraryResult[A]],
    rb: CodeOf[ArbitraryResult[B]],
    f:  CodeOf[(A, B) => C]
  ): CodeOf[ArbitraryResult[C]] = '{ $pipeDerivation.mergeResults($ra, $rb, $f) }
}
