package pipez.internal

trait PlatformGenerators[Pipe[_, _], In, Out](using quotes: scala.quoted.Quotes)
    extends Generators[Pipe, In, Out]
    with PlatformProductCaseGeneration[Pipe, In, Out]
    with PlatformSumCaseGeneration[Pipe, In, Out] {
  self: PlatformDefinitions[Pipe, In, Out] =>

  import quotes.*

  final def isSubtype[A, B](lower: Type[A], higher: Type[B]): Boolean = ???

  def reportDiagnostics[A](result: DerivationResult[A]): Unit = ???

  def reportError(errors: List[DerivationError]): Nothing = ???

  final def lift[I, O](
    call: CodeOf[(I, ArbitraryContext) => ArbitraryResult[O]]
  ): CodeOf[Pipe[I, O]] = ???

  final def unlift[I, O](
    pipe: CodeOf[Pipe[I, O]],
    in:   CodeOf[I],
    ctx:  Argument[ArbitraryContext]
  ): CodeOf[ArbitraryResult[O]] = ???

  final def updateContext(
    ctx:  Argument[ArbitraryContext],
    path: CodeOf[Path]
  ): CodeOf[ArbitraryContext] = ???

  final def pureResult[A](a: CodeOf[A]): CodeOf[ArbitraryResult[A]] = ???

  final def mergeResults[A, B, C](
    ra: CodeOf[ArbitraryResult[A]],
    rb: CodeOf[ArbitraryResult[B]],
    f:  CodeOf[(A, B) => C]
  ): CodeOf[ArbitraryResult[C]] = ???
}
