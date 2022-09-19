package pipez

/** Mix-in providing `derive` method for automatic `Pipe` derivation allowing recursion but not custom configuration */
trait PipeAutoSupport[Pipe[_, _]] {

  /** Derives `Pipe[In, Out]` using default settings, defined as implicit */
  implicit inline def deriveAutomatic[In, Out](implicit
    pipeDerivation: PipeDerivation[Pipe]
  ): Pipe[In, Out] = ${ pipez.internal.Macros.deriveDefault[Pipe, In, Out]('{ pipeDerivation }) }
}
