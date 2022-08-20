package pipez

trait PipeSemiautoConfiguredSupport[Pipe[_, _]] {

  inline def derive[Pipe[_, _], In, Out](
    inline config: PipeDerivationConfig[Pipe, In, Out]
  )(using
    pipeDerivation: PipeDerivation[Pipe]
  ): Pipe[In, Out] = ${ pipez.internal.Macros.deriveConfigured[Pipe, In, Out]('{ config })('{ pipeDerivation }) }

  object Config {

    def apply[In, Out]: PipeDerivationConfig[Pipe, In, Out] = ???
  }
}
