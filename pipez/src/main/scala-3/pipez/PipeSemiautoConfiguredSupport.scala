package pipez

trait PipeSemiautoConfiguredSupport[Pipe[_, _]] {

  inline def derive[Pipe[_, _], In, Out](
    inline config: PipeDerivationConfig[Pipe, In, Out]
  )(using
    inline pd: PipeDerivation[Pipe]
  ): Pipe[In, Out] = ${ pipez.internal.Macros.deriveConfigured[Pipe, In, Out]('{ config })('{ pd }) }

  object Config {

    def apply[In, Out]: PipeDerivationConfig[Pipe, In, Out] = ???
  }
}
