package pipez

trait PipeSemiautoSupport[Pipe[_, _]] {

  inline def derive[In, Out](using
    inline pd: PipeDerivation[Pipe]
  ): Pipe[In, Out] = ${ pipez.internal.Macros.deriveDefault[Pipe, In, Out]('{ pd }) }
}
