package pipez

trait PipeSemiautoSupport[Pipe[_, _]] {

  /** Derives `Pipe[In, Out]` using default settings */
  inline def derive[In, Out](using
      inline pipeDerivation: PipeDerivation[Pipe]
  ): Pipe[In, Out] = ${
    pipez.internal.compiletime.PipezMacros.deriveDefault[Pipe, In, Out]('pipeDerivation)
  }
}
