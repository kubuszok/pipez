package pipez

trait PipeAutoSupport[Pipe[_, _]] {

  inline given derive[In, Out](using
    inline pd: PipeDerivation[Pipe]
  ): Pipe[In, Out] = ${ pipez.internal.Macros.deriveDefault[Pipe, In, Out]('{ pd }) }
}
