package pipez

trait PipeDerivationPlatform { self: PipeDerivation.type =>

  inline def derive[Pipe[_, _], In, Out](using
    pipeDerivation: PipeDerivation[Pipe]
  ): Pipe[In, Out] = ${ pipez.internal.Macros.deriveDefault[Pipe, In, Out]('{ pipeDerivation }) }

  inline def derive[Pipe[_, _], In, Out](
    inline config: PipeDerivationConfig[Pipe, In, Out]
  )(using
    pipeDerivation: PipeDerivation[Pipe]
  ): Pipe[In, Out] = ${ pipez.internal.Macros.deriveConfigured[Pipe, In, Out]('{ config })('{ pipeDerivation }) }

  /** Default instance for `(In, Ctx) => Out` with `Ctx` passed around without updating */
  implicit def contextFunction[Ctx]: PipeDerivation[(_, Ctx) => _] = contextFunction[Ctx]()
}
