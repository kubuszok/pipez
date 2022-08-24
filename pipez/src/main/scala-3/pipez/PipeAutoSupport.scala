package pipez

trait PipeAutoSupport[Pipe[_, _]] {

  implicit inline def derive[In, Out](implicit
    pipeDerivation: PipeDerivation[Pipe]
  ): Pipe[In, Out] = ${ pipez.internal.Macros.deriveDefault[Pipe, In, Out]('{ pipeDerivation }) }
}
