package pipez

trait PipeAutoSupport[Pipe[_, _]] {

  implicit inline def deriveAutomatic[In, Out](implicit
    pipeDerivation: PipeDerivation[Pipe]
  ): Pipe[In, Out] = ${ pipez.internal.Macros.deriveDefault[Pipe, In, Out]('{ pipeDerivation }) }
}
