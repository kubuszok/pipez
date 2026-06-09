package pipez

import scala.language.experimental.macros

trait PipeAutoSupport[Pipe[_, _]] {

  implicit def deriveAutomatic[In, Out](implicit
      pipeDerivation: PipeDerivation[Pipe]
  ): Pipe[In, Out] = macro pipez.internal.compiletime.PipezMacro.deriveDefault[Pipe, In, Out]
}
