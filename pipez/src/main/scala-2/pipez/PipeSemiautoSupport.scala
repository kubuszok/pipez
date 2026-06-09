package pipez

import scala.language.experimental.macros

trait PipeSemiautoSupport[Pipe[_, _]] {

  def derive[In, Out](implicit
      pipeDerivation: PipeDerivation[Pipe]
  ): Pipe[In, Out] = macro pipez.internal.compiletime.PipezMacro.deriveDefault[Pipe, In, Out]
}
