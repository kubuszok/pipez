package pipez

import scala.language.experimental.macros

trait PipeSemiautoConfiguredSupport[Pipe[_, _]] {

  def derive[In, Out](
      config: PipeDerivationConfig[Pipe, In, Out]
  )(implicit
      pipeDerivation: PipeDerivation[Pipe]
  ): Pipe[In, Out] = macro pipez.internal.compiletime.PipezMacro.deriveConfigured[Pipe, In, Out]

  object Config {
    def apply[In, Out]: Config[In, Out] = ???
    def empty[In, Out]: Config[In, Out] = ???
  }
  type Config[In, Out] = PipeDerivationConfig[Pipe, In, Out]
}
