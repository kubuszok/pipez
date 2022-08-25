package pipez

import scala.language.experimental.macros

trait PipeDerivationPlatform { self: PipeDerivation.type =>

  def derive[Pipe[_, _], In, Out](implicit
    pipeDerivation: PipeDerivation[Pipe]
  ): Pipe[In, Out] = macro pipez.internal.Macro.deriveDefault[Pipe, In, Out]

  def derive[Pipe[_, _], In, Out](
    config: PipeDerivationConfig[Pipe, In, Out]
  )(implicit
    pipeDerivation: PipeDerivation[Pipe]
  ): Pipe[In, Out] = macro pipez.internal.Macro.deriveConfigured[Pipe, In, Out]
}
