package pipez

import scala.language.experimental.macros

private[pipez] trait PipeDerivationPlatform { self: PipeDerivation.type =>

  def derive[Pipe[_, _], In, Out](implicit
      pipeDerivation: PipeDerivation[Pipe]
  ): Pipe[In, Out] = macro pipez.internal.compiletime.PipezMacro.deriveDefault[Pipe, In, Out]

  def derive[Pipe[_, _], In, Out](
      config: PipeDerivationConfig[Pipe, In, Out]
  )(implicit
      pipeDerivation: PipeDerivation[Pipe]
  ): Pipe[In, Out] = macro pipez.internal.compiletime.PipezMacro.deriveConfigured[Pipe, In, Out]
}
