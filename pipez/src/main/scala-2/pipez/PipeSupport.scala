package pipez

import scala.language.experimental.macros

trait PipeSupport[Pipe[_, _]] {

  def derive[In, Out](implicit
    pd: PipeDerivation[Pipe]
  ): Pipe[In, Out] = macro pipez.internal.MacroDispatcher.deriveDefault[Pipe, In, Out]

  def derive[In, Out](
    config: PipeDerivationConfig[Pipe, In, Out]
  )(implicit
    pd: PipeDerivation[Pipe]
  ): Pipe[In, Out] = macro pipez.internal.MacroDispatcher.deriveConfigured[Pipe, In, Out]

  object Auto {

    implicit def derive[In, Out](implicit
      pd: PipeDerivation[Pipe]
    ): Pipe[In, Out] = macro pipez.internal.MacroDispatcher.deriveDefault[Pipe, In, Out]
  }
}
