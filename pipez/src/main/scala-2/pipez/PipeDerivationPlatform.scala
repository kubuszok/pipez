package pipez

import scala.reflect.macros.blackbox.Context
import scala.language.experimental.macros

trait PipeDerivationPlatform { self: PipeDerivation.type =>

  def derive[Pipe[_, _], In, Out](implicit
    pd: PipeDerivation[Pipe]
  ): Pipe[In, Out] = macro pipez.internal.MacroDispatcher.deriveDefault[Pipe, In, Out]

  def derive[Pipe[_, _], In, Out](
    config: PipeDerivationConfig[Pipe, In, Out]
  )(implicit
    pd: PipeDerivation[Pipe]
  ): Pipe[In, Out] = macro pipez.internal.MacroDispatcher.deriveConfigured[Pipe, In, Out]
}
