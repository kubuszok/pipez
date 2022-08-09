package pipez

import scala.language.experimental.macros

/** Mix-in providing `derive` method for semiautomatic `Pipe` derivation without recursion and default config */
trait PipeSemiautoSupport[Pipe[_, _]] {

  def derive[In, Out](implicit
    pd: PipeDerivation[Pipe]
  ): Pipe[In, Out] = macro pipez.internal.MacroDispatcher.deriveDefault[Pipe, In, Out]
}
