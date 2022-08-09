package pipez

import scala.language.experimental.macros

/** Mix-in providing `derive` method for automatic `Pipe` derivation allowing recursion but not custom configuration */
trait PipeAutoSupport[Pipe[_, _]] {

  implicit def derive[In, Out](implicit
    pd: PipeDerivation[Pipe]
  ): Pipe[In, Out] = macro pipez.internal.MacroDispatcher.deriveDefault[Pipe, In, Out]
}
