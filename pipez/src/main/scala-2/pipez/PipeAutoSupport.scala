package pipez

import scala.language.experimental.macros

/** Mix-in providing `derive` method for automatic `Pipe` derivation allowing recursion but not custom configuration */
trait PipeAutoSupport[Pipe[_, _]] {

  /** Derives `Pipe[In, Out]` using default settings, defined as implicit */
  implicit def deriveAutomatic[In, Out](implicit
    pipeDerivation: PipeDerivation[Pipe]
  ): Pipe[In, Out] = macro pipez.internal.Macro.deriveDefault[Pipe, In, Out]
}
