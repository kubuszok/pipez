package pipez

import scala.language.experimental.macros

/** Mix-in providing `derive` method for semiautomatic `Pipe` derivation without recursion and custom configuration */
trait PipeSemiautoConfiguredSupport[Pipe[_, _]] {

  /** Derives `Pipe[In, Out]` using provided settings */
  def derive[In, Out](
    config: PipeDerivationConfig[Pipe, In, Out]
  )(implicit
    pipeDerivation: PipeDerivation[Pipe]
  ): Pipe[In, Out] = macro pipez.internal.Macro.deriveConfigured[Pipe, In, Out]

  /** Utility useful for providing configuration to macro.
    *
    * Example: `TypeClass.derive(TypeClass.Config[In, Out].enableDiagnostics)`.
    */
  object Config {

    /** Initiates the config object. Should be used ONLY within `TypeClass.derive(...)` */
    def apply[In, Out]: Config[In, Out] = ???

    /** Initiates the config object. Should be used ONLY within `TypeClass.derive(...)` */
    def empty[In, Out]: Config[In, Out] = ???
  }
  type Config[In, Out] = PipeDerivationConfig[Pipe, In, Out]
}
