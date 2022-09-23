package pipez

/** Mix-in providing `derive` method for semiautomatic `Pipe` derivation without recursion and custom configuration */
trait PipeSemiautoConfiguredSupport[Pipe[_, _]] {

  /** Derives `Pipe[In, Out]` using provided settings */
  inline def derive[In, Out](
    inline config: Config[In, Out]
  )(using
    pipeDerivation: PipeDerivation[Pipe]
  ): Pipe[In, Out] = ${ pipez.internal.Macros.deriveConfigured[Pipe, In, Out]('{ config })('{ pipeDerivation }) }

  /** Utility useful for providing configuration to macro.
    *
    * Example: `TypeClass.derive(TypeClass.Config[In, Out].enableDiagnostics)`.
    */
  object Config {

    /** Initiates the config object. Should be used ONLY within `TypeClass.derive(...)` */
    def apply[In, Out]: PipeDerivationConfig[Pipe, In, Out] = ???
  }
  type Config[In, Out] = PipeDerivationConfig[Pipe, In, Out]
}
