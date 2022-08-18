package pipez

/** Let you customize the derivation process
  *
  * It's used as a phantom type, so it shouldn't be stored in values and only created in-place in .derive(...) macro
  */
sealed trait PipeDerivationConfig[Pipe[_, _], In, Out] {

  /** Prints additional information during compilation about the derivation process, generated code and time it took */
  final def enableDiagnostics: this.type = this

  /** Let you manually compute the value for an output field by providing a Pipe from a whole In object */
  final def addField[OutField](outputField: Out => OutField, pipe: Pipe[In, OutField]): this.type = this

  /** Let you specify that value of a specific output field should be calculated using a specific input field */
  final def renameField[InField, OutField](inputField: In => InField, outputField: Out => OutField): this.type = this

  /** Let you specify the exact way a specific output field is computed using specific input field */
  final def plugInField[InField, OutField](
    inputField:  In => InField,
    outputField: Out => OutField,
    pipe:        Pipe[InField, OutField]
  ): this.type = this

  /** During derivation field names from In to Out won't be matches exactly but with case-insensitive comparison */
  final def fieldMatchingCaseInsensitive: this.type = this

  /** Let you manually handle removal of a specific subtype of output sum type */
  final def removeSubtype[InSubtype <: In](pipe: Pipe[InSubtype, Out]): this.type = this

  /** Let you specify that a specific output subtype is corresponding to a specific input subtype */
  final def renameSubtype[InSubtype <: In, OutSubtype <: Out]: this.type = this

  /** Let you specify the exact way a specific output subtype is computed using specific input subtype */
  final def plugInSubtype[InSubtype <: In, OutSubtype <: Out](pipe: Pipe[InSubtype, OutSubtype]): this.type = this

  /** During derivation subtype names from In to Out won't be matches exactly but with case-insensitive comparison */
  final def enumMatchingCaseInsensitive: this.type = this
}
object PipeDerivationConfig {

  /** Initiates the config object. Should be used ONLY within `PipeDerivation.derive(...)` */
  def apply[Pipe[_, _], In, Out]: PipeDerivationConfig[Pipe, In, Out] = ???
}
