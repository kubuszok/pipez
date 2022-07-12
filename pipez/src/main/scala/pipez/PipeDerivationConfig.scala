package pipez

// phantom type
sealed trait PipeDerivationConfig[Pipe[_, _], In, Out] {

  final def addField[OutField](outputField: Out => OutField, pipe: Pipe[In, OutField]): this.type = this

  final def renameField[InField, OutField](inputField: In => InField, outputField: Out => OutField): this.type = this

  final def removeSubtype[InSubtype <: In](pipe: Pipe[InSubtype, Out]): this.type = this

  final def renameSubtype[InSubtype <: In, OutSubtype <: Out]: this.type = this

  final def plugIn[InField, OutField](
    inputField:  In => InField,
    outputField: Out => OutField,
    pipe:        Pipe[InField, OutField]
  ): this.type = this

  final def fieldMatchingCaseInsensitive: this.type = this
}
object PipeDerivationConfig {

  // NOT intended to exist at runtime!
  def apply[Pipe[_, _], In, Out]: PipeDerivationConfig[Pipe, In, Out] = ???
}
