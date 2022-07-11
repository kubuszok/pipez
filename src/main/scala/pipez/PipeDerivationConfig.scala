package pipez

// phantom type
sealed trait PipeDerivationConfig[Pipe[_, _], In, Out] {

  final def addField[OutField](
      outputField: Out => OutField,
      pipe: Pipe[In, OutField]
  ): PipeDerivationConfig[Pipe, In, Out] = this

  final def renameField(
      inputField: In => Any,
      outputField: Out => Any
  ): PipeDerivationConfig[Pipe, In, Out] = this

  // TODO: addSubtype?

  final def renameSubtype[InSubtype <: In, OutSubtype <: Out]
      : PipeDerivationConfig[Pipe, In, Out] = this

  final def plugIn[InField, OutField](
      inputField: In => InField,
      outputField: Out => OutField,
      pipe: Pipe[InField, OutField]
  ): PipeDerivationConfig[Pipe, In, Out] = this
}
object PipeDerivationConfig {

  // NOT intended to exist at runtime!
  def apply[Pipe[_, _], In, Out]: PipeDerivationConfig[Pipe, In, Out] = ???
}
