package pipez.internal

import pipez.PipeDerivationConfig

import scala.annotation.nowarn

@nowarn("msg=The outer reference in this type test cannot be checked at run time.")
trait Definitions {

  type Type[A]

  type Code // TODO: remove?
  type CodeOf[A]

  type Argument
  type Field
  type Subtype
  type KeyValue

  sealed trait Path extends Product with Serializable
  object Path {

    case object Root extends Path
    final case class Field(from: Path, name: Field) extends Path
    final case class Subtype(from: Path, name: Subtype) extends Path
    final case class AtIndex(from: Path, index: Int) extends Path
    final case class AtKey(from: Path, key: KeyValue) extends Path
  }

  sealed trait ConfigEntry extends Product with Serializable
  object ConfigEntry {

    final case class AddField(outputField: Path, pipe: Code) extends ConfigEntry
    final case class RenameField(inputField: Path, outputField: Path) extends ConfigEntry
    final case class RemoveSubtype(inputSubtype: Path, pipe: Code) extends ConfigEntry
    final case class RenameSubtype(inputSubtype: Path, outputSubtype: Path) extends ConfigEntry
    final case class PlugIn(inputField: Path, outputField: Path, pipe: Code) extends ConfigEntry
    final case object AllowJavaBeanInput extends ConfigEntry
    final case object AllowJavaBeanOutput extends ConfigEntry
  }

  final class Settings(entries: List[ConfigEntry]) {

    def isJavaBeanInputAllowed: Boolean = entries.contains(ConfigEntry.AllowJavaBeanInput)

    def isJavaBeanOutputAllowed: Boolean = entries.contains(ConfigEntry.AllowJavaBeanOutput)
  }

  sealed trait DerivationError extends Product with Serializable
  object DerivationError {
    // TODO: better details and more cases
    final case class InvalidConfiguration(hint: String) extends DerivationError
    case object NoSupportedCase extends DerivationError
  }

  sealed trait DerivationResult[+A] extends Product with Serializable {

    import DerivationResult._

    final def flatMap[B](f: A => DerivationResult[B]): DerivationResult[B] = this match {
      case Success(value)  => f(value)
      case Failure(errors) => Failure(errors)
    }
    final def map[B](f: A => B): DerivationResult[B] = flatMap(f andThen pure)

    def zip[B](other: DerivationResult[B]): DerivationResult[(A, B)] = (this, other) match {
      case (Success(a), Success(b))   => Success((a, b))
      case (Failure(e1), Failure(e2)) => Failure(e1 ++ e2)
      case (Failure(e), _)            => Failure(e)
      case (_, Failure(e))            => Failure(e)
    }
  }
  object DerivationResult {
    final case class Success[+A](value: A) extends DerivationResult[A]
    final case class Failure(errors: List[DerivationError]) extends DerivationResult[Nothing]

    def pure[A](value: A):                           DerivationResult[A]       = Success(value)
    def fail(error: DerivationError):                DerivationResult[Nothing] = Failure(List(error))
    def failMultiple(errors: List[DerivationError]): DerivationResult[Nothing] = Failure(errors)
  }

  def readConfig[Pipe[_, _], In, Out](
    code: CodeOf[PipeDerivationConfig[Pipe, In, Out]]
  ): DerivationResult[Settings]

  final def readSettingsIfGiven[Pipe[_, _], In, Out](
    code: Option[CodeOf[PipeDerivationConfig[Pipe, In, Out]]]
  ): DerivationResult[Settings] =
    code.fold(DerivationResult.pure(new Settings(Nil)))(readConfig)
}
