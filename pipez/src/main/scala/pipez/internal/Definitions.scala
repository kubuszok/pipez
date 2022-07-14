package pipez.internal

import pipez.PipeDerivationConfig

import scala.annotation.nowarn
import scala.collection.{ Factory, mutable }
import scala.util.chaining.scalaUtilChainingOps

@nowarn("msg=The outer reference in this type test cannot be checked at run time.")
trait Definitions[Pipe[_, _], In, Out] {

  type Type[A]

  type Code // TODO: remove?
  type CodeOf[A]

  type Argument // TODO: add type?
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
    final case object FieldCaseInsensitive extends ConfigEntry
  }

  final class Settings(entries: List[ConfigEntry]) {

    import Path._
    import ConfigEntry._

    lazy val isFieldCaseInsensitive: Boolean = entries.contains(FieldCaseInsensitive)

    // TODO: Pipe[From, To] or Input Field name
    def forOutputFieldUse(outField: String): Either[Code, String] =
      entries.foldLeft[Either[Code, String]](Right(outField)) {
        case (_, AddField(Field(Root, name), pipe))
            if name.toString.equals(outField) || (isFieldCaseInsensitive && name.toString.equalsIgnoreCase(outField)) =>
          Left(pipe)
        case (_, RenameField(Field(Root, inName), Field(Root, outName)))
            if outName.toString
              .equals(outField) || (isFieldCaseInsensitive && outName.toString.equalsIgnoreCase(outField)) =>
          Right(inName.toString)
        case (old, _) => old
      }
  }

  sealed trait DerivationError extends Product with Serializable
  object DerivationError {

    final case class MissingPublicConstructor(outType: Type[_]) extends DerivationError
    final case class MissingPublicSource(outFieldName: String) extends DerivationError
    final case class NotSupportedConversion[In, Out](inType: Type[In], outType: Type[Out]) extends DerivationError
    case object NotYetSupported extends DerivationError
  }

  sealed trait DerivationResult[+A] extends Product with Serializable {

    import DerivationResult._

    final def flatMap[B](f: A => DerivationResult[B]): DerivationResult[B] = this match {
      case Success(value, diagnostic) =>
        f(value) match {
          case Success(value, diagnostic2)  => Success(value, diagnostic ++ diagnostic2)
          case Failure(errors, diagnostic2) => Failure(errors, diagnostic ++ diagnostic2)
        }
      case Failure(errors, diagnostic) => Failure(errors, diagnostic)
    }
    final def map[B](f: A => B): DerivationResult[B] = flatMap(f andThen pure)

    final def map2[B, C](other: DerivationResult[B])(f: (A, B) => C): DerivationResult[C] = (this, other) match {
      case (Success(a, d1), Success(b, d2))   => Success(f(a, b), d1 ++ d2)
      case (Failure(e1, d1), Failure(e2, d2)) => Failure(e1 ++ e2, d1 ++ d2)
      case (Failure(e, d1), Success(_, d2))   => Failure(e, d1 ++ d2)
      case (Success(_, d1), Failure(e, d2))   => Failure(e, d1 ++ d2)
    }

    final def zip[B](other: DerivationResult[B]): DerivationResult[(A, B)] = map2(other)(_ -> _)

    final def fold[B](success: A => B)(failure: List[DerivationError] => B): B = this match {
      case Success(value, _)  => success(value)
      case Failure(errors, _) => failure(errors)
    }

    def diagnostic: Diagnostic
    final def log(message: String): DerivationResult[A] = this match {
      case Success(value, diagnostic)  => Success(value, diagnostic :+ message)
      case Failure(errors, diagnostic) => Failure(errors, diagnostic :+ message)
    }
  }
  object DerivationResult {

    final private case class Success[+A](
      value:      A,
      diagnostic: Diagnostic
    ) extends DerivationResult[A]

    final private case class Failure(
      errors:     List[DerivationError],
      diagnostic: Diagnostic
    ) extends DerivationResult[Nothing]

    type Diagnostic = Vector[String]

    def pure[A](value: A):                           DerivationResult[A]       = Success(value, Vector.empty)
    def fail(error: DerivationError):                DerivationResult[Nothing] = Failure(List(error), Vector.empty)
    def failMultiple(errors: List[DerivationError]): DerivationResult[Nothing] = Failure(errors, Vector.empty)

    def sequence[A, Coll[A0] <: Seq[A0]](
      seq: Coll[DerivationResult[A]]
    )(implicit
      factory: Factory[A, Coll[A]]
    ): DerivationResult[Coll[A]] =
      seq.foldLeft(pure(factory.newBuilder))((builder, next) => builder.map2(next)(_.addOne(_))).map(_.result())

    def fromOption[A](opt: Option[A])(err: => DerivationError): DerivationResult[A] =
      opt.fold[DerivationResult[A]](fail(err))(pure)
  }

  def readConfig(
    code: CodeOf[PipeDerivationConfig[Pipe, In, Out]]
  ): DerivationResult[Settings]

  final def readSettingsIfGiven(
    code: Option[CodeOf[PipeDerivationConfig[Pipe, In, Out]]]
  ): DerivationResult[Settings] =
    code
      .fold(DerivationResult.pure(new Settings(Nil)))(readConfig)
      .log(if (code.isDefined) "Derivation started with configuration" else "Derivation started without configuration")
}
