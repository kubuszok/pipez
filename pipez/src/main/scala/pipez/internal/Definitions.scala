package pipez.internal

import pipez.PipeDerivationConfig

import scala.annotation.nowarn
import scala.collection.{ Factory, mutable }
import scala.util.chaining.scalaUtilChainingOps

@nowarn("msg=The outer reference in this type test cannot be checked at run time.")
trait Definitions[Pipe[_, _], In, Out] {

  type Type[A]
  type Argument[A]
  type CodeOf[A]

  val inType:  Type[In]
  val outType: Type[Out]

  sealed trait Path extends Product with Serializable
  object Path {

    case object Root extends Path
    final case class Field(from: Path, name: String) extends Path
    final case class Subtype[A](from: Path, tpe: Type[A]) extends Path
    final case class AtIndex(from: Path, index: Int) extends Path // might not be needed
    final case class AtKey(from: Path, key: String) extends Path // might not be needed
  }

  sealed trait ConfigEntry extends Product with Serializable
  object ConfigEntry {

    final case class AddField[OutField](
      outputField:  Path,
      outFieldType: Type[OutField],
      pipe:         CodeOf[Pipe[In, OutField]]
    ) extends ConfigEntry

    final case class RenameField[InField, OutField](
      inputField:     Path,
      inputFieldType: Type[InField],
      outputField:    Path,
      outFieldType:   Type[OutField]
    ) extends ConfigEntry

    final case class RemoveSubtype[InSubtype](
      inputSubtype:  Path,
      inSubtypeType: Type[InSubtype],
      pipe:          CodeOf[Pipe[InSubtype, Out]]
    ) extends ConfigEntry

    final case class RenameSubtype(
      inputSubtype:  Path,
      outputSubtype: Path
    ) extends ConfigEntry

    final case class PlugInField[InField, OutField](
      inputField:     Path,
      inputFieldType: Type[InField],
      outputField:    Path,
      outFieldType:   Type[OutField],
      pipe:           CodeOf[Pipe[InField, OutField]]
    ) extends ConfigEntry

    case object FieldCaseInsensitive extends ConfigEntry
  }

  sealed trait OutFieldLogic[OutField] extends Product with Serializable
  object OutFieldLogic {

    final case class DefaultField[OutField]() extends OutFieldLogic[OutField]

    final case class FieldAdded[OutField](
      pipe: CodeOf[Pipe[In, OutField]]
    ) extends OutFieldLogic[OutField]

    final case class FieldRenamed[InField, OutField](
      inField:     String,
      inFieldType: Type[InField]
    ) extends OutFieldLogic[OutField]

    final case class PipeProvided[InField, OutField](
      inField:     String,
      inFieldType: Type[InField],
      pipe:        CodeOf[Pipe[InField, OutField]]
    ) extends OutFieldLogic[OutField]
  }

  final class Settings(entries: List[ConfigEntry]) {

    import Path._
    import ConfigEntry._

    lazy val isFieldCaseInsensitive: Boolean = entries.contains(FieldCaseInsensitive)

    def forOutputFieldUse[OutField](outField: String, outFieldType: Type[OutField]): OutFieldLogic[OutField] =
      entries.foldLeft[OutFieldLogic[OutField]](OutFieldLogic.DefaultField()) {
        case (_, AddField(Field(Root, `outField`), pipe, outType)) =>
          // validate that outType <:< outFieldType is correct
          OutFieldLogic.FieldAdded(pipe.asInstanceOf[CodeOf[Pipe[In, OutField]]])
        case (_, RenameField(Field(Root, inName), inType, Field(Root, `outField`), outType)) =>
          // validate that outType <:< outFieldType is correct
          OutFieldLogic.FieldRenamed(inName, inType)
        case (_, PlugInField(Field(Root, inName), inType, Field(Root, `outField`), outType, pipe)) =>
          // validate that outType <:< outFieldType is correct
          OutFieldLogic.PipeProvided(inName, inType, pipe.asInstanceOf[CodeOf[Pipe[Any, OutField]]])
        case (old, _) => old
      }
  }

  sealed trait DerivationError extends Product with Serializable
  object DerivationError {

    case object MissingPublicConstructor extends DerivationError

    final case class MissingPublicSource(
      outFieldName: String
    ) extends DerivationError

    final case class NotSupportedConversion[I, O](
      inField:  String,
      inType:   Type[I],
      outField: String,
      outType:  Type[O]
    ) extends DerivationError

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

  def summonPipe[InField, OutField](
    inType:  Type[InField],
    outType: Type[OutField]
  ): DerivationResult[CodeOf[Pipe[InField, OutField]]]

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
