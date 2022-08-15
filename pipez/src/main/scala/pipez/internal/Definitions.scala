package pipez.internal

import pipez.{ PipeDerivation, PipeDerivationConfig }

import scala.annotation.nowarn
import scala.collection.Factory

@nowarn("msg=The outer reference in this type test cannot be checked at run time.")
trait Definitions[Pipe[_, _], In, Out] {

  type Type[A]
  type Argument[A]
  type CodeOf[A]

  implicit def pipeType[I: Type, O: Type]: Type[Pipe[I, O]]
  implicit val inType:  Type[In]
  implicit val outType: Type[Out]

  /** Can be used instead of pd.Context to avoid path-dependent types */
  type ArbitraryContext

  /** Can be used instead of pd.Context to avoid path-dependent types */
  type ArbitraryResult[O]

  val inCode:         Argument[In] => CodeOf[In]
  val pipeDerivation: CodeOf[PipeDerivation[Pipe] { type Context = ArbitraryContext; type Result[O] = ArbitraryResult[O] }]

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

    case object EnableDiagnostics extends ConfigEntry

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

    final case class PlugInField[InField, OutField](
      inputField:     Path,
      inputFieldType: Type[InField],
      outputField:    Path,
      outFieldType:   Type[OutField],
      pipe:           CodeOf[Pipe[InField, OutField]]
    ) extends ConfigEntry

    case object FieldCaseInsensitive extends ConfigEntry

    final case class RemoveSubtype[InSubtype <: In](
      inputSubtype:     Path,
      inputSubtypeType: Type[InSubtype],
      pipe:             CodeOf[Pipe[InSubtype, Out]]
    ) extends ConfigEntry

    final case class RenameSubtype[InSubtype <: In, OutSubtype <: Out](
      inputSubtype:      Path,
      inputSubtypeType:  Type[InSubtype],
      outputSubtype:     Path,
      outputSubtypeType: Type[OutSubtype]
    ) extends ConfigEntry

    final case class PlugInSubtype[InSubtype <: In, OutSubtype <: Out](
      inputSubtype:      Path,
      inputSubtypeType:  Type[InSubtype],
      outputSubtype:     Path,
      outputSubtypeType: Type[OutSubtype],
      pipe:              CodeOf[Pipe[InSubtype, OutSubtype]]
    ) extends ConfigEntry

    case object EnumCaseInsensitive extends ConfigEntry
  }

  final class Settings(entries: List[ConfigEntry]) {

    import ConfigEntry.*

    lazy val isDiagnosticsEnabled: Boolean = entries.contains(EnableDiagnostics)

    lazy val isFieldCaseInsensitive: Boolean = entries.contains(FieldCaseInsensitive)

    lazy val isEnumCaseInsensitive: Boolean = entries.contains(EnumCaseInsensitive)

    def resolve[A](default: A)(overrideWhen: PartialFunction[ConfigEntry, A]): A = entries.foldLeft(default) {
      (a, entry) => overrideWhen.applyOrElse[ConfigEntry, A](entry, _ => a)
    }

    override def toString: String = s"Settings(${entries.mkString(", ")})"
  }

  sealed trait DerivationError extends Product with Serializable
  object DerivationError {

    case object MissingPublicConstructor extends DerivationError

    final case class MissingPublicSource(
      outFieldName: String
    ) extends DerivationError

    final case class MissingMatchingSubType(
      subtypeName: String
    ) extends DerivationError

    final case class MissingMatchingValue(
      valueName: String
    ) extends DerivationError

    final case class RequiredImplicitNotFound[I, O](
      inFieldType:  Type[I],
      outFieldType: Type[O]
    ) extends DerivationError

    final case class NotSupportedFieldConversion[I, O](
      inField:      String,
      inFieldType:  Type[I],
      outField:     String,
      outFieldType: Type[O]
    ) extends DerivationError

    final case class NotSupportedEnumConversion(
      isInSumType:  Boolean,
      isOutSumType: Boolean
    ) extends DerivationError

    case object NotYetSupported extends DerivationError

    final case class InvalidConfiguration(msg: String) extends DerivationError

    final case class NotYetImplemented(msg: String) extends DerivationError
  }

  sealed trait DerivationResult[+A] extends Product with Serializable {

    import DerivationResult.*

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
    final def logResult(success: A => String)(failure: List[DerivationError] => String): DerivationResult[A] =
      this match {
        case DerivationResult.Success(value, _)  => log(success(value))
        case DerivationResult.Failure(errors, _) => log(failure(errors))
      }
    final def logSuccess(success: A => String): DerivationResult[A] = this match {
      case DerivationResult.Success(value, _) => log(success(value))
      case DerivationResult.Failure(_, _)     => this
    }
    final def logFailure(failure: List[DerivationError] => String): DerivationResult[A] = this match {
      case DerivationResult.Success(_, _)      => this
      case DerivationResult.Failure(errors, _) => log(failure(errors))
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

    def unsafe[A](thunk: => A)(error: Throwable => DerivationError): DerivationResult[A] =
      try
        pure(thunk)
      catch {
        case e: Throwable => fail(error(e))
      }

    def sequence[A, Coll[A0] <: Seq[A0]](
      seq: Coll[DerivationResult[A]]
    )(implicit
      factory: Factory[A, Coll[A]]
    ): DerivationResult[Coll[A]] =
      seq.foldLeft(pure(factory.newBuilder))((builder, next) => builder.map2(next)(_.addOne(_))).map(_.result())

    def fromOption[A](opt: Option[A])(err: => DerivationError): DerivationResult[A] =
      opt.fold[DerivationResult[A]](fail(err))(pure)
    def fromEither[E, A](either: Either[E, A])(err: E => DerivationError): DerivationResult[A] =
      either.fold(e => fail(err(e)), pure)
  }

  def previewCode[A](code: CodeOf[A]): String

  def summonPipe[Input: Type, Output: Type]: DerivationResult[CodeOf[Pipe[Input, Output]]]

  /** If we pass Single Abstract Method as argument, after expansion inference sometimes fails, compiler might need a
    * hint
    */
  def singleAbstractMethodExpansion[SAM](tpe: Type[SAM], code: CodeOf[SAM]): CodeOf[SAM]

  def readConfig(
    code: CodeOf[PipeDerivationConfig[Pipe, In, Out]]
  ): DerivationResult[Settings]

  final def readSettingsIfGiven(
    code: Option[CodeOf[PipeDerivationConfig[Pipe, In, Out]]]
  ): DerivationResult[Settings] =
    code
      .fold(DerivationResult.pure(new Settings(Nil)))(readConfig)
      .log(if (code.isDefined) "Derivation started with configuration" else "Derivation started without configuration")
      .log(s"Pipeline from $inType to $outType")
      .log(s"PipeDerivation used: ${previewCode(pipeDerivation)}")
      .logSuccess(config => s"Configuration used: $config")
}
