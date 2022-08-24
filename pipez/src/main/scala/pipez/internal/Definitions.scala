package pipez.internal

import pipez.{ PipeDerivation, PipeDerivationConfig }

import scala.annotation.nowarn
import scala.collection.Factory

/** Contains definitions that could be shared between Scala 2 and Scala 3 implementation of the derivation logic, which
  * aren't strictly related to code parsing and generation.
  *
  * @tparam Pipe
  *   the type class that we are deriving for
  * @tparam In
  *   input type of the type class we are deriving
  * @tparam Out
  *   output type of the type class we are deriving
  */
@nowarn("msg=The outer reference in this type test cannot be checked at run time.")
trait Definitions[Pipe[_, _], In, Out] { self =>

  /** Platform-specific type representation (c.universe.Type in 2, scala.quoted.Type[A] in 3) */
  type Type[A]

  /** Platform-specific expression representation (c.universe.Expr[A] in 2, quotes.Expr[A] in 3 */
  type CodeOf[A]

  /** Summons Type independently of the platform */
  def typeOf[A](implicit tpe: Type[A]): Type[A] = tpe

  /** Provides `Type` instance for `Pipe[I, O]` */
  implicit def PipeOf[I: Type, O: Type]: Type[Pipe[I, O]]

  /** Prints type value */
  def previewType[A: Type]: String

  /** Provides `Type` instance for `In` */
  implicit val In: Type[In]

  /** Provides `Type` instance for `Out` */
  implicit val Out: Type[Out]

  /** Value of `PipeDerivation[Pipe]`, which was passed to macro as (most likely) implicit */
  val pipeDerivation: CodeOf[PipeDerivation.Aux[Pipe, Definitions.Context, Definitions.Result]]

  /** Type representing how we got the specific value from the `in: In` argument */
  sealed trait Path extends Product with Serializable
  object Path {

    case object Root extends Path
    final case class Field(from: Path, name: String) extends Path
    final case class Subtype[A](from: Path, tpe: Type[A]) extends Path
    final case class AtIndex(from: Path, index: Int) extends Path // might not be needed
    final case class AtKey(from: Path, key: String) extends Path // might not be needed
  }

  /** Possible configuration options that we can work with inside a macro. Parsed from `pipez.PipeDerivationConfig` */
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

  /** Collection of config options obtained after parsing `pipez.PipeDerivationConfig` */
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

  /** Possible errors that can happen during derivation */
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

  /** Helper which allows: use of for-comprehension, parallel error composition, logging */
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

  /** Allows displaying the generated code in platform-independent way */
  def previewCode[A](code: CodeOf[A]): String

  /** Allows summoning the type class in platform-independent way */
  def summonPipe[Input: Type, Output: Type]: DerivationResult[CodeOf[Pipe[Input, Output]]]

  /** If we pass Single Abstract Method as argument, after expansion inference sometimes fails, compiler might need a
    * hint
    */
  def singleAbstractMethodExpansion[SAM: Type](code: CodeOf[SAM]): CodeOf[SAM]

  /** Turns the code defining `PipeDerivationConfig[Pipe, In, Out]` into `Settings`.
    *
    * Requires that config is created as one chain while passing the parameter.
    */
  def readConfig(code: CodeOf[PipeDerivationConfig[Pipe, In, Out]]): DerivationResult[Settings]

  /** Reads configs if passed, or fallback to defaults (empty `Settings`) otherwise */
  final def readSettingsIfGiven(code: Option[CodeOf[PipeDerivationConfig[Pipe, In, Out]]]): DerivationResult[Settings] =
    code
      .fold(DerivationResult.pure(new Settings(Nil)))(readConfig)
      .log(if (code.isDefined) "Derivation started with configuration" else "Derivation started without configuration")
      .log(s"Pipeline from ${previewType[In]} to ${previewType[Out]}")
      .log(s"PipeDerivation used: ${previewCode(pipeDerivation)}")
      .logSuccess(config => s"Configuration used: $config")
}
object Definitions {

  /** Can be used instead of pipeDerivation.Context to avoid path-dependent types */
  type Context

  /** Can be used instead of pipeDerivation.Result[O] to avoid path-dependent types */
  type Result[O]
}
