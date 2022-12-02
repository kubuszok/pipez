package pipez.internal

import pipez.PipeDerivationConfig
import pipez.internal.Definitions.{ Context, Result }

import scala.annotation.nowarn
import scala.util.chaining.*

@nowarn("msg=The outer reference in this type test cannot be checked at run time.")
private[internal] trait Generators[Pipe[_, _], In, Out]
    extends AnyValCaseGeneration[Pipe, In, Out]
    with ProductCaseGeneration[Pipe, In, Out]
    with SumCaseGeneration[Pipe, In, Out] {
  self: Definitions[Pipe, In, Out] =>

  /** Check is `A <:< B` in a platform-independent code */
  def isSubtype[A: Type, B: Type]: Boolean

  /** Takes `Settings` and passes them to generators, the first which decides it's their case, attempt generation */
  final val resolveConversion: Settings => DerivationResult[Expr[Pipe[In, Out]]] = {
    case AnyValConversion(generatedCode)      => generatedCode
    case ProductTypeConversion(generatedCode) => generatedCode
    case SumTypeConversion(generatedCode)     => generatedCode
    case _                                    => DerivationResult.fail(DerivationError.NotYetSupported)
  }

  /** Generate message to be displayed by macro on INFO level (if requested by config) */
  final def diagnosticsMessage[A](result: DerivationResult[A]): String =
    "Macro diagnostics:\n" + result.diagnostic.map(" - " + _).mkString("\n")

  /** Should use platform-specific way of reporting information from macro on INFO level */
  def reportDiagnostics[A](result: DerivationResult[A]): Unit

  /** Generates error message to be returned from macro on ERROR level */
  final def errorMessage(errors: List[DerivationError]): String = {
    val pipeType = previewType(PipeOf[In, Out])
    def generateErrorsFor(inType: String, outType: String, errors: List[DerivationError]): List[String] = errors
      .map {
        case DerivationError.MissingPublicConstructor =>
          s"$outType is missing a public constructor that could be used to initiate its value"
        case DerivationError.RequiredImplicitNotFound(inFieldType, outFieldType) =>
          s"Couldn't find implicit of type ${previewType(PipeOf(inFieldType, outFieldType))}"
        case DerivationError.RecursiveDerivationFailed(recInType, recOutType, errors) =>
          s"Couldn't derive instance of type ${previewType(PipeOf(recInType, recOutType))} due to errors:\n" +
            generateErrorsFor(previewType(recInType), previewType(recOutType), errors).map("  " + _).mkString("\n")
        case DerivationError.MissingPublicSource(outFieldName) =>
          s"Couldn't find a field/method which could be used as a source for $outFieldName from $outType; use config to provide it manually"
        case DerivationError.MissingMatchingSubType(inSubtypeType) =>
          s"Couldn't find corresponding subtype for $inSubtypeType}"
        case DerivationError.MissingMatchingValue(inValue) =>
          s"Couldn't find corresponding value for $inValue"
        case DerivationError.NotSupportedFieldConversion(inField, inFieldType, outField, outFieldType) =>
          s"Couldn't find an implicit value converting ${previewType(inFieldType)} to ${previewType(outFieldType)}, required by $inType.$inField to $outType.$outField conversion; provide the right implicit or configuration"
        case DerivationError.NotSupportedEnumConversion(isInSumType, isOutSumType) =>
          s"Couldn't convert $inType (${if (isInSumType) "sum type" else "value enumeration"}) into $outType (${
              if (isOutSumType) "sum type" else "value enumeration"
            })"
        case DerivationError.InvalidConfiguration(msg) =>
          s"The configuration you provided was incorrect: $msg"
        case DerivationError.InvalidInput(msg) =>
          msg
        case DerivationError.NotYetSupported =>
          s"Derivation is only supported for conversions: case class/Java Bean <=> case class/Java Bean, case class <=> tuple, ADT <=> ADT, AnyVal <=> primitives - types $inType => $outType don't match this requirement, consider providing $pipeType yourself"
        case DerivationError.NotYetImplemented(msg) =>
          s"The functionality \"$msg\" is not yet implemented, this message is intended as diagnostic for library authors and you shouldn't have seen it"
      }
      .map(" - " + _)
    s"$pipeType couldn't be generated due to errors:\n" + generateErrorsFor(previewType[In], previewType[Out], errors)
      .mkString("\n")
  }

  /** Should use platform-specific way of reporting errors from macro */
  def reportError(errors: List[DerivationError]): Nothing

  /** Should generate code `pipeDerivation.lift { (in, ctx) => ... }` */
  def lift[I: Type, O: Type](
    call: Expr[(I, Context) => Result[O]]
  ): Expr[Pipe[I, O]]

  /** Should generate code `pipeDerivation.unlift(pipe)(in, ctx)` */
  def unlift[I: Type, O: Type](
    pipe: Expr[Pipe[I, O]],
    in:   Expr[I],
    ctx:  Expr[Context]
  ): Expr[Result[O]]

  /** Should generate code `pipeDerivation.updateContext(ctx, path)` */
  def updateContext(
    context: Expr[Context],
    path:    Expr[pipez.Path]
  ): Expr[Context]

  /** Should generate code `pipeDerivation.pureResult(a)` */
  def pureResult[A: Type](a: Expr[A]): Expr[Result[A]]

  /** Should generate code `pipeDerivation.mergeResults(ctx, ra, rb, (a, b) => ...)` */
  def mergeResults[A: Type, B: Type, C: Type](
    context: Expr[Context],
    ra:      Expr[Result[A]],
    rb:      Expr[Result[B]],
    f:       Expr[(A, B) => C]
  ): Expr[Result[C]]

  /** Used by `derive(Option[Expr[PipeDerivationConfig[Pipe, In, Out]]])` and `derivePipe[Input: Type, Output: Type]` */
  def derive(config: Settings): DerivationResult[Expr[Pipe[In, Out]]] = {
    val isDiagnosticsEnabled = config.isDiagnosticsEnabled
    lazy val startTime       = java.time.Instant.now()
    if (isDiagnosticsEnabled) {
      startTime.hashCode
    }
    val result = resolveConversion(config)
    if (isDiagnosticsEnabled) {
      val stopTime = java.time.Instant.now()
      val duration = java.time.Duration.between(startTime, stopTime)
      result.log(f"Derivation took ${duration.getSeconds}%d.${duration.getNano}%09d s")
    } else result
  }

  private def derive(configurationCode: Option[Expr[PipeDerivationConfig[Pipe, In, Out]]]): Expr[Pipe[In, Out]] = {
    var isDiagnosticsEnabled = false
    readSettingsIfGiven(configurationCode)
      .flatMap { config =>
        isDiagnosticsEnabled = config.isDiagnosticsEnabled
        derive(config)
      }
      .tap { result =>
        if (isDiagnosticsEnabled) {
          reportDiagnostics(result)
        }
      }
      .fold(identity)(reportError)
  }

  /** Derives using default `Settings` */
  final def deriveDefault: Expr[Pipe[In, Out]] = derive(None)

  /** Derives using `Settings` parsed from `PipeDerivationConfig[Pipe, In, Out]` expression */
  final def deriveConfigured(configurationCode: Expr[PipeDerivationConfig[Pipe, In, Out]]): Expr[Pipe[In, Out]] =
    derive(Some(configurationCode))
}
