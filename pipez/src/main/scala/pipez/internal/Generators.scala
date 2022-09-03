package pipez.internal

import pipez.PipeDerivationConfig
import pipez.internal.Definitions.{ Context, Result }

import scala.annotation.nowarn
import scala.util.chaining.scalaUtilChainingOps

@nowarn("msg=The outer reference in this type test cannot be checked at run time.")
trait Generators[Pipe[_, _], In, Out]
    extends ProductCaseGeneration[Pipe, In, Out]
    with SumCaseGeneration[Pipe, In, Out] {
  self: Definitions[Pipe, In, Out] =>

  /** Check is `A <:< B` in a platform-independent code */
  def isSubtype[A: Type, B: Type]: Boolean

  /** Takes `Settings` and passes them to generators, the first which decides it's their case, attempt generation */
  final val resolveConversion: Settings => DerivationResult[CodeOf[Pipe[In, Out]]] = {
    case ProductTypeConversion(generatedCode) => generatedCode
    case SumTypeConversion(generatedCode)     => generatedCode
    case _ =>
      DerivationResult.fail(DerivationError.NotYetSupported) // TODO: better error message
  }

  /** Generate message to be displayed by macro on INFO level (if requested by config) */
  final def diagnosticsMessage[A](result: DerivationResult[A]): String =
    "Macro diagnostics:\n" + result.diagnostic.map(" - " + _).mkString("\n")

  /** Should use platform-specific way of reporting information from macro on INFO level */
  def reportDiagnostics[A](result: DerivationResult[A]): Unit

  /** Generates error message to be returned from macro on ERROR level */
  final def errorMessage(errors: List[DerivationError]): String = "Pipe couldn't be generated due to errors:\n" + errors
    .map {
      case DerivationError.MissingPublicConstructor =>
        s"${previewType[Out]} is missing a public constructor that could be used to initiate its value"
      case DerivationError.RequiredImplicitNotFound(inFieldType, outFieldType) =>
        s"Couldn't find implicit of type ${previewType(PipeOf(inFieldType, outFieldType))}"
      case DerivationError.MissingPublicSource(outFieldName) =>
        s"Couldn't find a field/method which could be used as a source for $outFieldName from ${previewType[Out]}; use config to provide it manually"
      case DerivationError.MissingMatchingSubType(inSubtypeType) =>
        s"Couldn't find corresponding subtype for $inSubtypeType"
      case DerivationError.MissingMatchingValue(inValue) =>
        s"Couldn't find corresponding value for $inValue"
      case DerivationError.NotSupportedFieldConversion(inField, inFieldType, outField, outFieldType) =>
        s"Couldn't find an implicit value converting $inFieldType to ${previewType(
            outFieldType
          )}, required by ${previewType[In]}.$inField to ${previewType[Out]}.$outField conversion; provide the right implicit or configuration"
      case DerivationError.NotSupportedEnumConversion(isInSumType, isOutSumType) =>
        s"Couldn't convert ${previewType[In]} (${
            if (isInSumType) "sum type" else "value enumeration"
          }) into ${previewType[Out]} (${if (isOutSumType) "sum type" else "value enumeration"})"
      case DerivationError.NotYetSupported =>
        s"Your setup is valid, but the library doesn't support it yet; if you think it's a bug contact library authors"
      case DerivationError.InvalidConfiguration(msg) =>
        s"The configuration you provided was incorrect: $msg"
      case DerivationError.NotYetImplemented(msg) =>
        s"The functionality \"$msg\" is not yet implemented, this message is intended as diagnostic for library authors and you shouldn't have seen it"
    }
    .map(" - " + _)
    .mkString("\n")

  /** Should use platform-specific way of reporting errors from macro */
  def reportError(errors: List[DerivationError]): Nothing

  /** Should generate code `pipeDerivation.lift { (in, ctx) => ... }` */
  def lift[I: Type, O: Type](
    call: CodeOf[(I, Context) => Result[O]]
  ): CodeOf[Pipe[I, O]]

  /** Should generate code `pipeDerivation.unlift(pipe)(in, ctx)` */
  def unlift[I: Type, O: Type](
    pipe: CodeOf[Pipe[I, O]],
    in:   CodeOf[I],
    ctx:  CodeOf[Context]
  ): CodeOf[Result[O]]

  /** Should generate code `pipeDerivation.updateContext(ctx, path)` */
  def updateContext(
    context: CodeOf[Context],
    path:    CodeOf[pipez.Path]
  ): CodeOf[Context]

  /** Should generate code `pipeDerivation.pureResult(a)` */
  def pureResult[A: Type](a: CodeOf[A]): CodeOf[Result[A]]

  /** Should generate code `pipeDerivation.mergeResults(ctx, ra, rb, (a, b) => ...)` */
  def mergeResults[A: Type, B: Type, C: Type](
    context: CodeOf[Context],
    ra:      CodeOf[Result[A]],
    rb:      CodeOf[Result[B]],
    f:       CodeOf[(A, B) => C]
  ): CodeOf[Result[C]]

  private def derive(configurationCode: Option[CodeOf[PipeDerivationConfig[Pipe, In, Out]]]): CodeOf[Pipe[In, Out]] = {
    var isDiagnosticsEnabled = false
    readSettingsIfGiven(configurationCode)
      .flatMap { config =>
        isDiagnosticsEnabled = config.isDiagnosticsEnabled
        lazy val startTime = java.time.Instant.now()
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
      .tap { result =>
        if (isDiagnosticsEnabled) {
          reportDiagnostics(result)
        }
      }
      .fold(identity)(reportError)
  }

  /** Derives using default `Settings` */
  final def deriveDefault: CodeOf[Pipe[In, Out]] = derive(None)

  /** Derives using `Settings` parsed from `PipeDerivationConfig[Pipe, In, Out]` expression */
  final def deriveConfigured(configurationCode: CodeOf[PipeDerivationConfig[Pipe, In, Out]]): CodeOf[Pipe[In, Out]] =
    derive(Some(configurationCode))
}
