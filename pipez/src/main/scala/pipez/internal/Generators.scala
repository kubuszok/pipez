package pipez.internal

import pipez.PipeDerivationConfig

import scala.annotation.nowarn
import scala.util.chaining.scalaUtilChainingOps

@nowarn("msg=The outer reference in this type test cannot be checked at run time.")
trait Generators[Pipe[_, _], In, Out] extends ProductCaseGeneration[Pipe, In, Out] with SumCaseGeneration[Pipe, In, Out]{
  self: Definitions[Pipe, In, Out]  =>

  def isSubtype[A, B](lower: Type[A], higher: Type[B]): Boolean

  trait CodeGeneratorExtractor {

    def unapply(settings: Settings): Option[DerivationResult[CodeOf[Pipe[In, Out]]]]
  }

  final val resolveConversion: Settings => DerivationResult[CodeOf[Pipe[In, Out]]] = {
    case ProductTypeConversion(generator) => generator
    case SumTypeConversion(generator)     => generator
    case _ =>
      DerivationResult.fail(DerivationError.NotYetSupported) // TODO: better error message
  }

  final def diagnosticsMessage[A](result: DerivationResult[A]): String =
    "Macro diagnostics\n" + result.diagnostic.map(" - " + _).mkString("\n")

  /** Should use platform-specific way of reporting information from macro */
  def reportDiagnostics[A](result: DerivationResult[A]): Unit

  final def errorMessage(errors: List[DerivationError]): String = "Pipe couldn't be generated due to errors:\n" + errors
    .map {
      case DerivationError.MissingPublicConstructor =>
        s"$outType is missing a public constructor that could be used to initiate its value"
      case DerivationError.RequiredImplicitNotFound(inFieldType, outFieldType) =>
        s"Couldn't find implicit of type ${pipeType(inFieldType, outFieldType)}"
      case DerivationError.MissingPublicSource(outFieldName) =>
        s"Couldn't find a field/method which could be used as a source for $outFieldName from $outType; use config to provide it manually"
      case DerivationError.MissingMatchingSubType(inSubtypeType) =>
        s"Couldn't find corresponding subtype for $inSubtypeType"
      case DerivationError.MissingMatchingValue(inValue) =>
        s"Couldn't find corresponding value for $inValue"
      case DerivationError.NotSupportedFieldConversion(inField, inFieldType, outField, outFieldType) =>
        s"Couldn't find an implicit value converting $inFieldType to $outFieldType, required by $inType.$inField to $outType.$outField conversion; provide the right implicit or configuration"
      case DerivationError.NotSupportedEnumConversion(isInSumType, isOutSumType) =>
        s"Couldn't convert $inType (${if (isInSumType) "sum type" else "value enumeration"}) into ${outType} (${
            if (isOutSumType) "sum type" else "value enumeration"
          })"
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

  /** Can be used instead of pd.Context to avoid path-dependent types */
  type ArbitraryContext

  /** Can be used instead of pd.Context to avoid path-dependent types */
  type ArbitraryResult[O]

  /** Should generate code `pd.lift { (in, ctx) => ... }` */
  def lift[I, O](
    call: CodeOf[(I, ArbitraryContext) => ArbitraryResult[O]]
  ): CodeOf[Pipe[I, O]]

  /** Should generate code `pd.unlift(pipe)(in, ctx)` */
  def unlift[I, O](
    pipe: CodeOf[Pipe[I, O]],
    in:   CodeOf[I],
    ctx:  Argument[ArbitraryContext]
  ): CodeOf[ArbitraryResult[O]]

  /** Should generate code `pd.updateContext(ctx, path)` */
  def updateContext(
    context: Argument[ArbitraryContext],
    path:    CodeOf[Path]
  ): CodeOf[ArbitraryContext]

  /** Should generate code `pd.pureResult(a)` */
  def pureResult[A](a: CodeOf[A]): CodeOf[ArbitraryResult[A]]

  /** Should generate code `pd.mergeResults(ra, rb) { (a, b) => ... }` */
  def mergeResults[A, B, C](
    ra: CodeOf[ArbitraryResult[A]],
    rb: CodeOf[ArbitraryResult[B]],
    f:  CodeOf[(A, B) => C]
  ): CodeOf[ArbitraryResult[C]]

  final def derive(configurationCode: Option[CodeOf[PipeDerivationConfig[Pipe, In, Out]]]): CodeOf[Pipe[In, Out]] = {
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

  final def deriveDefault: CodeOf[Pipe[In, Out]] = derive(None)

  final def deriveConfigured(configurationCode: CodeOf[PipeDerivationConfig[Pipe, In, Out]]): CodeOf[Pipe[In, Out]] =
    derive(Some(configurationCode))
}
