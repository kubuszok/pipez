package pipez.internal

import pipez.PipeDerivation

import scala.annotation.nowarn

@nowarn("msg=The outer reference in this type test cannot be checked at run time.")
trait Generators[Pipe[_, _], In, Out]
    extends ProductCaseGeneration[Pipe, In, Out]
    with SumCaseGeneration[Pipe, In, Out] { self: Definitions[Pipe, In, Out] =>

  trait CodeGeneratorExtractor {

    def unapply(settings: Settings): Option[DerivationResult[CodeOf[Pipe[In, Out]]]]
  }

  val resolveConversion: Settings => DerivationResult[CodeOf[Pipe[In, Out]]] = {
    case ProductTypeConversion(generator) => generator
    case SumTypeConversion(generator)     => generator
    case _ =>
      DerivationResult.fail(DerivationError.NotYetSupported) // TODO: better error message
  }

  def diagnosticsMessage[A](result: DerivationResult[A]): String =
    "Macro diagnostics\n" + result.diagnostic.map(" - " + _.toString).mkString("\n")

  def errorMessage(errors: List[DerivationError]): String = "Pipe couldn't be generated due to errors:\n" + errors
    .map {
      case DerivationError.MissingPublicConstructor =>
        s"$outType is missing a public constructor that could be used to initiate its value"
      case DerivationError.RequiredImplicitNotFound(inFieldType, outFieldType) =>
        s"Couldn't find implicit of type ${pipeType(inFieldType, outFieldType)}"
      case DerivationError.MissingPublicSource(outFieldName) =>
        s"Couldn't find a field/method which could be used as a source for $outFieldName from $outType; use config to provide it manually"
      case DerivationError.NotSupportedConversion(inField, inFieldType, outField, outFieldType) =>
        s"Couldn't find an implicit value converting $inFieldType to $outFieldType, required by $inType.$inField to $outType.$outField conversion; provide the right implicit or configuration"
      case DerivationError.NotYetSupported =>
        s"Your setup is valid, but the library doesn't support it yet; if you think it's a bug contact library authors"
      case DerivationError.InvalidConfiguration(msg) =>
        s"The configuration you provided was incorrect: $msg"
      case DerivationError.NotYetImplemented(msg) =>
        s"The functionality \"$msg\" is not yet implemented, this message is intended as diagnostic for library authors and you shouldn't have seen it"
    }
    .map(" - " + _)
    .mkString("\n")

  /** Can be used instead of pd.Context to avoid path-dependent types */
  type ArbitraryContext

  /** Can be used instead of pd.Context to avoid path-dependent types */
  type ArbitraryResult[Out]

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
}
