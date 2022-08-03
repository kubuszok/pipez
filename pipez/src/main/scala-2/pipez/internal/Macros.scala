package pipez.internal

import pipez.{ PipeDerivation, PipeDerivationConfig }

import scala.annotation.nowarn
import scala.reflect.macros.blackbox
import scala.util.chaining.scalaUtilChainingOps

@nowarn("msg=The outer reference in this type test cannot be checked at run time.")
final class Macros[Pipe[_, _], In, Out](val c: blackbox.Context)(
  inTpe:                                       blackbox.Context#Type,
  outTpe:                                      blackbox.Context#Type,
  pd:                                          blackbox.Context#Expr[PipeDerivation[Pipe]]
) extends PlatformDefinitions[Pipe, In, Out]
    with PlatformGenerators[Pipe, In, Out]
    with PlatformProductCaseGeneration[Pipe, In, Out]
    with PlatformSumCaseGeneration[Pipe, In, Out] {

  val inType:  Type[In]  = inTpe.asInstanceOf[Type[In]]
  val outType: Type[Out] = outTpe.asInstanceOf[Type[Out]]

  val pipeDerivation: CodeOf[PipeDerivation[Pipe]] = pd.asInstanceOf[CodeOf[PipeDerivation[Pipe]]]

  import c.universe._

  private def reportDiagnostics[A](result: DerivationResult[A]): Unit =
    c.echo(c.enclosingPosition, diagnosticsMessage(result))

  private def reportError(errors: List[DerivationError]): Nothing = c.abort(c.enclosingPosition, errorMessage(errors))

  def derive(
    configurationCode: Option[c.Expr[PipeDerivationConfig[Pipe, In, Out]]]
  )(implicit In:       WeakTypeTag[In], Out: WeakTypeTag[Out]): c.Expr[Pipe[In, Out]] =
    readSettingsIfGiven(configurationCode)
      .flatMap { config =>
        val result = resolveConversion(config)
        if (config.isDiagnosticsEnabled) {
          reportDiagnostics(result)
        }
        result
      }
      .fold(identity)(reportError)
}

final class MacroDispatcher(val c: blackbox.Context) {

  import c.universe._

  def deriveDefault[Pipe[_, _], In: WeakTypeTag, Out: WeakTypeTag](
    pd: c.Expr[PipeDerivation[Pipe]]
  ): c.Expr[Pipe[In, Out]] = {
    val macros = new Macros[Pipe, In, Out](c)(c.weakTypeOf[In], c.weakTypeOf[Out], pd)
    macros.derive(None).asInstanceOf[c.Expr[Pipe[In, Out]]]
  }

  def deriveConfigured[Pipe[_, _], In: WeakTypeTag, Out: WeakTypeTag](
    config: c.Expr[PipeDerivationConfig[Pipe, In, Out]]
  )(
    pd: c.Expr[PipeDerivation[Pipe]]
  ): c.Expr[Pipe[In, Out]] = {
    val macros = new Macros[Pipe, In, Out](c)(c.weakTypeOf[In], c.weakTypeOf[Out], pd)
    macros
      .derive(Some(config.asInstanceOf[macros.c.Expr[PipeDerivationConfig[Pipe, In, Out]]]))
      .asInstanceOf[c.Expr[Pipe[In, Out]]]
  }
}
