package pipez.internal

import pipez.{ PipeDerivation, PipeDerivationConfig }

import scala.annotation.nowarn
import scala.reflect.macros.blackbox
import scala.util.chaining.scalaUtilChainingOps

@nowarn("msg=The outer reference in this type test cannot be checked at run time.")
final class Macros[Pipe[_, _], In, Out](val c: blackbox.Context)(
  pipeTpe:                                     blackbox.Context#Type,
  inTpe:                                       blackbox.Context#Type,
  outTpe:                                      blackbox.Context#Type,
  pd:                                          blackbox.Context#Expr[PipeDerivation[Pipe]]
) extends PlatformDefinitions[Pipe, In, Out]
    with PlatformGenerators[Pipe, In, Out]
    with PlatformProductCaseGeneration[Pipe, In, Out]
    with PlatformSumCaseGeneration[Pipe, In, Out] {

  def pipeType[I, O](i: Type[I], o: Type[O]): Type[Pipe[I, O]] =
    c.universe
      .appliedType(pipeTpe.typeSymbol.asInstanceOf[c.Symbol], i.asInstanceOf[c.Type], o.asInstanceOf[c.Type])
      .asInstanceOf[Type[Pipe[I, O]]]
  val inType:  Type[In]  = inTpe.asInstanceOf[Type[In]]
  val outType: Type[Out] = outTpe.asInstanceOf[Type[Out]]

  import c.universe._

  val pipeDerivation: CodeOf[PipeDerivation[Pipe]] = pd.asInstanceOf[CodeOf[PipeDerivation[Pipe]]]

  private def reportDiagnostics[A](result: DerivationResult[A]): Unit =
    c.echo(c.enclosingPosition, diagnosticsMessage(result))

  private def reportError(errors: List[DerivationError]): Nothing = c.abort(c.enclosingPosition, errorMessage(errors))

  def derive(configurationCode: Option[c.Expr[PipeDerivationConfig[Pipe, In, Out]]]): c.Expr[Pipe[In, Out]] = {
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
}

final class MacroDispatcher(val c: blackbox.Context) {

  import c.universe._

  type ConstructorWeakTypeTag[F[_, _]] = WeakTypeTag[F[Any, Nothing]]

  def deriveDefault[Pipe[_, _]: ConstructorWeakTypeTag, In: WeakTypeTag, Out: WeakTypeTag](
    pd: c.Expr[PipeDerivation[Pipe]]
  ): c.Expr[Pipe[In, Out]] = {
    val macros = new Macros[Pipe, In, Out](c)(
      pipeTpe = c.weakTypeOf[Pipe[Any, Nothing]].typeConstructor,
      inTpe = c.weakTypeOf[In],
      outTpe = c.weakTypeOf[Out],
      pd = pd
    )
    macros.derive(None).asInstanceOf[c.Expr[Pipe[In, Out]]]
  }

  def deriveConfigured[Pipe[_, _]: ConstructorWeakTypeTag, In: WeakTypeTag, Out: WeakTypeTag](
    config: c.Expr[PipeDerivationConfig[Pipe, In, Out]]
  )(
    pd: c.Expr[PipeDerivation[Pipe]]
  ): c.Expr[Pipe[In, Out]] = {
    val macros = new Macros[Pipe, In, Out](c)(
      pipeTpe = c.weakTypeOf[Pipe[Any, Nothing]].typeConstructor,
      inTpe = c.weakTypeOf[In],
      outTpe = c.weakTypeOf[Out],
      pd = pd
    )
    macros
      .derive(Some(config.asInstanceOf[macros.c.Expr[PipeDerivationConfig[Pipe, In, Out]]]))
      .asInstanceOf[c.Expr[Pipe[In, Out]]]
  }
}
