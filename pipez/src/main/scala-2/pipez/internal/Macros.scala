package pipez.internal

import pipez.{ PipeDerivation, PipeDerivationConfig }

import scala.annotation.nowarn
import scala.reflect.macros.blackbox

@nowarn("msg=The outer reference in this type test cannot be checked at run time.")
final class Macros(val c: blackbox.Context)
    extends PlatformDefinitions
    with PlatformGenerators
    with PlatformProductCaseGeneration
    with PlatformSumCaseGeneration {

  import c.universe._

  def deriveDefault[Pipe[_, _], In: WeakTypeTag, Out: WeakTypeTag](
    pd: c.Expr[PipeDerivation[Pipe]]
  ): c.Expr[Pipe[In, Out]] =
    derive[Pipe, In, Out](None, pd)

  def deriveConfigured[Pipe[_, _], In: WeakTypeTag, Out: WeakTypeTag](
    config: c.Expr[PipeDerivationConfig[Pipe, In, Out]]
  )(
    pd: c.Expr[PipeDerivation[Pipe]]
  ): c.Expr[Pipe[In, Out]] =
    derive[Pipe, In, Out](Some(config), pd)

  def derive[Pipe[_, _], In: WeakTypeTag, Out: WeakTypeTag](
    configurationCode:  Option[c.Expr[PipeDerivationConfig[Pipe, In, Out]]],
    pipeDerivationCode: c.Expr[PipeDerivation[Pipe]]
  ): c.Expr[Pipe[In, Out]] = {
    val result = for {
      settings <- readSettingsIfGiven(configurationCode)
      configuration = Configuration[Pipe, In, Out](
        inType = weakTypeTag[In].tpe,
        outType = weakTypeTag[Out].tpe,
        settings = settings,
        pipeDerivation = pipeDerivationCode
      )
      expr <- resolveConversion(configuration)
    } yield expr

    println("Macro diagnostics")
    println(result.diagnostic.mkString("\n"))

    result.fold(identity)(errors => c.abort(c.enclosingPosition, errors.mkString(", ")))
  }
}
