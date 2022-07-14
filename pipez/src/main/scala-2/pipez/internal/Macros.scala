package pipez.internal

import pipez.{ PipeDerivation, PipeDerivationConfig }

import scala.annotation.nowarn
import scala.reflect.macros.blackbox

@nowarn("msg=The outer reference in this type test cannot be checked at run time.")
final class Macros[Pipe[_, _], In, Out](val c: blackbox.Context)(
  inTpe:                                       blackbox.Context#Type,
  outTpe:                                      blackbox.Context#Type
) extends PlatformDefinitions[Pipe, In, Out]
    with PlatformGenerators[Pipe, In, Out]
    with PlatformProductCaseGeneration[Pipe, In, Out]
    with PlatformSumCaseGeneration[Pipe, In, Out] {

  val inType:  Type[In]  = inTpe.asInstanceOf[Type[In]]
  val outType: Type[Out] = inTpe.asInstanceOf[Type[Out]]

  import c.universe._

  def derive(
    configurationCode:  Option[c.Expr[PipeDerivationConfig[Pipe, In, Out]]],
    pipeDerivationCode: c.Expr[PipeDerivation[Pipe]]
  )(implicit In:        WeakTypeTag[In], Out: WeakTypeTag[Out]): c.Expr[Pipe[In, Out]] = {
    val result = for {
      settings <- readSettingsIfGiven(configurationCode)
      configuration = Configuration(
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

final class MacroDispatcher(val c: blackbox.Context) {

  import c.universe._

  def deriveDefault[Pipe[_, _], In: WeakTypeTag, Out: WeakTypeTag](
    pd: c.Expr[PipeDerivation[Pipe]]
  ): c.Expr[Pipe[In, Out]] = {
    val macros = new Macros[Pipe, In, Out](c)(c.weakTypeOf[In], c.weakTypeOf[Out])
    macros.derive(None, pd.asInstanceOf[macros.c.Expr[PipeDerivation[Pipe]]]).asInstanceOf[c.Expr[Pipe[In, Out]]]
  }

  def deriveConfigured[Pipe[_, _], In: WeakTypeTag, Out: WeakTypeTag](
    config: c.Expr[PipeDerivationConfig[Pipe, In, Out]]
  )(
    pd: c.Expr[PipeDerivation[Pipe]]
  ): c.Expr[Pipe[In, Out]] = {
    val macros = new Macros[Pipe, In, Out](c)(c.weakTypeOf[In], c.weakTypeOf[Out])
    macros
      .derive(
        Some(config.asInstanceOf[macros.c.Expr[PipeDerivationConfig[Pipe, In, Out]]]),
        pd.asInstanceOf[macros.c.Expr[PipeDerivation[Pipe]]]
      )
      .asInstanceOf[c.Expr[Pipe[In, Out]]]
  }
}
