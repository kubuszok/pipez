package pipez.internal

import pipez.{ PipeDerivation, PipeDerivationConfig }

import scala.annotation.nowarn
import scala.reflect.macros.blackbox

@nowarn("msg=The outer reference in this type test cannot be checked at run time.")
final class Macros(val c: blackbox.Context) extends PlatformDefinitions with PlatformDispatchers {

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
  ): c.Expr[Pipe[In, Out]] =
    (for {
      settings <- readConfigIfGiven(configurationCode)
      configuration = Configuration[Pipe, In, Out](
        inType = weakTypeTag[In].tpe,
        outType = weakTypeTag[Out].tpe,
        settings = settings,
        pipeDerivation = pipeDerivationCode
      )
      generator <- resolveConversion(configuration)
      expr <- generator.generate
    } yield expr) match {
      case DerivationResult.Success(value)  => value
      case DerivationResult.Failure(errors) => c.abort(c.enclosingPosition, errors.mkString(", "))
    }
}
