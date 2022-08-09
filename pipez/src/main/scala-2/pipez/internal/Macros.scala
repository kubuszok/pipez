package pipez.internal

import pipez.{ PipeDerivation, PipeDerivationConfig }

import scala.annotation.nowarn
import scala.reflect.macros.blackbox

// The way we are dispatching things here is one giant workaround for https://github.com/scala/bug/issues/5712

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

  import c.universe.*

  val pipeDerivation: CodeOf[PipeDerivation[Pipe]] = pd.asInstanceOf[CodeOf[PipeDerivation[Pipe]]]
}

final class MacroDispatcher(val c: blackbox.Context) {

  import c.universe.*

  type ConstructorWeakTypeTag[F[_, _]] = WeakTypeTag[F[Any, Nothing]]

  private def macros[Pipe[_, _]: ConstructorWeakTypeTag, In: WeakTypeTag, Out: WeakTypeTag](
    pd: c.Expr[PipeDerivation[Pipe]]
  ) = new Macros[Pipe, In, Out](c)(
    pipeTpe = c.weakTypeOf[Pipe[Any, Nothing]].typeConstructor,
    inTpe = c.weakTypeOf[In],
    outTpe = c.weakTypeOf[Out],
    pd = pd
  )

  def deriveDefault[Pipe[_, _]: ConstructorWeakTypeTag, In: WeakTypeTag, Out: WeakTypeTag](
    pd: c.Expr[PipeDerivation[Pipe]]
  ): c.Expr[Pipe[In, Out]] = {
    val m = macros[Pipe, In, Out](pd)
    m.deriveDefault
  }.asInstanceOf[c.Expr[Pipe[In, Out]]]

  def deriveConfigured[Pipe[_, _]: ConstructorWeakTypeTag, In: WeakTypeTag, Out: WeakTypeTag](
    config: c.Expr[PipeDerivationConfig[Pipe, In, Out]]
  )(
    pd: c.Expr[PipeDerivation[Pipe]]
  ): c.Expr[Pipe[In, Out]] = {
    val m = macros[Pipe, In, Out](pd)
    m.deriveConfigured(config.asInstanceOf[m.c.Expr[PipeDerivationConfig[Pipe, In, Out]]])
  }.asInstanceOf[c.Expr[Pipe[In, Out]]]
}
