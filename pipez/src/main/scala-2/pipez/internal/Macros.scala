package pipez.internal

import pipez.{ PipeDerivation, PipeDerivationConfig }

import scala.annotation.nowarn
import scala.reflect.macros.blackbox

// The way we are dispatching things here is one giant workaround for https://github.com/scala/bug/issues/5712

@nowarn("msg=The outer reference in this type test cannot be checked at run time.")
final class MacrosImpl[Pipe[_, _], In, Out](val c: blackbox.Context)(
  pipeTpe:                                         blackbox.Context#Type,
  inTpe:                                           blackbox.Context#Type,
  outTpe:                                          blackbox.Context#Type,
  pd:                                              blackbox.Context#Expr[PipeDerivation[Pipe]]
) extends PlatformDefinitions[Pipe, In, Out]
    with PlatformGenerators[Pipe, In, Out] {

  def PipeOf[I, O](i: Type[I], o: Type[O]): Type[Pipe[I, O]] =
    c.universe
      .appliedType(pipeTpe.typeSymbol.asInstanceOf[c.Symbol], i.asInstanceOf[c.Type], o.asInstanceOf[c.Type])
      .asInstanceOf[Type[Pipe[I, O]]]
  val In:  Type[In]  = inTpe.asInstanceOf[Type[In]]
  val Out: Type[Out] = outTpe.asInstanceOf[Type[Out]]

  import c.universe.*

  val pipeDerivation: CodeOf[PipeDerivation[Pipe] { type Context = self.Context; type Result[O] = self.Result[O] }] =
    pd.asInstanceOf[CodeOf[PipeDerivation[Pipe] { type Context = self.Context; type Result[O] = self.Result[O] }]]
}

final class Macro(val c: blackbox.Context) {

  import c.universe.*

  type ConstructorWeakTypeTag[F[_, _]] = WeakTypeTag[F[Any, Nothing]]

  private def macros[Pipe[_, _]: ConstructorWeakTypeTag, In: WeakTypeTag, Out: WeakTypeTag](
                                                                                             pipeDerivation: c.Expr[PipeDerivation[Pipe]]
  ) = new MacrosImpl[Pipe, In, Out](c)(
    pipeTpe = c.weakTypeOf[Pipe[Any, Nothing]].typeConstructor,
    inTpe = c.weakTypeOf[In],
    outTpe = c.weakTypeOf[Out],
    pd = pipeDerivation
  )

  /** Called with `macro pipez.internal.Macro.deriveDefault[Pipe, In, Out]` */
  def deriveDefault[Pipe[_, _]: ConstructorWeakTypeTag, In: WeakTypeTag, Out: WeakTypeTag](
    pipeDerivation: c.Expr[PipeDerivation[Pipe]]
  ): c.Expr[Pipe[In, Out]] = {
    val m = macros[Pipe, In, Out](pipeDerivation)
    m.deriveDefault
  }.asInstanceOf[c.Expr[Pipe[In, Out]]]

  /** Called with `macro pipez.internal.Macro.deriveConfigured[Pipe, In, Out]` */
  def deriveConfigured[Pipe[_, _]: ConstructorWeakTypeTag, In: WeakTypeTag, Out: WeakTypeTag](
    config: c.Expr[PipeDerivationConfig[Pipe, In, Out]]
  )(
    pipeDerivation: c.Expr[PipeDerivation[Pipe]]
  ): c.Expr[Pipe[In, Out]] = {
    val m = macros[Pipe, In, Out](pipeDerivation)
    m.deriveConfigured(config.asInstanceOf[m.c.Expr[PipeDerivationConfig[Pipe, In, Out]]])
  }.asInstanceOf[c.Expr[Pipe[In, Out]]]
}
