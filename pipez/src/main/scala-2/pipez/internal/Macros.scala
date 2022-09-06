package pipez.internal

import pipez.{ PipeDerivation, PipeDerivationConfig }
import pipez.internal.Definitions.{ Context, Result }

import scala.annotation.nowarn
import scala.reflect.macros.{ TypecheckException, blackbox }

// The way we are dispatching things here is one giant workaround for https://github.com/scala/bug/issues/5712

@nowarn("msg=The outer reference in this type test cannot be checked at run time.")
final class MacrosImpl[Pipe[_, _], In, Out](val c: blackbox.Context)(
  pipeTpe:                                         blackbox.Context#Type,
  inTpe:                                           blackbox.Context#Type,
  outTpe:                                          blackbox.Context#Type,
  pd:                                              blackbox.Context#Expr[PipeDerivation[Pipe]]
) extends PlatformDefinitions[Pipe, In, Out]
    with PlatformGenerators[Pipe, In, Out] {

  override val In:  Type[In]  = inTpe.asInstanceOf[Type[In]]
  override val Out: Type[Out] = outTpe.asInstanceOf[Type[Out]]

  def PipeOf[I: Type, O: Type]: Type[Pipe[I, O]] =
    c.universe
      .appliedType(pipeTpe.typeSymbol.asInstanceOf[c.Symbol], typeOf[I], typeOf[O])
      .asInstanceOf[Type[Pipe[I, O]]]

  import c.universe.*

  val pipeDerivation: CodeOf[PipeDerivation.Aux[Pipe, Context, Result]] =
    pd.asInstanceOf[CodeOf[PipeDerivation.Aux[Pipe, Context, Result]]]
  val previewPipeDerivation: String = previewCode(pipeDerivation)
}

final class Macro(val c: blackbox.Context) {

  import c.universe.*

  type ConstructorWeakTypeTag[F[_, _]] = WeakTypeTag[F[Any, Nothing]]

  private def macros[Pipe[_, _]: ConstructorWeakTypeTag, In: WeakTypeTag, Out: WeakTypeTag](
    pipeDerivation: c.Expr[PipeDerivation[Pipe]]
  ) = new MacrosImpl[Pipe, In, Out](
    c
  )(
    pipeTpe = c.weakTypeOf[Pipe[Any, Nothing]].typeConstructor,
    inTpe = c.weakTypeOf[In],
    outTpe = c.weakTypeOf[Out],
    pd = pipeDerivation
  )

  // I messed up something with the types of generated Trees - shape is almost identical to the last working version
  // before I started working on Scala 3 and making adjustments, but without this, it produces "type mismatch error".
  private def fixTypes[Pipe[_, _]: ConstructorWeakTypeTag, Out](
    expr:           blackbox.Context#Expr[Out],
    pipeDerivation: c.Expr[PipeDerivation[Pipe]]
  ): c.Expr[Out] = try
    c.Expr[Out](c.typecheck(tree = c.untypecheck(expr.tree.asInstanceOf[c.Tree])))
  catch {
    case TypecheckException(_, msg) if msg.startsWith("stable identifier required") =>
      val tc = c.weakTypeOf[Pipe[Any, Nothing]].typeConstructor
      val pd = showCode(pipeDerivation.tree)
      c.abort(c.enclosingPosition, s"Macro requires the implicit PipeDerivation[$tc] to be a stable val, given $pd")
  }

  /** Called with `macro pipez.internal.Macro.deriveDefault[Pipe, In, Out]` */
  def deriveDefault[Pipe[_, _]: ConstructorWeakTypeTag, In: WeakTypeTag, Out: WeakTypeTag](
    pipeDerivation: c.Expr[PipeDerivation[Pipe]]
  ): c.Expr[Pipe[In, Out]] = {
    val m = macros[Pipe, In, Out](pipeDerivation)
    fixTypes(m.deriveDefault, pipeDerivation)
  }

  /** Called with `macro pipez.internal.Macro.deriveConfigured[Pipe, In, Out]` */
  def deriveConfigured[Pipe[_, _]: ConstructorWeakTypeTag, In: WeakTypeTag, Out: WeakTypeTag](
    config: c.Expr[PipeDerivationConfig[Pipe, In, Out]]
  )(
    pipeDerivation: c.Expr[PipeDerivation[Pipe]]
  ): c.Expr[Pipe[In, Out]] = {
    val m = macros[Pipe, In, Out](pipeDerivation)
    fixTypes(m.deriveConfigured(config.asInstanceOf[m.c.Expr[PipeDerivationConfig[Pipe, In, Out]]]), pipeDerivation)
  }
}
