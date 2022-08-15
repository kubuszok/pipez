package pipez.internal

import pipez.{ PipeDerivation, PipeDerivationConfig }

import scala.quoted.*

class MacrosImpl[Pipe[_, _], In, Out](
  pd:                       Expr[PipeDerivation[Pipe]]
)(using Quotes)(using Pipe: Type[Pipe], In: Type[In], Out: Type[Out])
    extends PlatformDefinitions[Pipe, In, Out]
    with PlatformGenerators[Pipe, In, Out] {

  import quotes.*
  import quotes.reflect.*

  def pipeType[I: Type, O: Type]: Type[Pipe[I, O]] =
    TypeRepr.of[Pipe].appliedTo(List(TypeRepr.of[I], TypeRepr.of[O])).asType.asInstanceOf[Type[Pipe[I, O]]]
  val inType:  Type[In]  = In
  val outType: Type[Out] = Out

  val pipeDerivation: CodeOf[
    PipeDerivation[Pipe] { type Context = ArbitraryContext; type Result[O] = ArbitraryResult[O] }
  ] = pd.asInstanceOf[CodeOf[
    PipeDerivation[Pipe] { type Context = ArbitraryContext; type Result[O] = ArbitraryResult[O] }
  ]]

  // Scala 3 specific definitions
  val pipeTypeConstructor = Pipe
  val contextType         = Type.of[Any].asInstanceOf[scala.quoted.Type[ArbitraryContext]]
  val resultType          = Type.of[Any].asInstanceOf[scala.quoted.Type[ArbitraryResult]]
}

object Macros {

  private def macros[Pipe[_, _], In, Out](
    pd: Expr[PipeDerivation[Pipe]]
  )(using Quotes, Type[Pipe], Type[In], Type[Out]): MacrosImpl[Pipe, In, Out] =
    new MacrosImpl[Pipe, In, Out](pd)

  def deriveDefault[Pipe[_, _], In, Out](
    pd: Expr[PipeDerivation[Pipe]]
  )(using Quotes, Type[Pipe], Type[In], Type[Out]): Expr[Pipe[In, Out]] = macros[Pipe, In, Out](pd).deriveDefault

  def deriveConfigured[Pipe[_, _], In, Out](
    config: Expr[PipeDerivationConfig[Pipe, In, Out]]
  )(
    pd: Expr[PipeDerivation[Pipe]]
  )(using Quotes, Type[Pipe], Type[In], Type[Out]): Expr[Pipe[In, Out]] =
    macros[Pipe, In, Out](pd).deriveConfigured(config)
}
