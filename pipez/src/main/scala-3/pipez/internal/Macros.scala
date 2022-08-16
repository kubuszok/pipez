package pipez.internal

import pipez.{ PipeDerivation, PipeDerivationConfig }

import scala.quoted.{ Type as _, * }

@scala.annotation.experimental // due to Quotes.reflect.Symbol.typeRef usage
class MacrosImpl[Pipe[_, _], In, Out](q: Quotes)(
  pd:                                    Expr[PipeDerivation[Pipe]],
  override val pipeTypeConstructor:      scala.quoted.Type[Pipe],
  override val inType:                   scala.quoted.Type[In],
  override val outType:                  scala.quoted.Type[Out]
) extends PlatformDefinitions[Pipe, In, Out](using q)
    with PlatformGenerators[Pipe, In, Out] {

  import quotes.*
  import quotes.reflect.*

  def pipeType[I: Type, O: Type]: Type[Pipe[I, O]] = TypeRepr
    .of(using pipeTypeConstructor)
    .appliedTo(List(TypeRepr.of[I], TypeRepr.of[O]))
    .asType
    .asInstanceOf[Type[Pipe[I, O]]]

  val pipeDerivation: CodeOf[
    PipeDerivation[Pipe] { type Context = ArbitraryContext; type Result[O] = ArbitraryResult[O] }
  ] = pd.asInstanceOf[CodeOf[
    PipeDerivation[Pipe] { type Context = ArbitraryContext; type Result[O] = ArbitraryResult[O] }
  ]]

  // Scala 3 specific definitions
  val contextType: scala.quoted.Type[ArbitraryContext] = typeOf[Any].asInstanceOf[scala.quoted.Type[ArbitraryContext]]
  val resultType:  scala.quoted.Type[ArbitraryResult]  = typeOf[Any].asInstanceOf[scala.quoted.Type[ArbitraryResult]]
}

@scala.annotation.experimental // due to Quotes.reflect.Symbol.typeRef usage
object Macros {

  import scala.quoted.Type

  private def macros[Pipe[_, _], In, Out](
    pd:      Expr[PipeDerivation[Pipe]]
  )(using q: Quotes, pipeTypeConstructor: Type[Pipe], inType: Type[In], outType: Type[Out]): MacrosImpl[Pipe, In, Out] =
    new MacrosImpl[Pipe, In, Out](q)(pd, pipeTypeConstructor, inType, outType)

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
