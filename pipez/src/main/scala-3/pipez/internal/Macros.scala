package pipez.internal

import pipez.{ PipeDerivation, PipeDerivationConfig }

import scala.quoted.*

class MacrosImpl[Pipe[_, _], In, Out](using Quotes)
    extends PlatformDefinitions[Pipe, In, Out]
    with PlatformGenerators[Pipe, In, Out] {

  def pipeType[I, O](i: Type[I], o: Type[O]): Type[Pipe[I, O]] = ???
  val inType:                                 Type[In]         = ???
  val outType:                                Type[Out]        = ???

  val pipeDerivation: CodeOf[PipeDerivation[Pipe]] = ???
}

object Macros {

  private def macros[Pipe[_, _], In, Out](using Quotes): MacrosImpl[Pipe, In, Out] =
    new MacrosImpl[Pipe, In, Out]

  def deriveDefault[Pipe[_, _], In, Out](using Quotes): Expr[Pipe[In, Out]] = macros[Pipe, In, Out].deriveDefault

  def deriveConfigured[Pipe[_, _], In, Out](
    config: Expr[PipeDerivationConfig[Pipe, In, Out]]
  )(using Quotes): Expr[Pipe[In, Out]] = macros[Pipe, In, Out].deriveConfigured(config)
}
