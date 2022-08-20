package pipez.internal

import pipez.{ PipeDerivation, PipeDerivationConfig }

import scala.quoted.{ Type as _, * }

// The way we are dispatching things here is a workaround that methods expanded in `inline` should be `object`

@scala.annotation.experimental // due to Quotes.reflect.Symbol.typeRef usage
class MacrosImpl[Pipe[_, _], In, Out](q: Quotes)(
  pd:                                    Expr[PipeDerivation[Pipe]],
  override val Pipe:                     scala.quoted.Type[Pipe],
  override val In:                       scala.quoted.Type[In],
  override val Out:                      scala.quoted.Type[Out]
) extends PlatformDefinitions[Pipe, In, Out](using q)
    with PlatformGenerators[Pipe, In, Out] { self =>

  import quotes.*
  import quotes.reflect.*

  def PipeOf[I: Type, O: Type]: Type[Pipe[I, O]] =
    TypeRepr.of(using Pipe).appliedTo(List(TypeRepr.of[I], TypeRepr.of[O])).asType.asInstanceOf[Type[Pipe[I, O]]]

  val pipeDerivation: CodeOf[PipeDerivation[Pipe] { type Context = self.Context; type Result[O] = self.Result[O] }] =
    pd.asInstanceOf[CodeOf[PipeDerivation[Pipe] { type Context = self.Context; type Result[O] = self.Result[O] }]]

  // Scala 3-macro specific instances, required because code-generation needs these types

  implicit val Context: scala.quoted.Type[Context] = {
    given p: scala.quoted.Type[Pipe] = Pipe
//    println('{
//      val ppp = ${ pd }
//      ??? : ppp.Context
//    }.asTerm)

    import scala.util.chaining.*

    println(pd.asTerm)

    Select(pd.asTerm, TypeRepr.of[PipeDerivation].typeSymbol.declaredType("Context").head)
      .tap(println)
      .symbol
      .tap(println)
      .pipe(TypeTree.ref)
      .tpe
      .tap(println)
      .asType
      .asInstanceOf[scala.quoted.Type[Context]]
      .tap(println)

    // val result = TypeRepr.of[PipeDerivation].typeSymbol.declaredType("Context").head
    // Select(pd.asTerm, result).symbol.typeRef.asType.asInstanceOf[scala.quoted.Type[Context]]
  }
  implicit val Result: scala.quoted.Type[Result] = {
    // val result = TypeRepr.of[PipeDerivation].typeSymbol.declaredType("Result").head
    // Select(pd.asTerm, result).symbol.typeRef.asType.asInstanceOf[scala.quoted.Type[Result]]

    // pd.asTerm.tpe.typeSymbol.declaredType("Result").head.typeRef.asType.asInstanceOf[scala.quoted.Type[Result]]

    import scala.util.chaining.*

    Select(pd.asTerm, TypeRepr.of[PipeDerivation].typeSymbol.declaredType("Result").head).symbol
      .pipe(TypeTree.ref)
      .tpe.asType
      .asInstanceOf[scala.quoted.Type[Result]]
  }
  println(TypeRepr.of[Context])
  println(TypeRepr.of[Result])
}

@scala.annotation.experimental // due to Quotes.reflect.Symbol.typeRef usage
object Macros {

  import scala.quoted.Type

  private def macros[Pipe[_, _], In, Out](
    pd:      Expr[PipeDerivation[Pipe]]
  )(using q: Quotes, pipeTypeConstructor: Type[Pipe], In: Type[In], Out: Type[Out]): MacrosImpl[Pipe, In, Out] =
    new MacrosImpl[Pipe, In, Out](q)(pd, pipeTypeConstructor, In, Out)

  /** Called with `${ pipez.internal.Macro.deriveDefault[Pipe, In, Out]('pd) }` */
  def deriveDefault[Pipe[_, _], In, Out](
    pd: Expr[PipeDerivation[Pipe]]
  )(using Quotes, Type[Pipe], Type[In], Type[Out]): Expr[Pipe[In, Out]] = macros[Pipe, In, Out](pd).deriveDefault

  /** Called with `macro pipez.internal.Macro.deriveConfigured[Pipe, In, Out]('config)('pd)` */
  def deriveConfigured[Pipe[_, _], In, Out](
    config: Expr[PipeDerivationConfig[Pipe, In, Out]]
  )(
    pd: Expr[PipeDerivation[Pipe]]
  )(using Quotes, Type[Pipe], Type[In], Type[Out]): Expr[Pipe[In, Out]] =
    macros[Pipe, In, Out](pd).deriveConfigured(config)
}
