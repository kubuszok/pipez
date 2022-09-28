package pipez.internal

import pipez.internal.Definitions.{ Context, Result }

private[internal] trait PlatformGenerators[Pipe[_, _], In, Out]
    extends Generators[Pipe, In, Out]
    with PlatformAnyValCaseGeneration[Pipe, In, Out]
    with PlatformProductCaseGeneration[Pipe, In, Out]
    with PlatformSumCaseGeneration[Pipe, In, Out] {
  self: PlatformDefinitions[Pipe, In, Out] =>

  import c.universe.*

  final def isSubtype[A: Type, B: Type]: Boolean =
    typeOf[A] <:< typeOf[B]

  final def reportDiagnostics[A](result: DerivationResult[A]): Unit =
    c.echo(c.enclosingPosition, diagnosticsMessage(result))

  final def reportError(errors: List[DerivationError]): Nothing =
    c.abort(c.enclosingPosition, errorMessage(errors))

  final def lift[I: Type, O: Type](
    call: Expr[(I, Context) => Result[O]]
  ): Expr[Pipe[I, O]] = c.Expr[Pipe[I, O]](q"""$pipeDerivation.lift($call)""")

  final def unlift[I: Type, O: Type](
    pipe: Expr[Pipe[I, O]],
    in:   Expr[I],
    ctx:  Expr[Context]
  ): Expr[Result[O]] = c.Expr[Result[O]](q"""$pipeDerivation.unlift($pipe, $in, $ctx)""")

  final def updateContext(
    ctx:  Expr[Context],
    path: Expr[pipez.Path]
  ): Expr[Context] = c.Expr[Context](q"""$pipeDerivation.updateContext($ctx, $path)""")

  final def pureResult[A: Type](a: Expr[A]): Expr[Result[A]] =
    c.Expr[Result[A]](q"""$pipeDerivation.pureResult($a)""")

  final def mergeResults[A: Type, B: Type, C: Type](
    ctx: Expr[Context],
    ra:  Expr[Result[A]],
    rb:  Expr[Result[B]],
    f:   Expr[(A, B) => C]
  ): Expr[Result[C]] = c.Expr[Result[C]](q"""$pipeDerivation.mergeResults($ctx, $ra, $rb, $f)""")

  private val garbage = Set(
    // product methods
    "productElementName",
    "productIterator",
    "canEqual",
    "productElement",
    "productArity",
    "productPrefix",
    "productElementNames",
    // object methods
    "synchronized",
    "wait",
    "equals",
    "hashCode",
    "getClass",
    "asInstanceOf",
    "isInstanceOf"
  )
  protected val isGarbage: Symbol => Boolean = s => garbage(s.name.toString)

  /** Applies type arguments obtained from tpe to the type parameters in method's parameters' types */
  protected val paramListsOf = (tpe: c.Type, method: c.Symbol) => method.asMethod.typeSignatureIn(tpe).paramLists

  /** Applies type arguments obtained from tpe to the type parameters in method's return type */
  protected val returnTypeOf = (tpe: c.Type, method: c.Symbol) => method.typeSignatureIn(tpe).finalResultType
}
