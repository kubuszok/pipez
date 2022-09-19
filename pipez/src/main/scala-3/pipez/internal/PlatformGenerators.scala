package pipez.internal

import pipez.PipeDerivation
import pipez.internal.Definitions.{ Context, Result }

import scala.quoted.{ Type as _, * }

trait PlatformGenerators[Pipe[_, _], In, Out]
    extends Generators[Pipe, In, Out]
    with PlatformProductCaseGeneration[Pipe, In, Out]
    with PlatformSumCaseGeneration[Pipe, In, Out] { self: PlatformDefinitions[Pipe, In, Out] =>

  import quotes.*
  import quotes.reflect.*

  final def isSubtype[A: Type, B: Type]: Boolean =
    TypeRepr.of[A] <:< TypeRepr.of[B]

  final def reportDiagnostics[A](result: DerivationResult[A]): Unit =
    report.info(diagnosticsMessage(result))

  final def reportError(errors: List[DerivationError]): Nothing =
    report.errorAndAbort(errorMessage(errors))

  final def lift[I: Type, O: Type](
    call: Expr[(I, Context) => Result[O]]
  ): Expr[Pipe[I, O]] = '{ ${ pipeDerivation }.lift(${ call }) }

  final def unlift[I: Type, O: Type](
    pipe: Expr[Pipe[I, O]],
    in:   Expr[I],
    ctx:  Expr[Context]
  ): Expr[Result[O]] = '{ ${ pipeDerivation }.unlift(${ pipe }, ${ in }, ${ ctx }) }

  final def updateContext(
    ctx:  Expr[Context],
    path: Expr[pipez.Path]
  ): Expr[Context] = '{ ${ pipeDerivation }.updateContext(${ ctx }, ${ path }) }

  final def pureResult[A: Type](a: Expr[A]): Expr[Result[A]] = '{ ${ pipeDerivation }.pureResult(${ a }) }

  final def mergeResults[A: Type, B: Type, C: Type](
    ctx: Expr[Context],
    ra:  Expr[Result[A]],
    rb:  Expr[Result[B]],
    f:   Expr[(A, B) => C]
  ): Expr[Result[C]] = '{ ${ pipeDerivation }.mergeResults(${ ctx }, ${ ra }, ${ rb }, ${ f }) }
}
