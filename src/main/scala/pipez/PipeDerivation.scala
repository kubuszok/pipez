package pipez

trait PipeDerivation[Pipe[_, _]] {

  type Context
  type Result[Out]

  //  TODO: type ShouldUpdateContext <: Boolean
  //  TODO: type FieldsCaseSensitive <: Boolean
  //  TODO: type SubtypesCaseSensitive <: Boolean
  //  TODO: type JavaBeans <: Boolean

  def lift[In, Out](f: (In, Context) => Result[Out]): Pipe[In, Out]

  def unlift[In, Out](pipe: Pipe[In, Out])(in: In, ctx: Context): Result[Out]

  def updateContext(context: Context, path: Path): Context

  def pureResult[A](a: A): Result[A]

  def mergeResults[A, B, C](ra: Result[A], rb: => Result[B])(
      f: (A, B) => C
  ): Result[C]

  // TODO: traverse?

  // TODO: consider putting derive and derive configured here?
}
object PipeDerivation {

  type Aux[Pipe[_, _], Context0, Result0[_]] = PipeDerivation[Pipe] {
    type Context = Context0
    type Result[Out] = Result0[Out]
  }

  def derive[Pipe[_, _], In, Out](config: PipeDerivationConfig[Pipe, In, Out] = PipeDerivationConfig[Pipe, In, Out])(
      implicit pd: PipeDerivation[Pipe]
  ): Pipe[In, Out] = ??? // TODO: macro
}
