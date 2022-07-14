package pipez

/** Allows derivation macro to glue Pipes for `Pipe[in.field, out.field] for each field or `Pipe[In.Subtype,
  * Out.Subtype]` for each subtype together.
  *
  * Assumes that `Pipe[In, Out]` is interchangeable to `(in: In, ctx: Context) => Result[Out]` where:
  *   - `Context` is anything we want to thread through our calls (like in `ReaderT`)
  *   - `Result[Out]` is any effect we need: `Either[Error, Out]`, `IO[Out]`
  *
  * If there is no need to thread anything you might define your `Context` to `Unit` and if you don't need an effect on
  * returned value you might define `Result[Out] = Out`.
  *
  * @tparam Pipe
  *   typeclass with two type parameters, used like `Pipe[In, Out]`, which is most likely a Single Abstract Method
  *   interface, implementing some `(In, Context) => Result[Out]` function, which could be automatically generated for
  *   `InProduct => OutProduct` or `InSumType => OutSumType` by pipeing the corresponding fields or sum type elements by
  *   their name and combining the result together
  */
trait PipeDerivation[Pipe[_, _]] {

  /** Type of value you want to thread through all calls */
  type Context

  /** Type of value you want to return after the call */
  type Result[Out]

  /** Turns a function into your `Pipe` typeclass */
  def lift[In, Out](f: (In, Context) => Result[Out]): Pipe[In, Out]

  /** Calls `Pipe` as if it was a function */
  def unlift[In, Out](pipe: Pipe[In, Out])(in: In, ctx: Context): Result[Out]

  /** Let you inject int information about current Path (extracted field, matched subtypes) into `Context` */
  def updateContext(context: Context, path: Path): Context

  /** Wraps raw value into `Result` that `Pipe` should return */
  def pureResult[A](a: A): Result[A]

  /** Combines 2 `Results` into 1 */
  def mergeResults[A, B, C](ra: Result[A], rb: => Result[B])(f: (A, B) => C): Result[C]
}
object PipeDerivation extends PipeDerivationPlatform {

  type Aux[Pipe[_, _], Context0, Result0[_]] = PipeDerivation[Pipe] {
    type Context     = Context0
    type Result[Out] = Result0[Out]
  }

  /** Specialization for `Pipe`s which are interchangeable to `In => Result[Out]` */
  trait NoContext[Pipe[_, _]] extends PipeDerivation[Pipe] {

    final type Context = Unit

    /** Turns a function without `Context` into `Pipe` */
    def simpleLift[In, Out](f: In => Result[Out]): Pipe[In, Out]
    final def lift[In, Out](f: (In, Context) => Result[Out]): Pipe[In, Out] = simpleLift(in => f(in, ()))

    /** Calls `Pipe` as if it was a function without `Context` */
    def simpleUnlift[In, Out](pipe: Pipe[In, Out])(in: In): Result[Out]
    final def unlift[In, Out](pipe: Pipe[In, Out])(in: In, ctx: Context): Result[Out] = simpleUnlift(pipe)(in)

    final def updateContext(context: Context, path: Path): Context = context
  }

  /** Specialization for `Pipe`s which are interchangeable to `(In, Context) => Out` */
  trait NoParsing[Pipe[_, _]] extends PipeDerivation[Pipe] {

    final type Result[Out] = Out

    final def pureResult[A](a: A): Result[A] = a

    final def mergeResults[A, B, C](ra: Result[A], rb: => Result[B])(f: (A, B) => C): Result[C] = f(ra, rb)
  }

  /** Specialization for `Pipe`s which are interchangeable to `In => Out`` */
  trait Simple[Pipe[_, _]] extends NoContext[Pipe] with NoParsing[Pipe]
}
