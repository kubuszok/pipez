# Pipez

Library oriented about deriving (generating by type) functions:
* `In => Out`
* `In => F[Out]`
* `(In, Ctx) => Out`
* `(In, Ctx) => F[Out]`

as well as all type classes that could be converted to/from such functions.

## Motivation

Idea for Pipez was born as a side effect of research about possible ways of migrating
[Chimney](https://github.com/scalalandio/chimney) to Scala 3. Research assumed that:
* code would have to shared between Scala 2 and 3 to make sure that core logic is the same in both versions
* macro logic would have to be heavily refactored and probably simplified to make it more approachable to new
  contributors
* emitted code should still be reasonably fast

Early attempts shown that Chimney is too big it make such experiments comfortably, and it would be easier to make
a separate project which would find some solutions to the issues above and then backport them.

Such project could be similar to Chimney in some mechanics but not necessarily in projects goals.

## Pipez vs Chimney

Chimney focuses on giving user the best out-of-the-box developer experience:
* it provides DSL to transform value in-place without providing any custom definitions unless necessary
* it supports operations on _native_ types like `Option`, collections
* it doesn't require defining custom types to make transformation possible
* it has options like using default values to provide missing fields, providing pure values, generating pure values
  from transformed object,
* for validated transformation it can provide a path to the failed field, showing: fields, subtypes, sequence index or
  map key or value that led to failure

Pipez on the other hand is targeting library maintainers:
* it doesn't let you run it against the value, if you want it, you should configure that yourself
* it doesn't provide any support for `Option`s, collections, maps, etc - it assumes that user can provide the necessary
  implicits themselves
* it only let provide user function/type class in the same shape as a way of handling added fields/removed
  subtypes/renames
* it is intended however to let user inject the path to the currently transformed value as some value passed next to
  the input
* in other words, it only focuses on generating `(In, Ctx) => Result[Out]` functions (or equivalent type classes)
  out of implicitly acquired functions `(In2, Ctx) => Result[Out2]` mapping each field/subtype `In2` in `In`
  to a corresponding field/subtype `Out2` in `Out` (transformations are not necessary if `In2 >: Out2`)
* how functions/type classes are combined is defined with an implicit implementation of `PipezDerivation[F]`

## Tutorial for nerds

Pipez derives `F[In, Out]` where `F[I, O]` is a profunctor isomorphic to `(I, Context) => Result[O]` (for some `Context`
and `Result[_]` specific to particular `F`), by combining profunctor instances for corresponding elements of product/sum
types `I` and `O`.

`Context` and `Result` are defined as path-dependent parameters within driver `PipeDerivation[F]`. User is free
to define some useful `Context` (and carry it around akin to Reader monad) or set it to `Unit`.

Library assumes that results embedded in `Result[_]` can be combined using logic similar to `cats.Applicative` /
`cats.NonEmptyParallel` - it is up to library's User, to provide the behavior for merging results, so the library does
not assume whether `mergeResult` behaves like `map2` or `parMap2`.  User is also free to define `type Result[A] = A` and
get rid of effects, making `F` an alias/wrapper around `(In, Context) => Out` function.

```scala
// example of a typeclass with isn't using Context, but which is using Result
trait Codec[A, B] {
  def decode(value: A): Either[String, B]
}

implicit val codecDerivation: PipeDerivation[Codec] = new PipeDerivation[Codec] {
  type Contex = Unit
  type Result[A] = Either[String, A]
  // these 2 assume that you can always convert function to type class and you can always
  // call the type class providing argument for the function that created it
  def lift[A, B](f: (A, Context) => Result[Out]): Codec[A, B] = f(_, ())
  def unlift[A, B](codec: Codec[A, B], a: A, ctx: Context): Result[B] = codec.decode(a)
  // will be used when the value can be created without any conversion
  def pure[A](a: A): Result[A] = Right(a)
  // here we decided to implement map2 logic
  def mergeResult[A, B, C](fa: Result[A], fb: => Result[B], (A, B) => C): Result[C] =
    for { a <- fa; b <- fb } yield f(a, b)
  // TODO: not yet implemented
  def updateContext(context: Context, path: Path) = context
}
```

### Product types

For product types, unless overridden by user using config, library will pair each output field with an input field by
their name:

```scala
case class In(a: Int, b: Double)
case class Out(a: Int, b: String)

implicit val doubleToString: Codec[Double, String] = ...

PipeDerivation.derive[In, Out]
```

would derive something similar to:

```scala
codecDerivation.lift { (in: In, ctx: codecDerivation.Context) =>
  codecDerivation.mergeResult(
    codecDerivation.pure(in.a), // types match, no need to transform
    codecDerivation.unlift(doubleToString, in.b, ctx), // type don't match, converting
    (a: Int, b: String) => Out(a, b) // combining results together
  )
} // Codec[In, Out]
```

(_exact_ details are _not_ part of any contract, user should only assume that library would create `Results` with `pure`
or `unlift` and somehow combine them with `mergeResult` to get `Result[Out]`).

If the library cannot find a corresponding field in `In` for some field in `Out`, or if field types mismatch and there
is no implicit conversion, explicit configuration have to be passed or compilation will fail.

### Sum types

For sum types, unless overridden by user using config, library will pair each input subtype with an output subtype by
their name:

```scala
sealed trait In
object In {
  case class A(int: Int) extends In
  case class B(double: Int) extends In
}

sealed trait Out
object Out {
  case class A(int: Int) extends In
  case class B(double: Int) extends In
}

import PipeDerivation.Auto.* // to allow recursive derivation of Codec[In.A, Out.A] and Codec[In.B, Out.B]
PipeDerivation.derive[In, Out]
```

would generate something like:

```scala
codecDerivation.lift { (in: In, ctx: codecDerivation.Context) =>
  in match {
    case arg : In.A => codecDerivation(derivedForA : Codec[In.A, Out.A], arg, ctx)
    case arg : In.B => codecDerivation(derivedForB : Codec[In.B, Out.B], arg, ctx)
  }
} // Codec[In, Out]
```

(same as for product types user should just assume that argument will be delegated to the right subtype codec).

### Using and updating context

not yet implemented

## Examples

Check [tests](pipez/src/test/scala/pipez).
