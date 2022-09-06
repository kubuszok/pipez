# Pipez

Library oriented about deriving (generating by type) functions:
* `In => Out`
* `In => F[Out]`
* `(In, Ctx) => Out`
* `(In, Ctx) => F[Out]`

as well as all type classes that could be converted to/from such functions.

Example:

```scala
// When turning Foo to Bar, field a has to be converted and field b can be copied
case class Foo(a: Double, b: Int)
case class Bar(a: String, b: Int)

// How to convert Double to String in the world of _ => _
implicit val doubleToString1: Double => String = _.toString
// _ => _ works out of the box:
val derived1: Foo => Bar = PipeDerivation.derive[_ => _, Foo, Bar]
println(derived1(Foo(1.0, 2)))

case class Ctx(doubleFormat: String)
// How to convert Double to String in the world of (_, Ctx) => _
implicit val doubleToString1: (Double, Ctx) => String = (d, ctx) => ctx.doubleFormat.format(d)
// (_, Ctx) => _ requires a bit of help:
implicit val ctxDerivation: PipeDerivation[(_, Ctx) => _] = PipeDerivation.contextFunction[Ctx]()
val derived2: (Foo, Ctx) => Bar = PipeDerivation.derive[(_, Ctx) => _, Foo, Bar]
println(derived2(Foo(1.0, 2), Ctx("%.2f")))

// Derivation of type classes or functions returning F[Out] require writing PipeDerivation instance
```

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
  type Context = Unit
  type Result[A] = Either[String, A]
  // these 2 assume that you can always convert function to type class and you can always
  // call the type class providing argument for the function that created it
  def lift[A, B](f: (A, Context) => Result[Out]): Codec[A, B] = f(_, ())
  def unlift[A, B](codec: Codec[A, B], a: A, ctx: Context): Result[B] = codec.decode(a)
  // will be used when the value can be created without any conversion
  def pure[A](a: A): Result[A] = Right(a)
  // here we decided to implement map2 logic
  def mergeResult[A, B, C](ctx: Context, fa: Result[A], fb: => Result[B], f: (A, B) => C): Result[C] =
    for { a <- fa; b <- fb } yield f(a, b)
  // can be used to update Context with Field/Subtype information before passing it into codec
  def updateContext(context: Context, path: Path) = context
}
```

### Product types

For product types, unless overridden by user using config, library will pair each output field with an input field by
their name:

```scala
case class In(a: Int, b: Double)
case class Out(a: Int, b: String)

implicit val doubleToString: Codec[Double, String] = d => Right(d.toString)

PipeDerivation.derive[Codec, In, Out]
```

would derive something similar to:

```scala
codecDerivation.lift { (in: In, ctx: codecDerivation.Context) =>
  codecDerivation.mergeResult(
    ctx,
    codecDerivation.pure(in.a), // types match, no need to transform
    codecDerivation.unlift(doubleToString, in.b, codecDerivation.updateContext(ctx, Path.root.field("b"))), // type don't match, converting
    (a: Int, b: String) => Out(a, b) // combining results together
  )
} // Codec[In, Out]
```

(_exact_ details are _not_ part of any contract, user should only assume that library would create `Results` with `pure`
or `unlift` and somehow combine them with `mergeResult` to get `Result[Out]`).

If the library cannot find a corresponding field in `In` for some field in `Out`, or if field types mismatch and there
is no implicit conversion, explicit configuration have to be passed or compilation will fail.

```scala
PipeDerivation.derive(
  PipezDerivationConfig[Codec, In, Out]
    // output has field with no corresponding field in input - provide In => Out2 conversion
    .addField(_.out : Out2, codec: Codec[In, Out2])
    // rename fields, if needed fetch conversion from implicit scope
    .renameField(_.in, _.out)
    // wire input field to output field and provide codec manually
    .plugInField(_.in: In2, _.out: Out2, codec: Codec[In2, Out2])
    // don't match names in case sensitive way
    .fieldMatchingCaseInsensitive
)
```

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
PipeDerivation.derive[Codec, In, Out]
```

would generate something like:

```scala
codecDerivation.lift { (in: In, ctx: codecDerivation.Context) =>
  in match {
    case arg : In.A => codecDerivation(derivedForA : Codec[In.A, Out.A], arg, codecDerivation.updateContext(ctx, Path.root.subtype("A")))
    case arg : In.B => codecDerivation(derivedForB : Codec[In.B, Out.B], arg, codecDerivation.updateContext(ctx, Path.root.subtype("B")))
  }
} // Codec[In, Out]
```

(same as for product types user should just assume that argument will be delegated to the right subtype codec).

In case there is no matching output subtype for input subtype, user should provide a config:

```scala
PipeDerivation.derive(
  PipezDerivationConfig[Codec, In, Out]
    // output is missing subtype corresponding to input subtype, provide In2 => Out conversion
    .removeSubtype[In2](codec: Codec[In2, Out])
    // rename subtype, fetch conversion from implicit scope
    .renameSubtype[In2, Out2]
    // wire subtypes, provide conversion manually
    .plugInSubtype[In2, Out2](codec: Codec[In2, Out2])
    // don't match subtypes in case sensitive way
    .enumMatchingCaseInsensitive
)
```

### Using and updating context

If `F[A, B]` needs some additional value to create the result, e.g. it might contain a path to the currently converted
field/subtype, so that it could be placed in failure message on failed decoding, `updateContext` will be used
to make sure that `ctx: pipeDerivation.Context` passed to the `unlift` is updated with information about current
field subtype.

`Context` isn't restricted to such cases though. One could define codec which has the ability to e.g. fail fast
like this:

```scala
trait Codec[A, B] {
  def decode(value: A, shouldFailFast: Boolean): Either[List[String], B]
}
```

Then derivation would pass this extra argument around unchanged if you defined it as:

```scala
implicit val pipeDerivation: PipeDerivation[Codec] = new PipeDerivation[Codec] {
  type Context = Boolean
  def updateContext(ctx: Context, path: Path): Context = ctx
  ...
}
```

In case a type class/function doesn't expect a tuple but more arguments or `In` is nested
in some wrapper, `lift` and `unlift` should perform the conversion `(In, Context) <=> Arguments`.

### Utilities

Instead of calling `PipeDerivation.derive[Codec, In, Out]` it would be nicer to be able to call
`Codec.derive[In, Out]`. This can be achieved through

```scala
object Codec extends PipeSemiautoSupport[Codec]
```

Similarly `PipeDerivation.derive(PipeDerivationConfig[Codec, In, Out]...)` could be replaced with
`Coded.derive(Codec.Config[In, Out]...)` with:

```scala
object Codec extends PipeSemiautoConfiguredSupport[Codec]
```

(these 2 are non mutually exclusive so one can `extends` both traits).

If the `derive` with default options should be an implicit to make it automatic one can

```scala
object Codec extends PipeAutoSupport[Codec] // automatic derivation is always in scope

// or

object Codec {
  object Auto extends PipeAutoSupport[Codec] // require import Codec.Auto.*
}
```

Instance of `PipeDerivation[F]` is best to define in companion object as well unless there could be several
possible implementations that user should pick manually at call site (either by providing implicit in scope or passing
the value manually to the macro).

Additionally, if you:

 * don't need Context (`type Context = Unit`), you can extend `PipeDerivation.NoContext[F]`
 * don't need Result (`type Result[A] = A`), you can extend `PipeDerivation.NoParsing[F]`
 * don't need any of the above, you can extend `PipeDerivation.Simple[F]`

instead of `PipeDerivation[F]`.

### Debugging

If you are not sure what is happening during macro expansion and what code it generated,
pass it a configuration with `.enableDiagnosics` option.

### Contracts and laws

What Pipez promises is that it:

 * will not use conversion if an input field type is a subtype of output field type
 * when conversion of a field/subtype will be performed, the library will provide instance (from summoning or config)
   and then use users code (`pipeDerivation.unlift`) to run it
 * partial results of conversions of fields will be combined through `pipeDerivation.mergeResult`

It does not however:

 * provide a way of handling `Option` types (e.g. creating `F[Option[A], Option[B]]` from `F[A, B]`), or collections
   (e.g. creating `F[Seq[A], List[B]]` from `F[A, B]`) - it is assumed that it is the responsibility of the user
 * guarantee that the results build with `PipeDerivation[F]` will be following some contracts like Cats/Cats Effect
   laws. It is up to the user to make sure that their implementation of `PipeDerivation[F]` will not violate any laws

In other words, the user implementing their type class (function) and its derivation is responsible for defining
the type class contracts and its laws. Pipez is responsible to make sure that calling this type class and building
the final result is done through user-provided methods. With that user should be able to determine whether derived
code with follow the laws as well.

This is the biggest difference against `TransformerF`s from Chimney, which were coming with some predefined assumptions
which made it difficult to establish what are the laws that `TransformerF` should follow, and how it could be modified
to not break user's code.

## Examples

Check [tests](pipez/src/test/scala/pipez).
