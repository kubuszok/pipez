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

## TL;DR for nerds

Pipez derives `F[In, Out]` where `F[I, O]` is a profunctor isomorphic to `(I, Ctx) => G[O]` (for some `Ctx` and `G[_]`
specific to particular `F`), by combining profunctor instances for corresponding elements of product/sum types `I` and
`O`.

## Tutorial

None at the moment, check [tests](pipez/src/test/scala/pipez).
