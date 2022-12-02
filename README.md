# Pipez

[![Pipez JVM](https://index.scala-lang.org/mateuszkubuszok/pipez/pipez/latest-by-scala-version.svg?platform=jvm)](https://search.maven.org/artifact/com.kubuszok/pipez_2.13)
[![Pipez JS](https://index.scala-lang.org/mateuszkubuszok/pipez/pipez/latest-by-scala-version.svg?platform=sjs1)](https://search.maven.org/artifact/com.kubuszok/pipez_sjs1_2.13)
[![Pipez Native](https://index.scala-lang.org/mateuszkubuszok/pipez/pipez/latest-by-scala-version.svg?platform=native0.4)](https://search.maven.org/artifact/com.kubuszok/pipez_native0.4_3)

[![Scaladoc](https://javadoc.io/badge2/com.kubuszok/pipez_2.13/scaladoc%202.13.svg)](https://javadoc.io/doc/com.kubuszok/pipez_2.13)
[![Scaladoc](https://javadoc.io/badge2/com.kubuszok/pipez_3/scaladoc%203.svg)](https://javadoc.io/doc/com.kubuszok/pipez_3)
![CI build](https://github.com/MateuszKubuszok/pipez/workflows/CI%20build/badge.svg)
[![License](http://img.shields.io/:license-Apache%202-green.svg)](http://www.apache.org/licenses/LICENSE-2.0.txt)

Scala library for type-safe data-transformations, which allows you to build-in Chimney-like abilities to your own type classes and effects.

> Pipez is a result of research about possible ways of migrating Chimney to Scala 3. It focuses on a certain type class from Chimney- `TransformerF` - and while it attempts to replicate as much features as possible it is **not** intended to replace Chimney nor reimplement all of its features.

<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [Installation](#installation)
- [Motivating example](#motivating-example)
- [Custom parsers](#custom-parsers)
  - [Automatic derivation](#automatic-derivation)
  - [Semiautomatic derivation](#semiautomatic-derivation)
  - [Configured derivation](#configured-derivation)
- [Supported features and configuration options](#supported-features-and-configuration-options)
  - [Case classes and Java Beans conversions](#case-classes-and-java-beans-conversions)
    - [`addField` configuration](#addfield-configuration)
    - [`renameField` configuration](#renamefield-configuration)
    - [`plugInField` configuration](#pluginfield-configuration)
    - [`fieldMatchingCaseInsensitive` configuration](#fieldmatchingcaseinsensitive-configuration)
    - [`addFallbackToValue` configuration](#addfallbacktovalue-configuration)
    - [`enableFallbackToDefaults` configuration](#enablefallbacktodefaults-configuration)
    - [`recursiveDerivation` configuration](#recursivederivation-configuration)
  - [Tuples](#tuples)
  - [Sealed hierarchies and enums conversions](#sealed-hierarchies-and-enums-conversions)
    - [`removeSubtype` configuration](#removesubtype-configuration)
    - [`renameSubtype` configuration](#renamesubtype-configuration)
    - [`plugInSubtype` configuration](#pluginsubtype-configuration)
    - [`enumMatchingCaseInsensitive` configuration](#enummatchingcaseinsensitive-configuration)
  - [AnyVals conversions](#anyvals-conversions)
  - [Deriving instances for Scala 3 types in Scala 2.13 and vice-versa](#deriving-instances-for-scala-3-types-in-scala-213-and-vice-versa)
  - [Features you have to implement yourself](#features-you-have-to-implement-yourself)
- [How to define `PipeDerivation`](#how-to-define-pipederivation)
  - [Enriching Context with path to value](#enriching-context-with-path-to-value)
  - [Debugging](#debugging)
  - [Contracts and laws](#contracts-and-laws)
- [Pipez and Chimney](#pipez-and-chimney)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

## Installation

Core derivation library:

```scala
// Add to sbt if you use only JVM Scala
libraryDependencies += "com.kubuszok" %% "pipez" % "<version>"
// Add to sbt it you use Scala.js or Scala Native
libraryDependencies += "com.kubuszok" %%% "pipez" % "<version>"
```

Data transformation DSL:

```scala
// Add to sbt if you use only JVM Scala
libraryDependencies += "com.kubuszok" %% "pipez-dsl" % "<version>"
// Add to sbt it you use Scala.js or Scala Native
libraryDependencies += "com.kubuszok" %%% "pipez-dsl" % "<version>"
```

## Motivating example

Your user send a request to you and, while handling it, you obtained the domain data:

```scala
enum UserType:
  case Normal
  case Admin

final case class User(
  id:       String,
  name:     String,
  password: Array[Byte],
  userType: UserType
)
```

which you have to manually rewrite into format used by your API endpoints:

```scala
enum UType:
  case Normal
  case Admin

final case class ApiUser(
  id:       String,
  name:     String,
  userType: UType
)
```

These types are almost identical, so rewriting it would be pretty dumb - we should be able to convert one into the other
just by matching the corresponding fields (or subtypes) by name! That's what `Convert[From, To]` type class from pipez
DSL does:

```scala
// given such User
val user: User = User(
  id       = "user-1",
  name     = "User #1",
  password = "some-hash".getBytes,
  userType = UserType.Normal,
)

import pipez.dsl.* // provides convertInto

// we can generate the conversion into ApiUser!
val apiUser = user.convertInto[ApiUser]

apiUser == ApiUser(
  id       = "user-1",
  name     = "User #1",
  userType = UType.NORMAL
)
```

Under the hood there is a type class:

```scala
// Converts From value into To value, without failure
trait Converter[From, To]:
  def convert(from: From): To
```

and `user.convertInto[ApiUser]` creates an instance of `Converter[User, ApiUser]` and calls `.convert(user)` on it. It
basically writes for us:

```scala
// This is (more or less) what
//   user.convertInto[ApiUser]
// generates:
new Converter[User, ApiUser] {

  def convert(from: User): ApiUser = ApiUser(
    // Here we map each User field to corresponding to ApiUser
    // field by their name:
    id       = from.id,
    name     = from.name,
    // and when we need to map UserType to UType
    // we map them by their corresponding subtype names:
    userType = from.userName match {
      case UserType.Normal => UType.Normal
      case UserType.Admin  => UType.Admin
    }
  )
}
// Finally, we apply User value to created Converter
.convert(user)
```

letting us forget about writing all this dumb, error-prone boilerplate code ourselves!

> `Converter` is a demonstration how Pipez can be used to implement something similar to Chimney's `Transformer`.

Great, but what if needed to convert things the other way? We would receive the password and we would have to hash it
with a function:

```scala
// Left describes the parsing error
def hashPassword(password: String): Either[String, Array[Byte]]
```

and we receive:

```scala
final case class ApiUserWithPassword(
  id:       String,
  name:     String,
  password: String, // different type than User.password
  userType: UType
)
```

This would require some ability to fail conversion with an error. We still have a way of parsing that automatically!

```scala
import pipez.dsl.* // provides parseFastInto and Parser

val apiUserWithPassword: ApiUserWithPassword = ...

// We are turning the function into Parser instance...
implicit val passwordParser: Parser[String, Array[Byte]] =
  Parser.instance(hashPassword)

// ...because it will be picked up when looking how to perform conversion
//   ApiUserWithPassword.password => User.password
// which might produce errors:
val userResult = apiUserWithPassword.parseFastInto[User]

// assuming correct password:
userResult == Right(User(
  id       = apiUserWithPassword.id,
  name     = apiUserWithPassword.name,
  password = hashPassword(apiUserWithPassword.password).right.get,
  userType = apiUserWithPassword.userType match {
    case UType.Normal => UserType.Normal,
    case UType.Admin  => UserType.Admin
  }
))
```

Parsing allows you to calculate all possible errors or give up upon the first one - for that you have `parseFullInto`
and `parseFastInto` methods. Similarly to `Converter` there is `Parser` type class:

```scala
// Converts From value into To value, but allows conversion to fail, report path
// to the failed value, and chose between fail fast and full error reporting.
trait Parser[From, To]:

  def parse(
    from:     From, // parsed input
    path:     Parser.Path, // Vector of fields/subtype matches leading to the value
    failFast: Parser.ShouldFailFast // =:= Boolean, should be fail fast or continue
  ): Parser.ParsingResult[To] // =:= Either[Errors, To], accumulates parsing errors

  final def parseFast(from: From): Parser.ParsingResult[To] =
    parse(from, Vector.empty, failFast = true)
  final def parseFull(from: From): Parser.ParsingResult[To] =
    parse(from, Vector.empty, failFast = false)
```

When we called `apiUserWithPassword.parseFastInto[User]` we created `Parser[ApiUserWithPassword, User]` instance
and called `.parseFast(apiUserWithPassword.parseFastInto)` on it.

```scala
// This is (more or less!) what
//   apiUserWithPassword.parseFastInto[User]
// generates:
new Parser[ApiUserWithPassword, User] {

  def parse(
    from:     ApiUserWithPassword,
    path:     Parser.Path,
    failFast: Parser.ShouldFailFast
  ): Parser.ParsingResult[User] =
    // We start by calling Parsers for types we cannot just
    // copy paste:
    passwordParser.parse(
      from.password,
      // giving them some extra informaton how we got the value
      path :+ PathSegment.AtField("password"),
      failFast
    ).map { password =>
      // once all "parsable" fields are parsed, we just rewrite
      // the rest matching fields and subtypes by their name
      User(
        id       = from.id,
        name     = from.name,
        password = password,
        userType = from.userType match {
          case UType.Normal => UserType.Normal,
          case UType.Admin  => UserType.Admin
        }
      )
    }
}
// Finally, we pass ApiUserWithPassword to try to convert it to User
.parseFast(apiUserWithPassword)
```

> `Parser` is a demonstration how Pipez can be used to implement something similar to Chimney's `TransformerF` with
> effect `F` based on `Either` and `List` of errors, with `TransformerFErrorPathSupport` provided.

In other words: `Converter` and `Parser` let us easily generate code which rewrites corresponding fields and subtypes by
name, and plug-in our own conversion when it is not obvious how it could generate it.

There is also `PatchApplier` which is used to apply patches:

```scala
case class Input(a: Int, b: String, c: Long)
case class InputPatch(c: Long)

val result = Input(a = 1, b = "b", c = 10L).patchWith(InputPatch(c = 40L))
result == Input(a = 1, b = "b", c = 40L)
```

> `PatchApplier` is a demonstration how Pipez can be used to implement something similar to Chimney's `Patcher`.

## Custom parsers

While `Converter` and `Parser` achieve quite a lot - and can do a lot more as you'll see reading this documentation -
the true power of Pipez comes from the ability to defining your own conversion type class and deriving its instances for
it! How can you achieve it?

Perhaps you have your own conversion type class:

```scala
// Maybe something similar to Converter (no failures, no extra arguments)?
trait NonFailing[From, To]:
  def convert(from: From): To

// Or something which need to pass extra arguments next to converted value?
trait WithContext[From, To]:
  def convert(from: From, pathToFrom: String): To

// Or something which wraps result in some result type, to handle errors or effects?
trait WithResultType[From, To]:
  def convert(from: From): Either[String, To]

// Or both?
trait WithContextAndResult[From, To]:
  def convert(from: From, pathToFrom: String): Either[String, To]
```

then once you define a single `PipeDerivation` implicit (I suggest putting it in the companion object):

```scala
import pipez.*

// putting an instance in companion object:

object NonFailing:
  implicit val derivation: PipeDerivation[NonFailing] = ???

object WithContext:
  implicit val derivation: PipeDerivation[WithContext] = ???

object WithResultType:
  implicit val derivation: PipeDerivation[WithResultType] = ???

object WithContextAndResult:
  implicit val derivation: PipeDerivation[WithContextAndResult] = ???
```

you'll get access to derivation abilities! (For now let's focus what possibilities it gives us, as we can always get to
[how to define this implicit](#how-to-define-pipederivation) later on).

So, how to access the derived instances?

### Automatic derivation

If you want to be able to summon derived instance always, then **automatic derivation** is for you. You can enable it by
mixing in `PipeAutoSupport` to your companion object:

```scala
import pipez.*

// enabling automatic derivation with companion object

object NonFailing extends PipeAutoSupport[NonFailing]:
  implicit val derivation: PipeDerivation[NonFailing] = ???

object WithContext extends PipeAutoSupport[WithContext]:
  implicit val derivation: PipeDerivation[WithContext] = ???

object WithResultType extends PipeAutoSupport[WithResultType]:
  implicit val derivation: PipeDerivation[WithResultType] = ???

object WithContextAndResult extends PipeAutoSupport[WithContextAndResult]:
  implicit val derivation: PipeDerivation[WithContextAndResult] = ???
```

This will let you `summon[NonFailing[User, ApiUser]]` without any additional imports

```scala
// PipeDerivation instance with auto derivation unlocks summoning:
summon[NonFailing[User, ApiUser]]
summon[WithContext[User, ApiUser]]
summon[WithResultType[User, ApiUser]]
summon[WithContextAndResult[User, ApiUser]]
```

which let you define a DSL fetchig such instance (like was done with `Converter` and `Parser` in Pipez DSL).

### Semiautomatic derivation

Sometimes pulling type class instances out of thin air without user doing anything, might be a bit dangerous. You might
prefer them to define them somewhere explicitly - but not necessarily writing them by hand! This is when semiautomatic
derivation comes handy:

```scala
import pipez.*

// enabling semi-automatic derivation with companion object

object NonFailing extends PipeSemiautoSupport[NonFailing]:
  implicit val derivation: PipeDerivation[NonFailing] = ...

object WithContext extends PipeSemiautoSupport[WithContext]:
  implicit val derivation: PipeDerivation[WithContext] = ...

object WithResultType extends PipeSemiautoSupport[WithResultType]:
  implicit val derivation: PipeDerivation[WithResultType] = ...

object WithContextAndResult extends PipeSemiautoSupport[WithContextAndResult]:
  implicit val derivation: PipeDerivation[WithContextAndResult] = ...
```

This will let your users create dumb instances with a one-liner:

```scala
// CompanionObject.derive[From, To] generates an instance

implicit val nonFailingUser =
  NonFailing.derive[User, ApiUser]
implicit val withContextUser =
  WithContext.derive[User, ApiUser]
implicit val withResultTypeUser =
  WithResultType.derive[User, ApiUser]
implicit val withContextAndResultUser =
  WithContextAndResult.derive[User, ApiUser]
```

making it explicit that the conversion is defined in one place, but not forcing user to write all the dumb code by hand.

### Configured derivation

Not all types will be so nice to have corresponding fields or subtypes names. Sometimes a new field will appear in
the output type, some field or type will be renamed or maybe you'll want to plug-in your own conversion for a particular pairs of fields.

This requires an additional API and `PipeSemiautoConfiguredSupport` has you covered:

```scala
// With that CompanionObject.derive[From, To] will use defaults while
// CompanionObject.derive(CompanionObject.Config[From, To]) will let us
// pass custom configuration

object NonFailing
    extends PipeSemiautoSupport[NonFailing]
    with PipeSemiautoConfiguredSupport[NonFailing]:
  implicit val derivation: PipeDerivation[NonFailing] = ...

object WithContext
    extends PipeSemiautoSupport[WithContext]
    with PipeSemiautoConfiguredSupport[WithContext]:
  implicit val derivation: PipeDerivation[WithContext] = ...

object WithResultType
    extends PipeSemiautoSupport[WithResultType]
    with PipeSemiautoConfiguredSupport[WithResultType]:
  implicit val derivation: PipeDerivation[WithResultType] = ...

object WithContextAndResult
    extends PipeSemiautoSupport[WithContextAndResult]
    with PipeSemiautoConfiguredSupport[WithContextAndResult]:
  implicit val derivation: PipeDerivation[WithContextAndResult] = ...
```

This allow us to pass custom configuration to derivation:

```scala
// User has password field which is missing from ApiUser so we have
// to give the derivation a hint how to came up with the value for it
NonFailing.derive(
  NonFailing.Config[ApiUser, User]
    .addField(_.password, (apiUser: ApiUser) => "mock-password".getBytes)
)
```

Configuration is build with fluent API within `.derive(...)`. This let us erase whole `Config` and leave only
the generated type class value. Possible configuration options will be described next to each feature which uses them:

## Supported features and configuration options

The automatically generated mappings can be roughly divided into the following categories:

* case classes and Java Beans
* tuples
* sealed hierarchies and enums
* value types

> Examples below will just show semiautomatic and configured derivation, since however you use them in DSL is up to you.

### Case classes and Java Beans conversions

**Rules of derivation**:

* `case class` has a public constructor and each of its arguments is a `val`
* Java Bean has a public default constructor and getters/setters that you can use to access/set its values (getters have
  `get`/`is` prefix while setters have `set` prefix)
* both input and output is either case class or Java Bean
* unless configuration tells otherwise each output field will require a matching input field to copy value from.
  Matching is done by comparing names of fields (`get`/`is`/`set` prefixes are stripped). The last configuration for
  the output field "wins" and tells where to get the value from
* if value cannot be copied because the types differ, derivation will attempt to summon
  `TypeClass[InputField, OutputField]` to convert it
* if derivation cannot figure out where to get the value from (mismatching types + no conversion, no corresponding
  source field), it fails

```scala
// This requires just rewriting fields in a dumb way...
final case class Input(a: Int, b: String, c: Long)
final case class Output(a: Int, b: String, c: Long)

// ...this (Java Beans) as well
final case class InputBean private (
  @BeanProperty var a: Int,    // in 2.13 creates getA and setA
  @BeanProperty var b: String, // in 2.13 creates getB and setB
  @BeanProperty var c: Long    // in 2.13 creates getC and setC
) { def this() = this(0, "", 0L) }
final case class OutputBean private (
  @BeanProperty var a: Int,
  @BeanProperty var b: String,
  @BeanProperty var c: Long
) { def this() = this(0, "", 0L) }

// Pipez DSL
Converter.derive[Input, Output]
Converter.derive[InputBean, Output]
Converter.derive[Input, OutputBean]
Converter.derive[InputBean, OutputBean]
Parser.derive[Input, Output]
// your own types
NonFailing.derive[Input, Output]
WithContext.derive[Input, Output]
WithResultType.derive[Input, Output]
WithContextAndResult.derive[Input, Output]

// This requires some knowledge how to turn Long to String...
final case class Output2(a: Int, b: String, c: String)
// ... without these ...
implicit val convertLong2Str: Converter[Long, String] = ...
implicit val parseLong2Str: Parser[Long, String] = ...

implicit val notFailingLong2Str: NonFailing[Long, String] = ...
implicit val withContextLong2Str: WithContext[Long, String] = ...
implicit val withResultTypeLong2Str: WithResultType[Long, String] = ...
implicit val withContextAndResultLong2Str: WithContextAndResult[Long, String] = ...
// ...these would not compile:

// Pipez DSL
Converter.derive[Input, Output2]
Parser.derive[Input, Output2]
// your own types
NonFailing.derive[Input, Output2]
WithContext.derive[Input, Output2]
WithResultType.derive[Input, Output2]
WithContextAndResult.derive[Input, Output2]
```

#### `addField` configuration

Tells derivation to use `TypeClass[In, OutField]` to populate the output field that might not have a corresponding input
field. You might use Single Abstract Method syntax:

```scala
// Output.x doesn't have a corresponding source
final case class Input(a: Int, b: String, c: Long)
final case class Output(a: Int, b: String, c: Long, x: Double)

// Pipez DSL
Conversion.derive(
  Conversion.Config[Input, Output]
    .addField(_.x, (in: Input) => in.a.toDouble)
)
Parser.derive(
  Parser.Config[Input, Output]
    .addField(_.x, (in: Input) => Right(in.a.toDouble))
)
// your own types
NonFailing.derive(
  NonFailing.Config[Input, Output]
    .addField(_.x, (in: Input) => in.a.toDouble)
)
WithResultType.derive(
  WithResultType.Config[Input, Output]
    .addField(_.x, (in: Input) => Right(in.a.toDouble))
)
```

#### `renameField` configuration

Tells derivation that a specific target field should use the value from a specific input field. If types differ then the
derivation will attempt to summon a type class to convert it:

```scala
final case class Input(a: Int, b: String, c: Long, x: Double)
final case class Output(a: Int, b: String, c: Long, y: Double)

// Pipez DSL
Conversion.derive(
  Conversion.Config[Input, Output]
    .renameField(_.x, _.y)
)
Parser.derive(
  Parser.Config[Input, Output]
    .renameField(_.x, _.y)
)
// your own types
NonFailing.derive(
  NonFailing.Config[Input, Output]
    .renameField(_.x, _y)
)
WithResultType.derive(
  WithResultType.Config[Input, Output]
    .renameField(_.x, _y)
)
```

#### `plugInField` configuration

Tells derivation to use a specific, manually provided type class instance to convert one field to another:

```scala
final case class Input(a: Int, b: String, c: Long, x: Float)
final case class Output(a: Int, b: String, c: Long, y: Double)

// Pipez DSL
Conversion.derive(
  Conversion.Config[Input, Output]
    .plugInField(_.x, _.y, (in: Float) => in.toDouble)
)
Parser.derive(
  Parser.Config[Input, Output]
    .plugInField(_.x, _.y, (in: Float) => Right(in.toDouble))
)
// your own types
NonFailing.derive(
  NonFailing.Config[Input, Output]
    .plugInField(_.x, _y, (in: Float) => in.toDouble)
)
WithResultType.derive(
  WithResultType.Config[Input, Output]
    .plugInField(_.x, _y, (in: Float) => Right(in.toDouble))
)
```

#### `fieldMatchingCaseInsensitive` configuration

By default, field matching is case-sensitive. This flag enables case-insensitive comparison:

```scala
// This would fail as fields have different cases
// with with case-insensitive matching it works
final case class Input(a: Int, b: String, c: Long)
final case class Output(A: Int, B: String, C: Long)

// Pipez DSL
Converter.derive(
  Converter.Config[Input, Output]
    .fieldMatchingCaseInsensitive
)
Parser.derive(
  Parser.Config[Input, Output]
    .fieldMatchingCaseInsensitive
)
// your own types
NonFailing.derive(
  NonFailing.Config[Input, Output]
    .fieldMatchingCaseInsensitive
)
WithContext.derive(
  WithContext.Config[Input, Output]
    .fieldMatchingCaseInsensitive
)
WithResultType.derive(
  WithResultType.Config[Input, Output]
    .fieldMatchingCaseInsensitive
)
WithContextAndResult.derive(
  WithContextAndResult.Config[Input, Output]
    .fieldMatchingCaseInsensitive
)
```

#### `addFallbackToValue` configuration

Tells derivation that if there is no field in input with some name, it should try to get this field from the value
passed into the configuration. There can be multiple fallback values - derivation will fallback in the in order in which
they were provided.

```scala
// Input doesn't define x, but Fallback does
final case class Input(a: Int, b: String, c: Long)
final case class Output(a: Int, b: String, c: Long, x: Double)
final case class Fallback(x: Double)

// Pipez DSL
Converter.derive(
  Converter.Config[Input, Output]
    .addFallbackToValue(Fallback(x = 10.0))
)
Parser.derive(
  Parser.Config[Input, Output]
    .addFallbackToValue(Fallback(x = 10.0))
)
// your own types
NonFailing.derive(
  NonFailing.Config[Input, Output]
    .addFallbackToValue(Fallback(x = 10.0))
)
WithContext.derive(
  WithContext.Config[Input, Output]
    .addFallbackToValue(Fallback(x = 10.0))
)
WithResultType.derive(
  WithResultType.Config[Input, Output]
    .addFallbackToValue(Fallback(x = 10.0))
)
WithContextAndResult.derive(
  WithContextAndResult.Config[Input, Output]
    .addFallbackToValue(Fallback(x = 10.0))
)
```

#### `enableFallbackToDefaults` configuration

Tells derivation to use the default value if present and no other way of computing the field value is accessible:

```scala
// Conversion from Input to Output would fail since there is
// no x in Input, but we can tell derivation to use defaults
final case class Input(a: Int, b: String, c: Long)
final case class Output(a: Int, b: String, c: Long, x: Double = 1.0)

// Pipez DSL
Converter.derive(
  Converter.Config[Input, Output]
    .enableFallbackToDefaults
)
Parser.derive(
  Parser.Config[Input, Output]
    .enableFallbackToDefaults
)
// your own types
NonFailing.derive(
  NonFailing.Config[Input, Output]
    .enableFallbackToDefaults
)
WithContext.derive(
  WithContext.Config[Input, Output]
    .enableFallbackToDefaults
)
WithResultType.derive(
  WithResultType.Config[Input, Output]
    .enableFallbackToDefaults
)
WithContextAndResult.derive(
  WithContextAndResult.Config[Input, Output]
    .enableFallbackToDefaults
)
```

If `addFallbackToValue` is used, derivation will fallback to defaults only after there won't be any source provided
with config, available in source nor available in fallback values.

#### `recursiveDerivation` configuration

Tells derivation to allow recursive derivation for fields conversion if no implicit is present and types don't match.
Not needed if you mixed-in `PipeAutoSupport` into your companion:

```scala
// With semi-auto this would require deriving Input2 -> Output2,
// making it implicit val and then deriving Input -> Output
final case class Input2(d: Double)
final case class Input(a: Int, b: String, c: Long, d: Input2)
final case class Output2(d: Double)
final case class Output(a: Int, b: String, c: Long, d: Output2)

// Pipez DSL has automatic derivation enabled

// your own types (assuming they use semiauto)
NonFailing.derive(
  NonFailing.Config[Input, Output]
    .recursiveDerivation
)
WithContext.derive(
  WithContext.Config[Input, Output]
    .recursiveDerivation
)
WithResultType.derive(
  WithResultType.Config[Input, Output]
    .recursiveDerivation
)
WithContextAndResult.derive(
  WithContextAndResult.Config[Input, Output]
    .recursiveDerivation
)
```

### Tuples

**Rules of derivation**:

* either input or output type is a tuple
* the other type might be a case class instead of tuple
* fields are matched by their position (if it's a case class we consider in which order fields were defined).
  Configuration options from case classes apply
* if value cannot be copied because the types differ, derivation will attempt to summon
  `TypeClass[InputField, OutputField]` to convert it
* if derivation cannot figure out where to get the value from (mismatching types + no conversion, no corresponding
  source field), it fails

```scala
// When derived against tuple, derivation will use position
// of value in a case class:
final case class Input(a: Int, b: String, c: Long)
final case class Output(a: Int, b: String, c: Long)

// If types in the same position differ, conversion will be summoned
implicit val convertLong2Str: Converter[Long, String] = ...
implicit val parseLong2Str: Parser[Long, String] = ...

// Pipez DSL
Converter.derive[Input, (Int, String, Long)]
Converter.derive[(Int, String, Long), Output]
Converter.derive[(Int, String, Long), (Int, String, String)] // use convertLong2Str
Parser.derive[Input, (Int, String, Long)]
Parser.derive[(Int, String, Long), Output]
Parser.derive[(Int, String, Long), (Int, String, String)] // use parseLong2Str
// your own types
NonFailing.derive[Input, (Int, String, Long)]
NonFailing.derive[(Int, String, Long), Output]
...
```

### Sealed hierarchies and enums conversions

**Rules of derivation**:

* both input and output is a `sealed` type or `enum`
* subtypes will be matches by their names
* unless configuration tells otherwise each input subtype will require a matching output subtype to target a conversion.
  Matching is done by comparing names of subtypes. The last configuration for input subtype "wins" and tells where to
  get the value from
* by default for each `InputSubtype`, `OutputSubtype` pair derivation will attempt to summon implicit - if it cannot do
  it, it will attempt to derive it
* if derivation cannot figure out where to convert the subtype into (mismatching types + no conversion, no corresponding
  target subtype), it fails

```scala
// Derivation for ADTs uses subtype/element name for matching
sealed trait Input[+T] extends Product with Serializable:
object Input:
  case object A extends Input[Nothing]
  final case class B(b: T) extends Input[T]
  final case class C(s: String) extends Input[String]
enum Output[+T]:
  case A
  case B(b: T)
  case C(s: String) extends Output[String]

// Types' names match, no conversion needed - work OOTB!

// Pipez DSL
Converter.derive[Input, Output]
Parser.derive[Input, Output]
// your own types
NonFailing.derive[Input, Output]
WithContext.derive[Input, Output]
WithResultType.derive[Input, Output]
WithContextAndResult.derive[Input, Output]
```

#### `removeSubtype` configuration

Tells derivation that for particular input subtype it should use a specified type class instance:

```scala
// C doesn't have a corresponding target
enum Input:
  case A
  case B(b: Int)
  case C(c: Int)
enum Output:
  case A
  case B(b: Int)

// Pipez DSL
Conversion.derive(
  Conversion.Config[Input, Output]
    .removeSubtype[Input.C]((in: Input.C) => Output.B(in.c))
)
Parser.derive(
  Parser.Config[Input, Output]
    .removeSubtype[Input.C]((in: Input.C) => Right(Output.B(in.c)))
)
// your own types
NonFailing.derive(
  NonFailing.Config[Input, Output]
    .removeSubtype[Input.C]((in: Input.C) => Output.B(in.c))
)
WithResultType.derive(
  WithResultType.Config[Input, Output]
    .removeSubtype[Input.C]((in: Input.C) => Right(Output.B(in.c)))
)
```

#### `renameSubtype` configuration

Tells derivation that a particular input subtype it should be converted into particular output subtype:

```scala
// Input.B should target Output.C
enum Input:
  case A
  case B(b: Int)
enum Output:
  case A
  case C(b: Int)

// Pipez DSL
Conversion.derive(
  Conversion.Config[Input, Output]
    .removeSubtype[Input.B, Output.C]
)
Parser.derive(
  Parser.Config[Input, Output]
    .removeSubtype[Input.B, Output.C]
)
// your own types
NonFailing.derive(
  NonFailing.Config[Input, Output]
    .removeSubtype[Input.B, Output.C]
)
WithResultType.derive(
  WithResultType.Config[Input, Output]
    .removeSubtype[Input.B, Output.C]
)
```

#### `plugInSubtype` configuration

Tells derivation to use manually passed type class to convert one subtype into another

```scala
// Input.B should target Output.C
enum Input:
  case A
  case B(b: Int)
enum Output:
  case A
  case C(b: Double)

// Pipez DSL
Conversion.derive(
  Conversion.Config[Input, Output]
    .plugInSubtype[Input.B, Output.C]((in: Input.B) => Output.C(in.b.toDouble))
)
Parser.derive(
  Parser.Config[Input, Output]
    .plugInSubtype[Input.B, Output.C]((in: Input.B) => Right(Output.C(in.b.toDouble)))
)
// your own types
NonFailing.derive(
  NonFailing.Config[Input, Output]
    .plugInSubtype[Input.B, Output.C]((in: Input.B) => Output.C(in.b.toDouble))
)
WithResultType.derive(
  WithResultType.Config[Input, Output]
    .plugInSubtype[Input.B, Output.C]((in: Input.B) => Right(Output.C(in.b.toDouble)))
)
```

#### `enumMatchingCaseInsensitive` configuration

By default, subtype matching is case-sensitive. This flag enables case-insensitive comparison:

```scala
// Names don't match since they have different cases
enum Input:
  case Aaa
  case Bbb(b: Int)
enum Output:
  case AAA
  case BBB(b: Int)

// Pipez DSL
Conversion.derive(
  Conversion.Config[Input, Output]
    .enumMatchingCaseInsensitive
)
Parser.derive(
  Parser.Config[Input, Output]
    .enumMatchingCaseInsensitive
)
// your own types
NonFailing.derive(
  NonFailing.Config[Input, Output]
    .enumMatchingCaseInsensitive
)
WithResultType.derive(
  WithResultType.Config[Input, Output]
    .enumMatchingCaseInsensitive
)
```

### AnyVals conversions

**Rules of derivation**:

* either input or output has to be `AnyVal` type
* the other type might be a primitive (`Int`, `Long`, `String`, etc)
* derivation will unwrap value from input type (if needed) and wrap it (if needed) in output type
* if types of (unwrapped) input and (unwrapped) output don't match, derivation fails

```scala
// Whether AnyVal is case class or not...
class Input(val value: String) extends AnyVal
final case class Output(str: String) extends AnyVal

/// ...wrapping and unwrapping works out of the box

// Pipez DSL
Converter.derive[Input, Output]
Converter.derive[Input, String]
Converter.derive[String, Output]
Parser.derive[Input, Output]
Parser.derive[Input, String]
Parser.derive[String, Output]

// your own types
NonFailing.derive[Input, Output]
NonFailing.derive[Input, String]
NonFailing.derive[String, Output]
...
```

### Deriving instances for Scala 3 types in Scala 2.13 and vice-versa

Cross-compilation requires:

* `("organization" %% "library" % version).cross(CrossVersion.for3Use2_13)` to use Scala 2.13 type in Scala 3
* `("organization" %% "library" % version).cross(CrossVersion.for2_13Use3)` to use Scala 3 type in Scala 2.13, as well
  as adding `"-Ytasty-reader"` flag to `scalacOptions`
* matching versions of TASTY - Pipez was tested for 2.13.10 against 3.2.1

### Features you have to implement yourself

* Pipez only provides you a way of derive a type class - build-in instances of your type class you have to write
  yourself!
* this includes: collections, Maps, Options (lifting `F[A, B]` to `F[Option[A, B]]` or `F[A, Option[B]]` etc.)
* Pipez doesn't automatically support: Scala Enumeration or Java enums conversion (since they can be implemented in
  runtime)
* Pipez isn't going to write for you some DSL which would call Pipez in a customized way

## How to define `PipeDerivation`

Let's say you defined you type class like this:

```scala
trait WithContextAndResult[From, To]:
  def convert(from: From, pathToFrom: String): Either[String, To]
```

then you defined some typed and conversion between its fields' types:

```scala
final case class Input(a: Int, b: String, c: Int, x: Float)
final case class Output(a: Int, b: String, c: Long, y: Double)

implicit val int2long: WithContextAndResult[Int, Long] =
  (in, _) => Right(in.toLong)
implicit val float2double: WithContextAndResult[Float, Double] =
  (in, _) => Right(in.toDouble)
```

so that you could generate the code converting it:

```scala
WithContextAndResult.derive[Input, Output]
```

how could derivation actually create such type?

We need a few things:

* sometimes we need to get the type class instance and put a field in it, so we have to know how to call it
* this way we might end up with several results - each converting another field - which we would have to combine, so we
  need a way of combining results
* some of these values are not requiring conversion, and we just want to wrap them in result type
* finally, we need something that would let us create a type class from a recipe that: takes the input (possibly with
  these extra arguments), creates output result out of it
* the result type and extra arguments should not leak not we shouldn't require it to be a part of the type class
  signature

How could we express that?

Pipez arrived at one way of expressing these requirements using path-dependent types:

```scala
/** Pipe parameters is where you put your type class */
trait PipeDerivation[Pipe[_, _]] {

  /** With this you will pass all extra arguments */
  type Context

  /** Type of your results */
  type Result[Out]

  /** Turns a function into your `Pipe` typeclass */
  def lift[In, Out](f: (In, Context) => Result[Out]): Pipe[In, Out]

  /** Calls `Pipe` as if it was a function */
  def unlift[In, Out](pipe: Pipe[In, Out], in: In, ctx: Context): Result[Out]

  /** Wraps raw value into `Result` */
  def pureResult[A](a: A): Result[A]

  /** Combines 2 `Results` into 1 */
  def mergeResults[A, B, C](context: Context, ra: Result[A], rb: => Result[B], f: (A, B) => C): Result[C]

  /** Useful thing but we'll talk about it later on */
  def updateContext(context: Context, path: => Path): Context
}
```

For instance for `WithContextAndResult` the implementation could look like this:

```scala
import pipez.*

object WithContextAndResult
    extends PipeSemiautoSupport[WithContextAndResult]
    with PipeSemiautoConfiguredSupport[WithContextAndResult]:

  implicit val pd: PipeDerivation[WithContextAndResult] =
    new PipeDerivation[WithContextAndResult] {

      /** The only extra argument is pathToFrom: String */
      type Context = String

      /** What .convert(from, path) would return */
      type Result[Out] = Either[String, Out]

      /** Create a function from a type class */
      def lift[In, Out](
        f: (In, String) => Either[String, Out]
      ): WithContextAndResult[In, Out] = f(_, _) // SAM

      /** Calls `Pipe` as if it was a function */
      def unlift[In, Out](
        converter:  WithContextAndResult[In, Out],
        in:         In,
        pathToFrom: String
      ): Either[String, Out] = converter.convert(in, pathToFrom)

      /** Wraps raw value into `Result` */
      def pureResult[A](a: A): Either[String, A] = Right(a)

      /** Combines 2 `Results` into 1 */
      def mergeResults[A, B, C](
        pathToFrom: String,
        ra:         Either[String, A],
        rb:         => Either[String, B],
        f:          (A, B) => C
      ): Either[String, C] = for {
        a <- ra
        b <- rb
      } yield f(a, b)

      /** Useful thing but we'll talk about it later on */
      def updateContext(pathToFrom: String, path: => Path): String =
        pathToFrom
    }
```

This instance simple converts between `(In, String) => Either[String, Out]` and `WithContextAndResult[In, Out]`, glues
together `Either`s and wraps pure value. Nothing complex.

However, this allow us to easily create desired  `WithContextAndResult[Input, Output]` instance. It could be done line
this:

```scala
WithContextAndResult.pd.lift { (in: Input, pathToFrom: String) =>
  WithContextAndResult.pd.mergeResults(
    pathToFrom,
    WithContextAndResult.pd.unlift(int2long, in.c, pathToFrom),
    WithContextAndResult.pd.unlift(float2double, in.d, pathToFrom),
    (c, d) => Output(a = in.a, b = in.b, c = c, d = d)
  )
}
```

Similarly, for enum conversion, one could implement conversion for:

```scala
enum Input:
  case A
  case B(b: Int)
enum Output:
  case A
  case B(b: Long)
```

as

```scala
WithContextAndResult.pd.lift { (in: Input, pathToFrom: String) =>
  in match {
    case Input.A =>
      WithContextAndResult.pd.pure(Output.A)
    case Input.B(b) =>
      WithContextAndResult.pd.mergeResult(
        pathToFrom,
        WithContextAndResult.pd.pure(()),
        WithContextAndResult.pd.unlift(intoToLong, b, pathToFrom),
        (_, b) => Output.B(b)
      )
  }
}
```

While the *exact* ways the derivation would use the `PipeDerivation` type class is NOT a part of any contract, you can
assume that conversion would be performed using `.map2` logic of `Applicative` (or `.parMap2` from `NonEmptyParallel`).

### Enriching Context with path to value

The only not explained part of `PipeDerivation` is `updateContext`. It can be used to inject information about the path
that lead to the value passed through the `unlift`.

Basically every time you derivation extracts field before passing it into unlift it calls `updateContext`. For case
classes it can look like this:

```scala
WithContextAndResult.pd.lift { (in: Input, pathToFrom: String) =>
  WithContextAndResult.pd.mergeResults(
    pathToFrom, // not changed
    WithContextAndResult.pd.unlift(
      int2long,
      in.c,
      // we can update value of pathToFrom with knowledge that we picked .c
      WithContextAndResult.pd.updateContext(pathToFrom, Path.root.field("c"))
    ),
    WithContextAndResult.pd.unlift(
      float2double,
      in.d,
      // we can update value of pathToFrom with knowledge that we picked .d
      WithContextAndResult.pd.updateContext(pathToFrom, Path.root.field("d"))
    ),
    (c, d) => Output(a = in.a, b = in.b, c = c, d = d)
  )
}
```

Meanwhile, for enums it can look like this:

```scala
WithContextAndResult.pd.lift { (in: Input, pathToFrom: String) =>
  in match {
    case Input.A =>
      WithContextAndResult.pd.pure(Output.A)
    case Input.B(b) =>
      WithContextAndResult.pd.mergeResult(
        pathToFrom,
        WithContextAndResult.pd.pure(()),
        WithContextAndResult.pd.unlift(
          intoToLong,
          b,
          // we can update pathToFrom with knowledge that we picked subtype B
          WithContextAndResult.pd.updateContext(pathToFrom, Path.root.subtype("B"))
        ),
        (_, b) => Output.B(b)
      )
  }
}
```

If we define our `updateContext` method to add `pipez.Path` value to `Context` we passed it, we will be able to have
access a whole path to the obtained value. With that we could e.g. create better error messages in `Left` side of
`Either`... or log if we would make our `Result` to be side-effectful.

### Debugging

If you are not sure what is happening during macro expansion and what code it generated,
pass it a configuration with `.enableDiagnosics` option.

### Contracts and laws

What Pipez promises is that:

* it will not use conversion if an input field type is a subtype of output field type
* when conversion of a field/subtype will be performed, the library will provide instance (from summoning or config)
  and then use users code (`pipeDerivation.unlift`) to run it
* the partial results of conversions of fields will be combined through `pipeDerivation.mergeResult`

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

## Pipez and Chimney

Chimney focuses on giving user the best out-of-the-box developer experience:

* it provides DSL to transform value in-place without providing any custom definitions unless necessary
* it supports operations on _native_ types like `Option`, collections
* it doesn't require defining custom types to make transformation possible
* it has options like providing pure values, generating pure values from transformed object,
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
