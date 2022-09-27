package pipez.dsl

import scala.collection.Factory
import scala.collection.immutable.ListMap

/** Conversion of types which allow:
  *   - failure with error messages
  *   - choosing between fail-fast and full attempt to gather all errors
  *   - aggregating errors under path to the field which failed
  */
trait Parser[From, To] {

  def parse(from: From, path: Parser.Path, failFast: Parser.ShouldFailFast): Parser.ParsingResult[To]

  final def parseFast(from: From): Parser.ParsingResult[To] = parse(from, Vector.empty, failFast = true)
  final def parseFull(from: From): Parser.ParsingResult[To] = parse(from, Vector.empty, failFast = false)
}
object Parser
    extends pipez.PipeAutoSupport[Parser]
    with pipez.PipeSemiautoConfiguredSupport[Parser]
    with ParserInstances0 {

  /** Utility to create `Converter` without SAM */
  def instance[From, To](f: From => Either[Vector[String], To]): Parser[From, To] =
    (from: From, path: Path, _: ShouldFailFast) => f(from).left.map(errors => ListMap(path -> errors))

  /** Element of a `Path` to the failed field */
  sealed trait PathSegment extends Product with Serializable
  object PathSegment {

    final case class AtField(name: String) extends PathSegment
    final case class MatchType(name: String) extends PathSegment
    final case class AtIndex(index: Int) extends PathSegment
    final case class AtKey(key: Any) extends PathSegment
    final case class WithKey(key: Any) extends PathSegment
  }

  type Path             = Vector[PathSegment]
  type ShouldFailFast   = Boolean
  type Errors           = ListMap[Path, Vector[String]]
  type ParsingResult[A] = Either[Errors, A]

  implicit val pipeDerivation: pipez.PipeDerivation[Parser] = ParserDerivationDefinition
}

private[dsl] trait ParserInstances0 extends ParserInstances1 {

  implicit def parseEither[FromLeft, FromRight, ToLeft, ToRight, FromEither[L, R] <: Either[L, R]](implicit
    left:  Parser[FromLeft, ToLeft],
    right: Parser[FromRight, ToRight]
  ): Parser[FromEither[FromLeft, FromRight], Either[ToLeft, ToRight]] =
    pipez.PipeDerivation
      .derive[Parser, Either[FromLeft, FromRight], Either[ToLeft, ToRight]]
      .asInstanceOf[Parser[FromEither[FromLeft, FromRight], Either[ToLeft, ToRight]]]

  implicit def parseOption[From, To, FromOption[A] <: Option[A]](implicit
    parser: Parser[From, To]
  ): Parser[FromOption[From], Option[To]] =
    pipez.PipeDerivation.derive[Parser, Option[From], Option[To]].asInstanceOf[Parser[FromOption[From], Option[To]]]

  implicit def parseCollection[From, To, FromColl[A] <: Iterable[A], ToColl[_]](implicit
    parser: Parser[From, To],
    ToColl: Factory[To, ToColl[To]]
  ): Parser[FromColl[From], ToColl[To]] = (fromColl, path, failFast) =>
    if (failFast) {
      val toBuilder = ToColl.newBuilder
      var errorsStore: Parser.Errors = null.asInstanceOf[Parser.Errors]
      val iterator = fromColl.iterator.zipWithIndex
      var noError  = true
      while (iterator.hasNext && noError) {
        val (from, index) = iterator.next()
        parser.parse(from, path :+ Parser.PathSegment.AtIndex(index), failFast) match {
          case Left(errors) =>
            errorsStore = errors
            noError = false
          case Right(value) =>
            toBuilder.addOne(value)
        }
      }
      if (noError) Right(toBuilder.result()) else Left(errorsStore)
    } else {
      val toBuilder     = ToColl.newBuilder
      val errorsBuilder = ListMap.newBuilder[Parser.Path, Vector[String]]
      val iterator      = fromColl.iterator.zipWithIndex
      var noError       = true
      while (iterator.hasNext) {
        val (from, index) = iterator.next()
        parser.parse(from, path :+ Parser.PathSegment.AtIndex(index), failFast) match {
          case Left(errors) =>
            errorsBuilder.addAll(errors)
            noError = false
          case Right(value) =>
            if (noError) {
              toBuilder.addOne(value)
            }
        }
      }
      if (noError) Right(toBuilder.result()) else Left(errorsBuilder.result())
    }

  implicit def parseMap[FromKey, FromValue, ToKey, ToValue, FromMap[K, V] <: Map[K, V], ToMap[K, V] <: Map[K, V]](
    implicit
    key:   Parser[FromKey, ToKey],
    value: Parser[FromValue, ToValue],
    ToMap: Factory[(ToKey, ToValue), ToMap[ToKey, ToValue]]
  ): Parser[FromMap[FromKey, FromValue], ToMap[ToKey, ToValue]] = (fromMap, path, failFast) =>
    if (failFast) {
      val toBuilder = ToMap.newBuilder
      var errorsStore: Parser.Errors = null.asInstanceOf[Parser.Errors]
      val iterator = fromMap.iterator
      var noError  = true
      while (iterator.hasNext && noError) {
        val (k, v) = iterator.next()
        (for {
          k <- key.parse(k, path :+ Parser.PathSegment.WithKey(k), failFast)
          v <- value.parse(v, path :+ Parser.PathSegment.AtKey(k), failFast)
        } yield (k, v)) match {
          case Right(pair) =>
            toBuilder.addOne(pair)
          case Left(e) =>
            errorsStore = e
            noError = false
        }
      }
      if (noError) Right(toBuilder.result()) else Left(errorsStore)
    } else {
      val toBuilder     = ToMap.newBuilder
      val errorsBuilder = ListMap.newBuilder[Parser.Path, Vector[String]]
      val iterator      = fromMap.iterator
      var noError       = true
      while (iterator.hasNext) {
        val (k, v) = iterator.next()
        (key.parse(k, path :+ Parser.PathSegment.WithKey(k), failFast),
         value.parse(v, path :+ Parser.PathSegment.AtKey(k), failFast)
        ) match {
          case (Right(k2), Right(v2)) =>
            toBuilder.addOne(k2, v2)
          case (Left(e1), Left(e2)) =>
            errorsBuilder.addAll(e1).addAll(e2)
            noError = false
          case (Left(e), _) =>
            errorsBuilder.addAll(e)
            noError = false
          case (_, Left(e)) =>
            errorsBuilder.addAll(e)
            noError = false
        }
      }
      if (noError) Right(toBuilder.result()) else Left(errorsBuilder.result())
    }
}
private[dsl] trait ParserInstances1 extends ParserInstances2 {

  implicit def parseToOption[From, To](implicit parser: Parser[From, To]): Parser[From, Option[To]] =
    (from, path, failFast) => parser.parse(from, path, failFast).map(Some(_))
}
private[dsl] trait ParserInstances2 {

  implicit def parseFromOption[From, To](implicit parser: Parser[From, To]): Parser[Option[From], To] =
    (from, path, failFast) =>
      Parser.parseOption[From, To, Option].parse(from, path, failFast) match {
        case Right(Some(value)) => Right(value)
        case Right(None)        => Left(ListMap(path -> Vector("Expected non-empty Option")))
        case Left(errors)       => Left(errors)
      }

  implicit def parseToSelf[A, B >: A]: Parser[A, B] = (a, _, _) => Right(a)
}
private[dsl] object ParserDerivationDefinition extends pipez.PipeDerivation[Parser] {
  import Parser._

  override type Context   = (Path, ShouldFailFast)
  override type Result[A] = ParsingResult[A]

  override def lift[In, Out](f: (In, Context) => Result[Out]): Parser[In, Out] =
    (from: In, path: Path, failFast: ShouldFailFast) => f(from, (path, failFast))
  override def unlift[In, Out](pipe: Parser[In, Out], in: In, ctx: Context): Result[Out] =
    pipe.parse(in, ctx._1, ctx._2)
  override def updateContext(context: Context, path: => pipez.Path): Context = {
    @scala.annotation.tailrec
    def fromPath(in: pipez.Path, out: Path): Path = in match {
      case pipez.Path.Root                => out
      case pipez.Path.Field(from, name)   => fromPath(from, PathSegment.AtField(name) +: out)
      case pipez.Path.Subtype(from, name) => fromPath(from, PathSegment.MatchType(name) +: out)
    }

    val append = fromPath(path, Vector.empty)
    if (append.isEmpty) context else context.copy(context._1 ++ append)
  }
  override def pureResult[A](a: A): Result[A] =
    Right(a)
  override def mergeResults[A, B, C](context: Context, ra: Result[A], rb: => Result[B], f: (A, B) => C): Result[C] =
    if (context._2) {
      // should fail fast
      for {
        a <- ra
        b <- rb
      } yield f(a, b)
    } else {
      // should parse everything
      (ra, rb) match {
        case (Right(a), Right(b)) => Right(f(a, b))
        case (Left(e1), Left(e2)) => Left(e1 ++ e2)
        case (Left(e), _)         => Left(e)
        case (_, Left(e))         => Left(e)
      }
    }
}
