package pipez.dsl

import scala.collection.Factory

trait Converter[From, To] {

  def convert(value: From): To
}
object Converter
    extends pipez.PipeAutoSupport[Converter]
    with pipez.PipeSemiautoConfiguredSupport[Converter]
    with ConverterInstances0 {

  def instance[From, To](f: From => To): Converter[From, To] = f(_)

  object unsafe {

    implicit def unsafeConvertFromOption[From, To](implicit safe: Converter[From, To]): Converter[Option[From], To] =
      option => safe.convert(option.get)
  }

  implicit val pipeDerivation: pipez.PipeDerivation[Converter] = ConverterDerivationDefinition
}
private[dsl] trait ConverterInstances0 extends ConverterInstances1 { self: Converter.type =>

  implicit def convertEither[FromLeft, FromRight, ToLeft, ToRight, FromEither[L, R] <: Either[L, R]](implicit
    left:  Converter[FromLeft, ToLeft],
    right: Converter[FromRight, ToRight]
  ): Converter[FromEither[FromLeft, FromRight], Either[ToLeft, ToRight]] = either =>
    (either: Either[FromLeft, FromRight]) match {
      case Left(value)  => Left(left.convert(value))
      case Right(value) => Right(right.convert(value))
    }
  implicit def convertLeft[FromLeft, FromRight, ToLeft, ToRight](implicit
    left: Converter[FromLeft, ToLeft]
  ): Converter[Left[FromLeft, FromRight], Left[ToLeft, ToRight]] = { case Left(value) =>
    Left(left.convert(value))
  }
  implicit def convertRight[FromLeft, FromRight, ToLeft, ToRight](implicit
    right: Converter[FromRight, ToRight]
  ): Converter[Right[FromLeft, FromRight], Right[ToLeft, ToRight]] = { case Right(value) =>
    Right(right.convert(value))
  }

  implicit def covertOption[From, To, FromOption[A] <: Option[A]](implicit
    converter: Converter[From, To]
  ): Converter[FromOption[From], Option[To]] =
    _.map(converter.convert)
  implicit def covertSome[From, To](implicit converter: Converter[From, To]): Converter[Some[From], Some[To]] = {
    case Some(value) => Some(converter.convert(value))
  }

  implicit def convertCollection[From, To, FromColl[A] <: Iterable[A], ToColl[_]](implicit
    converter: Converter[From, To],
    ToColl:    Factory[To, ToColl[To]]
  ): Converter[FromColl[From], ToColl[To]] = fromColl => {
    val builder  = ToColl.newBuilder
    val iterator = fromColl.iterator
    while (iterator.hasNext)
      builder.addOne(converter.convert(iterator.next()))
    builder.result()
  }

  implicit def convertMap[FromKey, FromValue, ToKey, ToValue, FromMap[K, V] <: Map[K, V], ToMap[K, V] <: Map[K, V]](
    implicit
    key:   Converter[FromKey, ToKey],
    value: Converter[FromValue, ToValue],
    ToMap: Factory[(ToKey, ToValue), ToMap[ToKey, ToValue]]
  ): Converter[FromMap[FromKey, FromValue], ToMap[ToKey, ToValue]] = fromMap => {
    val builder  = ToMap.newBuilder
    val iterator = fromMap.iterator
    while (iterator.hasNext) {
      val (k, v) = iterator.next()
      builder.addOne(key.convert(k) -> value.convert(v))
    }
    builder.result()
  }
}
private[dsl] trait ConverterInstances1 extends ConverterInstances2 { self: Converter.type =>

  implicit def convertToOption[From, To](implicit converter: Converter[From, To]): Converter[From, Option[To]] =
    from => Some(converter.convert(from))
}
private[dsl] trait ConverterInstances2 { self: Converter.type =>

  implicit def convertToSelf[A, B >: A]: Converter[A, B] = a => a
}
private[dsl] object ConverterDerivationDefinition extends pipez.PipeDerivation.Simple[Converter] {
  import Converter._

  override def simpleLift[In, Out](f: In => Out):                       Converter[In, Out] = f(_)
  override def simpleUnlift[In, Out](pipe: Converter[In, Out], in: In): Out                = pipe.convert(in)
}
