package pipez.dsl

import scala.collection.immutable.{ HashMap, ListMap }

class ConverterDslSpec extends munit.FunSuite {

  test("from.convertInto[To] should handle identity mapping (From <:< To)") {
    assertEquals(10.convertInto[Int], 10)
    assertEquals("test".convertInto[String], "test")
    assertEquals(15L.convertInto[AnyVal], 15L)
  }

  test("from.convertInto[To] should handle Options") {
    implicit val intToSting: Converter[Int, String] = _.toString
    assertEquals(10.convertInto[Option[String]], Some("10"))
    assertEquals(Option(10).convertInto[Option[String]], Some("10"))
    assertEquals(Some(10).convertInto[Some[String]], Some("10"))
    assertEquals(
      {
        import Converter.unsafe.unsafeConvertFromOption
        Option(10).convertInto[String]
      },
      "10"
    )
  }

  test("from.convertInto[To] should handle Eithers") {
    implicit val intToSting: Converter[Int, String] = _.toString
    assertEquals((Left(10): Either[Int, Int]).convertInto[Either[String, String]], Left("10"))
    assertEquals((Right(10): Either[Int, Int]).convertInto[Either[String, String]], Right("10"))
    assertEquals(Left(10).convertInto[Left[String, Nothing]], Left("10"))
    assertEquals(Right(10).convertInto[Right[Nothing, String]], Right("10"))
  }

  test("from.convertInto[To] should handle collections") {
    implicit val intToSting: Converter[Int, String] = _.toString
    assertEquals(
      List(1, 2, 3, 4).convertInto[Vector[String]],
      Vector("1", "2", "3", "4")
    )
  }

  test("from.convertInto[To] should handle maps") {
    implicit val intToSting: Converter[Int, String] = _.toString
    assertEquals(
      HashMap(1 -> 1, 2 -> 2, 3 -> 3, 4 -> 4).convertInto[ListMap[String, String]],
      ListMap("1" -> "1", "2" -> "2", "3" -> "3", "4" -> "4")
    )
  }

  test("from.convertInto[To] should handle automatic derivation") {
    import pipez.{ `Backtick ADT In`, `Backtick ADT Out` }
    assertEquals(
      (`Backtick ADT In`.`Case Class`("test"): `Backtick ADT In`).convertInto[`Backtick ADT Out`],
      `Backtick ADT Out`.`Case Class`("test")
    )
  }

  test("from.convertInto[To] should handle configured derivation") {
    import pipez.{ CaseParamIn, CaseParamOutExt }
    implicit val intToSting: Converter[Int, String] = _.toString
    assertEquals(
      CaseParamIn(1, "test", 2).convertInto[CaseParamOutExt[String]](
        Converter.Config[CaseParamIn[Int], CaseParamOutExt[String]].addField(_.x, _ => "3")
      ),
      CaseParamOutExt(1, "test", "2", "3")
    )
  }
}
