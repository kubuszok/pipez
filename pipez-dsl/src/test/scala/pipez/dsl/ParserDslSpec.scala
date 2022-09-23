package pipez.dsl

import scala.collection.immutable.{ HashMap, ListMap }

class ParserDslSpec extends munit.FunSuite {

  test("from.parseFastInto[To] and from.parseFullInto[To] should handle identity mapping (From <:< To)") {
    assertEquals(10.parseFastInto[Int], Right(10))
    assertEquals("test".parseFastInto[String], Right("test"))
    assertEquals(15L.parseFastInto[AnyVal], Right(15L))
  }

  test("from.parseFastInto[To] and from.parseFullInto[To] should handle Options") {
    implicit val intToSting: Parser[Int, String] = Parser.instance(i => Right(i.toString))
    assertEquals(10.parseFastInto[Option[String]], Right(Some("10")))
    assertEquals(Option(10).parseFastInto[Option[String]], Right(Some("10")))
    assertEquals(Some(10).parseFastInto[Some[String]], Right(Some("10")))
    assertEquals(
      Option(10).parseFastInto[String],
      Right("10")
    )
  }

  test("from.parseFastInto[To] and from.parseFullInto[To] should handle Eithers") {
    implicit val intToSting: Parser[Int, String] = Parser.instance(i => Right(i.toString))
    assertEquals((Left(10): Either[Int, Int]).parseFastInto[Either[String, String]], Right(Left("10")))
    assertEquals((Right(10): Either[Int, Int]).parseFastInto[Either[String, String]], Right(Right("10")))
    assertEquals(Left(10).parseFastInto[Left[String, Nothing]], Right(Left("10")))
    assertEquals(Right(10).parseFastInto[Right[Nothing, String]], Right(Right("10")))
  }

  test("from.parseFastInto[To] and from.parseFullInto[To] should handle collections") {
    implicit val intToSting: Parser[Int, String] = Parser.instance(i => Right(i.toString))
    assertEquals(
      List(1, 2, 3, 4).parseFastInto[Vector[String]],
      Right(Vector("1", "2", "3", "4"))
    )
  }

  test("from.parseFastInto[To] and from.parseFullInto[To] should handle maps") {
    implicit val intToSting: Parser[Int, String] = Parser.instance(i => Right(i.toString))
    assertEquals(
      HashMap(1 -> 1, 2 -> 2, 3 -> 3, 4 -> 4).parseFastInto[ListMap[String, String]],
      Right(ListMap("1" -> "1", "2" -> "2", "3" -> "3", "4" -> "4"))
    )
  }

  test("from.parseFastInto[To] and from.parseFullInto[To] should handle automatic derivation") {
    import pipez.{ `Backtick ADT In`, `Backtick ADT Out` }
    assertEquals(
      (`Backtick ADT In`.`Case Class`("test"): `Backtick ADT In`).parseFastInto[`Backtick ADT Out`],
      Right(`Backtick ADT Out`.`Case Class`("test"))
    )
  }

  test("from.parseFastInto[To] and from.parseFullInto[To] should handle configured derivation") {
    import pipez.{ CaseParamIn, CaseParamOutExt }
    implicit val intToSting: Parser[Int, String] = Parser.instance(i => Right(i.toString))
    assertEquals(
      CaseParamIn(1, "test", 2).parseFastInto[CaseParamOutExt[String]](
        Parser.Config[CaseParamIn[Int], CaseParamOutExt[String]].addField(_.x, (_, _, _) => Right("3"))
      ),
      Right(CaseParamOutExt(1, "test", "2", "3"))
    )
  }

  test("from.parseFastInto[To] and from.parseFullInto[To] should provide error paths on failures") {
    import pipez.{ CaseParamIn, CaseParamOutExt }
    import Parser.PathSegment.*
    implicit val stringToInt: Parser[String, Int] =
      Parser.instance(i => scala.util.Try(i.toInt).toEither.left.map(_ => Vector(s"$i is not integer")))
    assertEquals(
      CaseParamIn(1, "test", Map("x" -> "y")).parseFastInto[CaseParamOutExt[Map[Int, Int]]](
        Parser
          .Config[CaseParamIn[Map[String, String]], CaseParamOutExt[Map[Int, Int]]]
          .addField(_.x, (_, _, _) => Right(Map(4 -> 4)))
      ),
      Left(ListMap(Vector(AtField("c"), WithKey("x")) -> Vector("x is not integer")))
    )
    assertEquals(
      CaseParamIn(1, "test", Map("x" -> "y")).parseFullInto[CaseParamOutExt[Map[Int, Int]]](
        Parser
          .Config[CaseParamIn[Map[String, String]], CaseParamOutExt[Map[Int, Int]]]
          .addField(_.x, (_, _, _) => Right(Map(4 -> 4)))
      ),
      Left(
        ListMap(
          Vector(AtField("c"), WithKey("x")) -> Vector("x is not integer"),
          Vector(AtField("c"), AtKey("x")) -> Vector("y is not integer")
        )
      )
    )
  }
}
