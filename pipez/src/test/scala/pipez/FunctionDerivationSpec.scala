package pipez

import scala.util.chaining.*

class FunctionDerivationSpec extends munit.FunSuite {

  test("In => Out derivation should work out of the box") {
    // default constructor -> default constructor
    assertEquals(
      PipeDerivation.derive[_ => _, ZeroIn, ZeroOut].apply(ZeroIn()),
      ZeroOut()
    )
    // case class -> case class
    assertEquals(
      PipeDerivation.derive[_ => _, CaseOnesIn, CaseOnesOut].apply(CaseOnesIn(1)),
      CaseOnesOut(1)
    )
    assertEquals(
      PipeDerivation.derive[_ => _, CaseManyIn, CaseManyOut].apply(CaseManyIn(1, "a", 2L)),
      CaseManyOut(1, "a", 2L)
    )
    // case class -> Java Beans
    assertEquals(
      PipeDerivation.derive[_ => _, CaseOnesIn, BeanOnesOut].apply(CaseOnesIn(1)),
      new BeanOnesOut().tap(_.setA(1))
    )
    assertEquals(
      PipeDerivation.derive[_ => _, CaseManyIn, BeanManyOut].apply(CaseManyIn(1, "a", 2L)),
      new BeanManyOut().tap(_.setA(1)).tap(_.setB("a")).tap(_.setC(2L))
    )
    // Java Beans -> case class
    assertEquals(
      PipeDerivation.derive[_ => _, BeanOnesIn, CaseOnesOut].apply(new BeanOnesIn().tap(_.setA(1))),
      CaseOnesOut(1)
    )
    assertEquals(
      PipeDerivation
        .derive[_ => _, BeanManyIn, CaseManyOut]
        .apply(new BeanManyIn().tap(_.setA(1)).tap(_.setB("a")).tap(_.setC(2L))),
      CaseManyOut(1, "a", 2L)
    )
    // Java Beans -> Java Beans
    assertEquals(
      PipeDerivation.derive[_ => _, BeanOnesIn, BeanOnesOut].apply(new BeanOnesIn().tap(_.setA(1))),
      new BeanOnesOut().tap(_.setA(1))
    )
    assertEquals(
      PipeDerivation
        .derive[_ => _, BeanManyIn, BeanManyOut]
        .apply(new BeanManyIn().tap(_.setA(1)).tap(_.setB("a")).tap(_.setC(2L))),
      new BeanManyOut().tap(_.setA(1)).tap(_.setB("a")).tap(_.setC(2L))
    )
  }

  test("(Ctx, In) => Out derivation should work with a little help".format()) {
    final case class Ctx()
    implicit val ctxDerivation: PipeDerivation[(_, Ctx) => _] = PipeDerivation.contextFunction[Ctx]()
    // default constructor -> default constructor
    assertEquals(
      PipeDerivation.derive[(_, Ctx) => _, ZeroIn, ZeroOut].apply(ZeroIn(), Ctx()),
      ZeroOut()
    )
    // case class -> case class
    assertEquals(
      PipeDerivation.derive[(_, Ctx) => _, CaseOnesIn, CaseOnesOut].apply(CaseOnesIn(1), Ctx()),
      CaseOnesOut(1)
    )
    assertEquals(
      PipeDerivation.derive[(_, Ctx) => _, CaseManyIn, CaseManyOut].apply(CaseManyIn(1, "a", 2L), Ctx()),
      CaseManyOut(1, "a", 2L)
    )
    // case class -> Java Beans
    assertEquals(
      PipeDerivation.derive[(_, Ctx) => _, CaseOnesIn, BeanOnesOut].apply(CaseOnesIn(1), Ctx()),
      new BeanOnesOut().tap(_.setA(1))
    )
    assertEquals(
      PipeDerivation.derive[(_, Ctx) => _, CaseManyIn, BeanManyOut].apply(CaseManyIn(1, "a", 2L), Ctx()),
      new BeanManyOut().tap(_.setA(1)).tap(_.setB("a")).tap(_.setC(2L))
    )
    // Java Beans -> case class
    assertEquals(
      PipeDerivation.derive[(_, Ctx) => _, BeanOnesIn, CaseOnesOut].apply(new BeanOnesIn().tap(_.setA(1)), Ctx()),
      CaseOnesOut(1)
    )
    assertEquals(
      PipeDerivation
        .derive[(_, Ctx) => _, BeanManyIn, CaseManyOut]
        .apply(new BeanManyIn().tap(_.setA(1)).tap(_.setB("a")).tap(_.setC(2L)), Ctx()),
      CaseManyOut(1, "a", 2L)
    )
    // Java Beans -> Java Beans
    assertEquals(
      PipeDerivation.derive[(_, Ctx) => _, BeanOnesIn, BeanOnesOut].apply(new BeanOnesIn().tap(_.setA(1)), Ctx()),
      new BeanOnesOut().tap(_.setA(1))
    )
    assertEquals(
      PipeDerivation
        .derive[(_, Ctx) => _, BeanManyIn, BeanManyOut]
        .apply(new BeanManyIn().tap(_.setA(1)).tap(_.setB("a")).tap(_.setC(2L)), Ctx()),
      new BeanManyOut().tap(_.setA(1)).tap(_.setB("a")).tap(_.setC(2L))
    )
  }

  test("In => Out derivation should handle field conversion") {
    implicit val fun: Int => String = _.toString
    // case class -> case class
    assertEquals(
      PipeDerivation.derive[_ => _, CaseOnesIn, CaseOnesOutMod].apply(CaseOnesIn(1)),
      CaseOnesOutMod("1")
    )
    assertEquals(
      PipeDerivation.derive[_ => _, CaseManyIn, CaseManyOutMod].apply(CaseManyIn(1, "a", 2L)),
      CaseManyOutMod("1", "a", 2L)
    )
    // case class -> Java Beans
    assertEquals(
      PipeDerivation.derive[_ => _, CaseOnesIn, BeanOnesOutMod].apply(CaseOnesIn(1)),
      new BeanOnesOutMod().tap(_.setA("1"))
    )
    assertEquals(
      PipeDerivation.derive[_ => _, CaseManyIn, BeanManyOutMod].apply(CaseManyIn(1, "a", 2L)),
      new BeanManyOutMod().tap(_.setA("1")).tap(_.setB("a")).tap(_.setC(2L))
    )
    // Java Beans -> case class
    assertEquals(
      PipeDerivation.derive[_ => _, BeanOnesIn, CaseOnesOutMod].apply(new BeanOnesIn().tap(_.setA(1))),
      CaseOnesOutMod("1")
    )
    assertEquals(
      PipeDerivation
        .derive[_ => _, BeanManyIn, CaseManyOutMod]
        .apply(new BeanManyIn().tap(_.setA(1)).tap(_.setB("a")).tap(_.setC(2L))),
      CaseManyOutMod("1", "a", 2L)
    )
    // Java Beans -> Java Beans
    assertEquals(
      PipeDerivation.derive[_ => _, BeanOnesIn, BeanOnesOutMod].apply(new BeanOnesIn().tap(_.setA(1))),
      new BeanOnesOutMod().tap(_.setA("1"))
    )
    assertEquals(
      PipeDerivation
        .derive[_ => _, BeanManyIn, BeanManyOutMod]
        .apply(new BeanManyIn().tap(_.setA(1)).tap(_.setB("a")).tap(_.setC(2L))),
      new BeanManyOutMod().tap(_.setA("1")).tap(_.setB("a")).tap(_.setC(2L))
    )
  }
}
