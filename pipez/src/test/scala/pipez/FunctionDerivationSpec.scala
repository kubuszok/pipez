package pipez

import scala.util.chaining.*

class FunctionDerivationSpec extends munit.FunSuite {

  test("In => Out derivation should work out of the box") {
    // default constructor -> default constructor
    assertEquals(
      PipeDerivation.derive[Function1, ZeroIn, ZeroOut].apply(ZeroIn()),
      ZeroOut()
    )
    // case class -> case class
    assertEquals(
      PipeDerivation.derive[Function1, CaseOnesIn, CaseOnesOut].apply(CaseOnesIn(1)),
      CaseOnesOut(1)
    )
    assertEquals(
      PipeDerivation.derive[Function1, CaseManyIn, CaseManyOut].apply(CaseManyIn(1, "a", 2L)),
      CaseManyOut(1, "a", 2L)
    )
    // case class -> Java Beans
    assertEquals(
      PipeDerivation.derive[Function1, CaseOnesIn, BeanOnesOut].apply(CaseOnesIn(1)),
      new BeanOnesOut().tap(_.setA(1))
    )
    assertEquals(
      PipeDerivation.derive[Function1, CaseManyIn, BeanManyOut].apply(CaseManyIn(1, "a", 2L)),
      new BeanManyOut().tap(_.setA(1)).tap(_.setB("a")).tap(_.setC(2L))
    )
    // Java Beans -> case class
    assertEquals(
      PipeDerivation.derive[Function1, BeanOnesIn, CaseOnesOut].apply(new BeanOnesIn().tap(_.setA(1))),
      CaseOnesOut(1)
    )
    assertEquals(
      PipeDerivation
        .derive[Function1, BeanManyIn, CaseManyOut]
        .apply(new BeanManyIn().tap(_.setA(1)).tap(_.setB("a")).tap(_.setC(2L))),
      CaseManyOut(1, "a", 2L)
    )
    // Java Beans -> Java Beans
    assertEquals(
      PipeDerivation.derive[Function1, BeanOnesIn, BeanOnesOut].apply(new BeanOnesIn().tap(_.setA(1))),
      new BeanOnesOut().tap(_.setA(1))
    )
    assertEquals(
      PipeDerivation
        .derive[Function1, BeanManyIn, BeanManyOut]
        .apply(new BeanManyIn().tap(_.setA(1)).tap(_.setB("a")).tap(_.setC(2L))),
      new BeanManyOut().tap(_.setA(1)).tap(_.setB("a")).tap(_.setC(2L))
    )
  }

  test("(Ctx, In) => Out derivation should work with a little help".format()) {
    final case class Ctx()
    // only needed in Scala 2!
    implicit lazy val ctxDerivation: PipeDerivation[Function2[*, Ctx, *]] = PipeDerivation.contextFunction[Ctx]()
    // default constructor -> default constructor
    assertEquals(
      PipeDerivation.derive[Function2[*, Ctx, *], ZeroIn, ZeroOut].apply(ZeroIn(), Ctx()),
      ZeroOut()
    )
    // case class -> case class
    assertEquals(
      PipeDerivation.derive[Function2[*, Ctx, *], CaseOnesIn, CaseOnesOut].apply(CaseOnesIn(1), Ctx()),
      CaseOnesOut(1)
    )
    assertEquals(
      PipeDerivation.derive[Function2[*, Ctx, *], CaseManyIn, CaseManyOut].apply(CaseManyIn(1, "a", 2L), Ctx()),
      CaseManyOut(1, "a", 2L)
    )
    // case class -> Java Beans
    assertEquals(
      PipeDerivation.derive[Function2[*, Ctx, *], CaseOnesIn, BeanOnesOut].apply(CaseOnesIn(1), Ctx()),
      new BeanOnesOut().tap(_.setA(1))
    )
    assertEquals(
      PipeDerivation.derive[Function2[*, Ctx, *], CaseManyIn, BeanManyOut].apply(CaseManyIn(1, "a", 2L), Ctx()),
      new BeanManyOut().tap(_.setA(1)).tap(_.setB("a")).tap(_.setC(2L))
    )
    // Java Beans -> case class
    assertEquals(
      PipeDerivation
        .derive[Function2[*, Ctx, *], BeanOnesIn, CaseOnesOut]
        .apply(new BeanOnesIn().tap(_.setA(1)), Ctx()),
      CaseOnesOut(1)
    )
    assertEquals(
      PipeDerivation
        .derive[Function2[*, Ctx, *], BeanManyIn, CaseManyOut]
        .apply(new BeanManyIn().tap(_.setA(1)).tap(_.setB("a")).tap(_.setC(2L)), Ctx()),
      CaseManyOut(1, "a", 2L)
    )
    // Java Beans -> Java Beans
    assertEquals(
      PipeDerivation
        .derive[Function2[*, Ctx, *], BeanOnesIn, BeanOnesOut]
        .apply(new BeanOnesIn().tap(_.setA(1)), Ctx()),
      new BeanOnesOut().tap(_.setA(1))
    )
    assertEquals(
      PipeDerivation
        .derive[Function2[*, Ctx, *], BeanManyIn, BeanManyOut]
        .apply(new BeanManyIn().tap(_.setA(1)).tap(_.setB("a")).tap(_.setC(2L)), Ctx()),
      new BeanManyOut().tap(_.setA(1)).tap(_.setB("a")).tap(_.setC(2L))
    )
  }

  test("In => Out derivation should handle field conversion") {
    implicit lazy val fun: Int => String = _.toString
    // case class -> case class
    assertEquals(
      PipeDerivation.derive[Function1, CaseOnesIn, CaseOnesOutMod].apply(CaseOnesIn(1)),
      CaseOnesOutMod("1")
    )
    assertEquals(
      PipeDerivation.derive[Function1, CaseManyIn, CaseManyOutMod].apply(CaseManyIn(1, "a", 2L)),
      CaseManyOutMod("1", "a", 2L)
    )
    // case class -> Java Beans
    assertEquals(
      PipeDerivation.derive[Function1, CaseOnesIn, BeanOnesOutMod].apply(CaseOnesIn(1)),
      new BeanOnesOutMod().tap(_.setA("1"))
    )
    assertEquals(
      PipeDerivation.derive[Function1, CaseManyIn, BeanManyOutMod].apply(CaseManyIn(1, "a", 2L)),
      new BeanManyOutMod().tap(_.setA("1")).tap(_.setB("a")).tap(_.setC(2L))
    )
    // Java Beans -> case class
    assertEquals(
      PipeDerivation.derive[Function1, BeanOnesIn, CaseOnesOutMod].apply(new BeanOnesIn().tap(_.setA(1))),
      CaseOnesOutMod("1")
    )
    assertEquals(
      PipeDerivation
        .derive[Function1, BeanManyIn, CaseManyOutMod]
        .apply(new BeanManyIn().tap(_.setA(1)).tap(_.setB("a")).tap(_.setC(2L))),
      CaseManyOutMod("1", "a", 2L)
    )
    // Java Beans -> Java Beans
    assertEquals(
      PipeDerivation.derive[Function1, BeanOnesIn, BeanOnesOutMod].apply(new BeanOnesIn().tap(_.setA(1))),
      new BeanOnesOutMod().tap(_.setA("1"))
    )
    assertEquals(
      PipeDerivation
        .derive[Function1, BeanManyIn, BeanManyOutMod]
        .apply(new BeanManyIn().tap(_.setA(1)).tap(_.setB("a")).tap(_.setC(2L))),
      new BeanManyOutMod().tap(_.setA("1")).tap(_.setB("a")).tap(_.setC(2L))
    )
  }
}
