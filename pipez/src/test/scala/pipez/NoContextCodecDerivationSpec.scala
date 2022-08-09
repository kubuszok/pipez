package pipez

import scala.util.chaining.scalaUtilChainingOps

// TODO: test backticked names like `a b`
// TODO: test conversion for types with type parameters

class NoContextCodecDerivationSpec extends munit.FunSuite {

  test("no config, no conversion -> use matching fields names") {
    // default constructor -> default constructor
    assertEquals(
      NoContextCodec.derive[ZeroIn, ZeroOut].decode(ZeroIn()),
      Right(ZeroOut())
    )
    // case class -> case class
    assertEquals(
      NoContextCodec.derive[CaseOnesIn, CaseOnesOut].decode(CaseOnesIn(1)),
      Right(CaseOnesOut(1))
    )
    assertEquals(
      NoContextCodec.derive[CaseManyIn, CaseManyOut].decode(CaseManyIn(1, "a", 2L)),
      Right(CaseManyOut(1, "a", 2L))
    )
    // case class -> Java Beans
    assertEquals(
      NoContextCodec.derive[CaseOnesIn, BeanOnesOut].decode(CaseOnesIn(1)),
      Right(new BeanOnesOut().tap(_.setA(1)))
    )
    assertEquals(
      NoContextCodec.derive[CaseManyIn, BeanManyOut].decode(CaseManyIn(1, "a", 2L)),
      Right(new BeanManyOut().tap(_.setA(1)).tap(_.setB("a")).tap(_.setC(2L)))
    )
    // Java Beans -> case class
    assertEquals(
      NoContextCodec.derive[BeanOnesIn, CaseOnesOut].decode(new BeanOnesIn().tap(_.setA(1))),
      Right(CaseOnesOut(1))
    )
    assertEquals(
      NoContextCodec
        .derive[BeanManyIn, CaseManyOut]
        .decode(new BeanManyIn().tap(_.setA(1)).tap(_.setB("a")).tap(_.setC(2L))),
      Right(CaseManyOut(1, "a", 2L))
    )
    // Java Beans -> Java Beans
    assertEquals(
      NoContextCodec.derive[BeanOnesIn, BeanOnesOut].decode(new BeanOnesIn().tap(_.setA(1))),
      Right(new BeanOnesOut().tap(_.setA(1)))
    )
    assertEquals(
      NoContextCodec
        .derive[BeanManyIn, BeanManyOut]
        .decode(new BeanManyIn().tap(_.setA(1)).tap(_.setB("a")).tap(_.setC(2L))),
      Right(new BeanManyOut().tap(_.setA(1)).tap(_.setB("a")).tap(_.setC(2L)))
    )
  }

  test("field conversion -> use implicit codec to convert field value if types differ but names match") {
    implicit val aCodec: NoContextCodec[Int, String] = int => Right(int.toString)
    // case class -> case class
    assertEquals(
      NoContextCodec.derive[CaseOnesIn, CaseOnesOutMod].decode(CaseOnesIn(1)),
      Right(CaseOnesOutMod("1"))
    )
    assertEquals(
      NoContextCodec.derive[CaseManyIn, CaseManyOutMod].decode(CaseManyIn(1, "a", 2L)),
      Right(CaseManyOutMod("1", "a", 2L))
    )
    // case class -> Java Beans
    assertEquals(
      NoContextCodec.derive[CaseOnesIn, BeanOnesOutMod].decode(CaseOnesIn(1)),
      Right(new BeanOnesOutMod().tap(_.setA("1")))
    )
    assertEquals(
      NoContextCodec.derive[CaseManyIn, BeanManyOutMod].decode(CaseManyIn(1, "a", 2L)),
      Right(new BeanManyOutMod().tap(_.setA("1")).tap(_.setB("a")).tap(_.setC(2L)))
    )
    // Java Beans -> case class
    assertEquals(
      NoContextCodec.derive[BeanOnesIn, CaseOnesOutMod].decode(new BeanOnesIn().tap(_.setA(1))),
      Right(CaseOnesOutMod("1"))
    )
    assertEquals(
      NoContextCodec
        .derive[BeanManyIn, CaseManyOutMod]
        .decode(new BeanManyIn().tap(_.setA(1)).tap(_.setB("a")).tap(_.setC(2L))),
      Right(CaseManyOutMod("1", "a", 2L))
    )
    // Java Beans -> Java Beans
    assertEquals(
      NoContextCodec.derive[BeanOnesIn, BeanOnesOutMod].decode(new BeanOnesIn().tap(_.setA(1))),
      Right(new BeanOnesOutMod().tap(_.setA("1")))
    )
    assertEquals(
      NoContextCodec
        .derive[BeanManyIn, BeanManyOutMod]
        .decode(new BeanManyIn().tap(_.setA(1)).tap(_.setB("a")).tap(_.setC(2L))),
      Right(new BeanManyOutMod().tap(_.setA("1")).tap(_.setB("a")).tap(_.setC(2L)))
    )
  }

  test("addField config -> create output field value from whole input object") {
    // default constructor -> case class/Java Beans
    assertEquals(
      NoContextCodec
        .derive(
          PipeDerivationConfig[NoContextCodec, ZeroIn, CaseZeroOutExt].addField(_.x, (in: ZeroIn) => Right(in.toString))
        )
        .decode(ZeroIn()),
      Right(CaseZeroOutExt("ZeroIn()"))
    )
    assertEquals(
      NoContextCodec
        .derive(
          PipeDerivationConfig[NoContextCodec, ZeroIn, BeanZeroOutExt].addField(_.getX(),
                                                                                (in: ZeroIn) => Right(in.toString)
          )
        )
        .decode(ZeroIn()),
      Right(new BeanZeroOutExt().tap(_.setX("ZeroIn()")))
    )
    // case class -> case class
    assertEquals(
      NoContextCodec
        .derive(
          PipeDerivationConfig[NoContextCodec, CaseOnesIn, CaseOnesOutExt]
            .addField(_.x, (in: CaseOnesIn) => Right(in.toString))
        )
        .decode(CaseOnesIn(1)),
      Right(CaseOnesOutExt(1, "CaseOnesIn(1)"))
    )
    assertEquals(
      NoContextCodec
        .derive(
          PipeDerivationConfig[NoContextCodec, CaseManyIn, CaseManyOutExt]
            .addField(_.x, (in: CaseManyIn) => Right(in.toString))
        )
        .decode(CaseManyIn(1, "a", 2L)),
      Right(CaseManyOutExt(1, "a", 2L, "CaseManyIn(1,a,2)"))
    )
    // case class -> Java Beans
    assertEquals(
      NoContextCodec
        .derive(
          PipeDerivationConfig[NoContextCodec, CaseOnesIn, BeanOnesOutExt]
            .addField(_.x, (in: CaseOnesIn) => Right(in.toString))
        )
        .decode(CaseOnesIn(1)),
      Right(new BeanOnesOutExt().tap(_.setA(1)).tap(_.setX("CaseOnesIn(1)")))
    )
    assertEquals(
      NoContextCodec
        .derive(
          PipeDerivationConfig[NoContextCodec, CaseManyIn, BeanManyOutExt]
            .addField(_.x, (in: CaseManyIn) => Right(in.toString))
        )
        .decode(CaseManyIn(1, "a", 2L)),
      Right(new BeanManyOutExt().tap(_.setA(1)).tap(_.setB("a")).tap(_.setC(2L)).tap(_.setX("CaseManyIn(1,a,2)")))
    )
    // Java Beans -> case class
    assertEquals(
      NoContextCodec
        .derive(
          PipeDerivationConfig[NoContextCodec, BeanOnesIn, CaseOnesOutExt]
            .addField(_.x, (in: BeanOnesIn) => Right(in.toString))
        )
        .decode(new BeanOnesIn().tap(_.setA(1))),
      Right(CaseOnesOutExt(1, "BeanOnesIn(1)"))
    )
    assertEquals(
      NoContextCodec
        .derive(
          PipeDerivationConfig[NoContextCodec, BeanManyIn, CaseManyOutExt]
            .addField(_.x, (in: BeanManyIn) => Right(in.toString))
        )
        .decode(new BeanManyIn().tap(_.setA(1)).tap(_.setB("a")).tap(_.setC(2L))),
      Right(CaseManyOutExt(1, "a", 2L, "BeanManyIn(1,a,2)"))
    )
    // Java Beans -> Java Beans
    assertEquals(
      NoContextCodec
        .derive(
          PipeDerivationConfig[NoContextCodec, BeanOnesIn, BeanOnesOutExt]
            .addField(_.getX(), (in: BeanOnesIn) => Right(in.toString))
        )
        .decode(new BeanOnesIn().tap(_.setA(1))),
      Right(new BeanOnesOutExt().tap(_.setA(1)).tap(_.setX("BeanOnesIn(1)")))
    )
    assertEquals(
      NoContextCodec
        .derive(
          PipeDerivationConfig[NoContextCodec, BeanManyIn, BeanManyOutExt]
            .addField(_.getX(), (in: BeanManyIn) => Right(in.toString))
        )
        .decode(new BeanManyIn().tap(_.setA(1)).tap(_.setB("a")).tap(_.setC(2L))),
      Right(new BeanManyOutExt().tap(_.setA(1)).tap(_.setB("a")).tap(_.setC(2L)).tap(_.setX("BeanManyIn(1,a,2)")))
    )
  }

  test("plugIn config -> create output field value from input field using explicitly passed codec") {
    // case class -> case class
    assertEquals(
      NoContextCodec
        .derive(
          PipeDerivationConfig[NoContextCodec, CaseOnesIn, CaseOnesOutExt]
            .plugInField(_.a, _.x, (a: Int) => Right(a.toString))
        )
        .decode(CaseOnesIn(1)),
      Right(CaseOnesOutExt(1, "1"))
    )
    assertEquals(
      NoContextCodec
        .derive(
          PipeDerivationConfig[NoContextCodec, CaseManyIn, CaseManyOutExt]
            .plugInField(_.a, _.x, (a: Int) => Right(a.toString))
        )
        .decode(CaseManyIn(1, "a", 2L)),
      Right(CaseManyOutExt(1, "a", 2L, "1"))
    )
    // case class -> Java Beans
    assertEquals(
      NoContextCodec
        .derive(
          PipeDerivationConfig[NoContextCodec, CaseOnesIn, BeanOnesOutExt]
            .plugInField(_.a, _.getX(), (a: Int) => Right(a.toString))
        )
        .decode(CaseOnesIn(1)),
      Right(new BeanOnesOutExt().tap(_.setA(1)).tap(_.setX("1")))
    )
    assertEquals(
      NoContextCodec
        .derive(
          PipeDerivationConfig[NoContextCodec, CaseManyIn, BeanManyOutExt]
            .plugInField(_.a, _.getX(), (a: Int) => Right(a.toString))
        )
        .decode(CaseManyIn(1, "a", 2L)),
      Right(new BeanManyOutExt().tap(_.setA(1)).tap(_.setB("a")).tap(_.setC(2L)).tap(_.setX("1")))
    )
    // Java Beans -> case class
    assertEquals(
      NoContextCodec
        .derive(
          PipeDerivationConfig[NoContextCodec, BeanOnesIn, CaseOnesOutExt]
            .plugInField(_.getA(), _.x, (a: Int) => Right(a.toString))
        )
        .decode(new BeanOnesIn().tap(_.setA(1))),
      Right(CaseOnesOutExt(1, "1"))
    )
    assertEquals(
      NoContextCodec
        .derive(
          PipeDerivationConfig[NoContextCodec, BeanManyIn, CaseManyOutExt]
            .plugInField(_.getA(), _.x, (a: Int) => Right(a.toString))
        )
        .decode(new BeanManyIn().tap(_.setA(1)).tap(_.setB("a")).tap(_.setC(2L))),
      Right(CaseManyOutExt(1, "a", 2L, "1"))
    )
    // Java Beans -> Java Beans
    assertEquals(
      NoContextCodec
        .derive(
          PipeDerivationConfig[NoContextCodec, BeanOnesIn, BeanOnesOutExt]
            .plugInField(_.getA(), _.getX(), (a: Int) => Right(a.toString))
        )
        .decode(new BeanOnesIn().tap(_.setA(1))),
      Right(new BeanOnesOutExt().tap(_.setA(1)).tap(_.setX("1")))
    )
    assertEquals(
      NoContextCodec
        .derive(
          PipeDerivationConfig[NoContextCodec, BeanManyIn, BeanManyOutExt]
            .plugInField(_.getA(), _.getX(), (a: Int) => Right(a.toString))
        )
        .decode(new BeanManyIn().tap(_.setA(1)).tap(_.setB("a")).tap(_.setC(2L))),
      Right(new BeanManyOutExt().tap(_.setA(1)).tap(_.setB("a")).tap(_.setC(2L)).tap(_.setX("1")))
    )
  }

  test("renameField config + conversion -> output field value taken from specified input field and converted") {
    implicit val aCodec: NoContextCodec[Int, String] = int => Right(int.toString)
    // case class -> case class
    assertEquals(
      NoContextCodec
        .derive(PipeDerivationConfig[NoContextCodec, CaseOnesIn, CaseOnesOutExt].renameField(_.a, _.x))
        .decode(CaseOnesIn(1)),
      Right(CaseOnesOutExt(1, "1"))
    )
    assertEquals(
      NoContextCodec
        .derive(PipeDerivationConfig[NoContextCodec, CaseManyIn, CaseManyOutExt].renameField(_.a, _.x))
        .decode(CaseManyIn(1, "a", 2L)),
      Right(CaseManyOutExt(1, "a", 2L, "1"))
    )
    // case class -> Java Beans
    assertEquals(
      NoContextCodec
        .derive(PipeDerivationConfig[NoContextCodec, CaseOnesIn, BeanOnesOutExt].renameField(_.a, _.getX()))
        .decode(CaseOnesIn(1)),
      Right(new BeanOnesOutExt().tap(_.setA(1)).tap(_.setX("1")))
    )
    assertEquals(
      NoContextCodec
        .derive(PipeDerivationConfig[NoContextCodec, CaseManyIn, BeanManyOutExt].renameField(_.a, _.getX()))
        .decode(CaseManyIn(1, "a", 2L)),
      Right(new BeanManyOutExt().tap(_.setA(1)).tap(_.setB("a")).tap(_.setC(2L)).tap(_.setX("1")))
    )
    // Java Beans -> case class
    assertEquals(
      NoContextCodec
        .derive(PipeDerivationConfig[NoContextCodec, BeanOnesIn, CaseOnesOutExt].renameField(_.getA(), _.x))
        .decode(new BeanOnesIn().tap(_.setA(1))),
      Right(CaseOnesOutExt(1, "1"))
    )
    assertEquals(
      NoContextCodec
        .derive(PipeDerivationConfig[NoContextCodec, BeanManyIn, CaseManyOutExt].renameField(_.getA(), _.x))
        .decode(new BeanManyIn().tap(_.setA(1)).tap(_.setB("a")).tap(_.setC(2L))),
      Right(CaseManyOutExt(1, "a", 2L, "1"))
    )
    // Java Beans -> Java Beans
    assertEquals(
      NoContextCodec
        .derive(PipeDerivationConfig[NoContextCodec, BeanOnesIn, BeanOnesOutExt].renameField(_.getA(), _.getX()))
        .decode(new BeanOnesIn().tap(_.setA(1))),
      Right(new BeanOnesOutExt().tap(_.setA(1)).tap(_.setX("1")))
    )
    assertEquals(
      NoContextCodec
        .derive(PipeDerivationConfig[NoContextCodec, BeanManyIn, BeanManyOutExt].renameField(_.getA(), _.getX()))
        .decode(new BeanManyIn().tap(_.setA(1)).tap(_.setB("a")).tap(_.setC(2L))),
      Right(new BeanManyOutExt().tap(_.setA(1)).tap(_.setB("a")).tap(_.setC(2L)).tap(_.setX("1")))
    )
  }

  test("fieldMatchingCaseInsensitive -> matching of input to output field names is case-insensitive") {
    // case class -> case class
    assertEquals(
      NoContextCodec
        .derive(PipeDerivationConfig[NoContextCodec, CaseLower, CaseUpper].fieldMatchingCaseInsensitive)
        .decode(CaseLower(1, "a", 2L)),
      Right(CaseUpper(1, "a", 2L))
    )
    // case class -> Java Beans
    assertEquals(
      NoContextCodec
        .derive(PipeDerivationConfig[NoContextCodec, CaseLower, BeanUpper].fieldMatchingCaseInsensitive)
        .decode(CaseLower(1, "a", 2L)),
      Right(new BeanUpper().tap(_.setAAA(1)).tap(_.setBBB("a")).tap(_.setCCC(2L)))
    )
    // Java Beans -> case class
    assertEquals(
      NoContextCodec
        .derive(PipeDerivationConfig[NoContextCodec, CaseLower, BeanUpper].fieldMatchingCaseInsensitive)
        .decode(CaseLower(1, "a", 2L)),
      Right(new BeanUpper().tap(_.setAAA(1)).tap(_.setBBB("a")).tap(_.setCCC(2L)))
    )
    // Java Beans -> Java Beans
    assertEquals(
      NoContextCodec
        .derive(PipeDerivationConfig[NoContextCodec, BeanLower, BeanUpper].fieldMatchingCaseInsensitive)
        .decode(new BeanLower().tap(_.setAaa(1)).tap(_.setBbb("a")).tap(_.setCcc(2L))),
      Right(new BeanUpper().tap(_.setAAA(1)).tap(_.setBBB("a")).tap(_.setCCC(2L)))
    )
  }

  test("no config, no implicits -> use matching subtypes") {
    import NoContextCodec.Auto.*
    // case object only in ADT
    assertEquals(
      NoContextCodec.derive[ADTObjectsIn, ADTObjectsOut].decode(ADTObjectsIn.B),
      Right(ADTObjectsOut.B)
    )
    // case classes in ADT
    assertEquals(
      NoContextCodec.derive[ADTClassesIn, ADTClassesOut].decode(ADTClassesIn.B(1)),
      Right(ADTClassesOut.B(1))
    )
    // smaller -> bigger
    // TODO
  }
}
