package pipez

import scala.util.chaining.*

class ContextCodecDerivationSpec extends munit.FunSuite {

  test("no config, no conversion -> use matching fields names") {
    // default constructor -> default constructor
    assertEquals(
      ContextCodec.derive[ZeroIn, ZeroOut].decode(ZeroIn(), shouldFailFast = false, path = "root"),
      Right(ZeroOut())
    )
    // case class -> case class
    assertEquals(
      ContextCodec.derive[CaseOnesIn, CaseOnesOut].decode(CaseOnesIn(1), shouldFailFast = false, path = "root"),
      Right(CaseOnesOut(1))
    )
    assertEquals(
      ContextCodec
        .derive[CaseManyIn, CaseManyOut]
        .decode(CaseManyIn(1, "a", 2L), shouldFailFast = false, path = "root"),
      Right(CaseManyOut(1, "a", 2L))
    )
    // case class -> Java Beans
    assertEquals(
      ContextCodec.derive[CaseOnesIn, BeanOnesOut].decode(CaseOnesIn(1), shouldFailFast = false, path = "root"),
      Right(new BeanOnesOut().tap(_.setA(1)))
    )
    assertEquals(
      ContextCodec
        .derive[CaseManyIn, BeanManyOut]
        .decode(CaseManyIn(1, "a", 2L), shouldFailFast = false, path = "root"),
      Right(new BeanManyOut().tap(_.setA(1)).tap(_.setB("a")).tap(_.setC(2L)))
    )
    // Java Beans -> case class
    assertEquals(
      ContextCodec
        .derive[BeanOnesIn, CaseOnesOut]
        .decode(new BeanOnesIn().tap(_.setA(1)), shouldFailFast = false, path = "root"),
      Right(CaseOnesOut(1))
    )
    assertEquals(
      ContextCodec
        .derive[BeanManyIn, CaseManyOut]
        .decode(new BeanManyIn().tap(_.setA(1)).tap(_.setB("a")).tap(_.setC(2L)),
                shouldFailFast = false,
                path = "root"
        ),
      Right(CaseManyOut(1, "a", 2L))
    )
    // Java Beans -> Java Beans
    assertEquals(
      ContextCodec
        .derive[BeanOnesIn, BeanOnesOut]
        .decode(new BeanOnesIn().tap(_.setA(1)), shouldFailFast = false, path = "root"),
      Right(new BeanOnesOut().tap(_.setA(1)))
    )
    assertEquals(
      ContextCodec
        .derive[BeanManyIn, BeanManyOut]
        .decode(new BeanManyIn().tap(_.setA(1)).tap(_.setB("a")).tap(_.setC(2L)),
                shouldFailFast = false,
                path = "root"
        ),
      Right(new BeanManyOut().tap(_.setA(1)).tap(_.setB("a")).tap(_.setC(2L)))
    )
  }

  test("field conversion -> use implicit codec to convert field value if types differ but names match") {
    implicit val aCodec: ContextCodec[Int, String] = (int: Int, _: Boolean, _: String) => Right(int.toString)
    // case class -> case class
    assertEquals(
      ContextCodec.derive[CaseOnesIn, CaseOnesOutMod].decode(CaseOnesIn(1), shouldFailFast = false, path = "root"),
      Right(CaseOnesOutMod("1"))
    )
    assertEquals(
      ContextCodec
        .derive[CaseManyIn, CaseManyOutMod]
        .decode(CaseManyIn(1, "a", 2L), shouldFailFast = false, path = "root"),
      Right(CaseManyOutMod("1", "a", 2L))
    )
    // case class -> Java Beans
    assertEquals(
      ContextCodec.derive[CaseOnesIn, BeanOnesOutMod].decode(CaseOnesIn(1), shouldFailFast = false, path = "root"),
      Right(new BeanOnesOutMod().tap(_.setA("1")))
    )
    assertEquals(
      ContextCodec
        .derive[CaseManyIn, BeanManyOutMod]
        .decode(CaseManyIn(1, "a", 2L), shouldFailFast = false, path = "root"),
      Right(new BeanManyOutMod().tap(_.setA("1")).tap(_.setB("a")).tap(_.setC(2L)))
    )
    // Java Beans -> case class
    assertEquals(
      ContextCodec
        .derive[BeanOnesIn, CaseOnesOutMod]
        .decode(new BeanOnesIn().tap(_.setA(1)), shouldFailFast = false, path = "root"),
      Right(CaseOnesOutMod("1"))
    )
    assertEquals(
      ContextCodec
        .derive[BeanManyIn, CaseManyOutMod]
        .decode(new BeanManyIn().tap(_.setA(1)).tap(_.setB("a")).tap(_.setC(2L)),
                shouldFailFast = false,
                path = "root"
        ),
      Right(CaseManyOutMod("1", "a", 2L))
    )
    // Java Beans -> Java Beans
    assertEquals(
      ContextCodec
        .derive[BeanOnesIn, BeanOnesOutMod]
        .decode(new BeanOnesIn().tap(_.setA(1)), shouldFailFast = false, path = "root"),
      Right(new BeanOnesOutMod().tap(_.setA("1")))
    )
    assertEquals(
      ContextCodec
        .derive[BeanManyIn, BeanManyOutMod]
        .decode(new BeanManyIn().tap(_.setA(1)).tap(_.setB("a")).tap(_.setC(2L)),
                shouldFailFast = false,
                path = "root"
        ),
      Right(new BeanManyOutMod().tap(_.setA("1")).tap(_.setB("a")).tap(_.setC(2L)))
    )
  }

  test("addField config -> create output field value from whole input object") {
    // default constructor -> case class/Java Beans
    assertEquals(
      ContextCodec
        .derive(
          ContextCodec
            .Config[ZeroIn, CaseZeroOutExt]
            .addField(_.x, (in: ZeroIn, _: Boolean, _: String) => Right(in.toString))
        )
        .decode(ZeroIn(), shouldFailFast = false, path = "root"),
      Right(CaseZeroOutExt("ZeroIn()"))
    )
    assertEquals(
      ContextCodec
        .derive(
          ContextCodec
            .Config[ZeroIn, BeanZeroOutExt]
            .addField(_.getX(), (in: ZeroIn, _: Boolean, _: String) => Right(in.toString))
        )
        .decode(ZeroIn(), shouldFailFast = false, path = "root"),
      Right(new BeanZeroOutExt().tap(_.setX("ZeroIn()")))
    )
    // case class -> case class
    assertEquals(
      ContextCodec
        .derive(
          ContextCodec
            .Config[CaseOnesIn, CaseOnesOutExt]
            .addField(_.x, (in: CaseOnesIn, _: Boolean, _: String) => Right(in.toString))
        )
        .decode(CaseOnesIn(1), shouldFailFast = false, path = "root"),
      Right(CaseOnesOutExt(1, "CaseOnesIn(1)"))
    )
    assertEquals(
      ContextCodec
        .derive(
          ContextCodec
            .Config[CaseManyIn, CaseManyOutExt]
            .addField(_.x, (in: CaseManyIn, _: Boolean, _: String) => Right(in.toString))
        )
        .decode(CaseManyIn(1, "a", 2L), shouldFailFast = false, path = "root"),
      Right(CaseManyOutExt(1, "a", 2L, "CaseManyIn(1,a,2)"))
    )
    // case class -> Java Beans
    assertEquals(
      ContextCodec
        .derive(
          ContextCodec
            .Config[CaseOnesIn, BeanOnesOutExt]
            .addField(_.x, (in: CaseOnesIn, _: Boolean, _: String) => Right(in.toString))
        )
        .decode(CaseOnesIn(1), shouldFailFast = false, path = "root"),
      Right(new BeanOnesOutExt().tap(_.setA(1)).tap(_.setX("CaseOnesIn(1)")))
    )
    assertEquals(
      ContextCodec
        .derive(
          ContextCodec
            .Config[CaseManyIn, BeanManyOutExt]
            .addField(_.x, (in: CaseManyIn, _: Boolean, _: String) => Right(in.toString))
        )
        .decode(CaseManyIn(1, "a", 2L), shouldFailFast = false, path = "root"),
      Right(new BeanManyOutExt().tap(_.setA(1)).tap(_.setB("a")).tap(_.setC(2L)).tap(_.setX("CaseManyIn(1,a,2)")))
    )
    // Java Beans -> case class
    assertEquals(
      ContextCodec
        .derive(
          ContextCodec
            .Config[BeanOnesIn, CaseOnesOutExt]
            .addField(_.x, (in: BeanOnesIn, _: Boolean, _: String) => Right(in.toString))
        )
        .decode(new BeanOnesIn().tap(_.setA(1)), shouldFailFast = false, path = "root"),
      Right(CaseOnesOutExt(1, "BeanOnesIn(1)"))
    )
    assertEquals(
      ContextCodec
        .derive(
          ContextCodec
            .Config[BeanManyIn, CaseManyOutExt]
            .addField(_.x, (in: BeanManyIn, _: Boolean, _: String) => Right(in.toString))
        )
        .decode(new BeanManyIn().tap(_.setA(1)).tap(_.setB("a")).tap(_.setC(2L)),
                shouldFailFast = false,
                path = "root"
        ),
      Right(CaseManyOutExt(1, "a", 2L, "BeanManyIn(1,a,2)"))
    )
    // Java Beans -> Java Beans
    assertEquals(
      ContextCodec
        .derive(
          ContextCodec
            .Config[BeanOnesIn, BeanOnesOutExt]
            .addField(_.getX(), (in: BeanOnesIn, _: Boolean, _: String) => Right(in.toString))
        )
        .decode(new BeanOnesIn().tap(_.setA(1)), shouldFailFast = false, path = "root"),
      Right(new BeanOnesOutExt().tap(_.setA(1)).tap(_.setX("BeanOnesIn(1)")))
    )
    assertEquals(
      ContextCodec
        .derive(
          ContextCodec
            .Config[BeanManyIn, BeanManyOutExt]
            .addField(_.getX(), (in: BeanManyIn, _: Boolean, _: String) => Right(in.toString))
        )
        .decode(new BeanManyIn().tap(_.setA(1)).tap(_.setB("a")).tap(_.setC(2L)),
                shouldFailFast = false,
                path = "root"
        ),
      Right(new BeanManyOutExt().tap(_.setA(1)).tap(_.setB("a")).tap(_.setC(2L)).tap(_.setX("BeanManyIn(1,a,2)")))
    )
  }

  test("plugInField config -> create output field value from input field using explicitly passed codec") {
    // case class -> case class
    assertEquals(
      ContextCodec
        .derive(
          ContextCodec
            .Config[CaseOnesIn, CaseOnesOutExt]
            .plugInField(_.a, _.x, (a: Int, _: Boolean, _: String) => Right(a.toString))
        )
        .decode(CaseOnesIn(1), shouldFailFast = false, path = "root"),
      Right(CaseOnesOutExt(1, "1"))
    )
    assertEquals(
      ContextCodec
        .derive(
          ContextCodec
            .Config[CaseManyIn, CaseManyOutExt]
            .plugInField(_.a, _.x, (a: Int, _: Boolean, _: String) => Right(a.toString))
        )
        .decode(CaseManyIn(1, "a", 2L), shouldFailFast = false, path = "root"),
      Right(CaseManyOutExt(1, "a", 2L, "1"))
    )
    // case class -> Java Beans
    assertEquals(
      ContextCodec
        .derive(
          ContextCodec
            .Config[CaseOnesIn, BeanOnesOutExt]
            .plugInField(_.a, _.getX(), (a: Int, _: Boolean, _: String) => Right(a.toString))
        )
        .decode(CaseOnesIn(1), shouldFailFast = false, path = "root"),
      Right(new BeanOnesOutExt().tap(_.setA(1)).tap(_.setX("1")))
    )
    assertEquals(
      ContextCodec
        .derive(
          ContextCodec
            .Config[CaseManyIn, BeanManyOutExt]
            .plugInField(_.a, _.getX(), (a: Int, _: Boolean, _: String) => Right(a.toString))
        )
        .decode(CaseManyIn(1, "a", 2L), shouldFailFast = false, path = "root"),
      Right(new BeanManyOutExt().tap(_.setA(1)).tap(_.setB("a")).tap(_.setC(2L)).tap(_.setX("1")))
    )
    // Java Beans -> case class
    assertEquals(
      ContextCodec
        .derive(
          ContextCodec
            .Config[BeanOnesIn, CaseOnesOutExt]
            .plugInField(_.getA(), _.x, (a: Int, _: Boolean, _: String) => Right(a.toString))
        )
        .decode(new BeanOnesIn().tap(_.setA(1)), shouldFailFast = false, path = "root"),
      Right(CaseOnesOutExt(1, "1"))
    )
    assertEquals(
      ContextCodec
        .derive(
          ContextCodec
            .Config[BeanManyIn, CaseManyOutExt]
            .plugInField(_.getA(), _.x, (a: Int, _: Boolean, _: String) => Right(a.toString))
        )
        .decode(new BeanManyIn().tap(_.setA(1)).tap(_.setB("a")).tap(_.setC(2L)),
                shouldFailFast = false,
                path = "root"
        ),
      Right(CaseManyOutExt(1, "a", 2L, "1"))
    )
    // Java Beans -> Java Beans
    assertEquals(
      ContextCodec
        .derive(
          ContextCodec
            .Config[BeanOnesIn, BeanOnesOutExt]
            .plugInField(_.getA(), _.getX(), (a: Int, _: Boolean, _: String) => Right(a.toString))
        )
        .decode(new BeanOnesIn().tap(_.setA(1)), shouldFailFast = false, path = "root"),
      Right(new BeanOnesOutExt().tap(_.setA(1)).tap(_.setX("1")))
    )
    assertEquals(
      ContextCodec
        .derive(
          ContextCodec
            .Config[BeanManyIn, BeanManyOutExt]
            .plugInField(_.getA(), _.getX(), (a: Int, _: Boolean, _: String) => Right(a.toString))
        )
        .decode(new BeanManyIn().tap(_.setA(1)).tap(_.setB("a")).tap(_.setC(2L)),
                shouldFailFast = false,
                path = "root"
        ),
      Right(new BeanManyOutExt().tap(_.setA(1)).tap(_.setB("a")).tap(_.setC(2L)).tap(_.setX("1")))
    )
  }

  test("renameField config + conversion -> output field value taken from specified input field and converted") {
    implicit val aCodec: ContextCodec[Int, String] = (int: Int, _: Boolean, _: String) => Right(int.toString)
    // case class -> case class
    assertEquals(
      ContextCodec
        .derive(ContextCodec.Config[CaseOnesIn, CaseOnesOutExt].renameField(_.a, _.x))
        .decode(CaseOnesIn(1), shouldFailFast = false, path = "root"),
      Right(CaseOnesOutExt(1, "1"))
    )
    assertEquals(
      ContextCodec
        .derive(ContextCodec.Config[CaseManyIn, CaseManyOutExt].renameField(_.a, _.x))
        .decode(CaseManyIn(1, "a", 2L), shouldFailFast = false, path = "root"),
      Right(CaseManyOutExt(1, "a", 2L, "1"))
    )
    // case class -> Java Beans
    assertEquals(
      ContextCodec
        .derive(ContextCodec.Config[CaseOnesIn, BeanOnesOutExt].renameField(_.a, _.getX()))
        .decode(CaseOnesIn(1), shouldFailFast = false, path = "root"),
      Right(new BeanOnesOutExt().tap(_.setA(1)).tap(_.setX("1")))
    )
    assertEquals(
      ContextCodec
        .derive(ContextCodec.Config[CaseManyIn, BeanManyOutExt].renameField(_.a, _.getX()))
        .decode(CaseManyIn(1, "a", 2L), shouldFailFast = false, path = "root"),
      Right(new BeanManyOutExt().tap(_.setA(1)).tap(_.setB("a")).tap(_.setC(2L)).tap(_.setX("1")))
    )
    // Java Beans -> case class
    assertEquals(
      ContextCodec
        .derive(ContextCodec.Config[BeanOnesIn, CaseOnesOutExt].renameField(_.getA(), _.x))
        .decode(new BeanOnesIn().tap(_.setA(1)), shouldFailFast = false, path = "root"),
      Right(CaseOnesOutExt(1, "1"))
    )
    assertEquals(
      ContextCodec
        .derive(ContextCodec.Config[BeanManyIn, CaseManyOutExt].renameField(_.getA(), _.x))
        .decode(new BeanManyIn().tap(_.setA(1)).tap(_.setB("a")).tap(_.setC(2L)),
                shouldFailFast = false,
                path = "root"
        ),
      Right(CaseManyOutExt(1, "a", 2L, "1"))
    )
    // Java Beans -> Java Beans
    assertEquals(
      ContextCodec
        .derive(ContextCodec.Config[BeanOnesIn, BeanOnesOutExt].renameField(_.getA(), _.getX()))
        .decode(new BeanOnesIn().tap(_.setA(1)), shouldFailFast = false, path = "root"),
      Right(new BeanOnesOutExt().tap(_.setA(1)).tap(_.setX("1")))
    )
    assertEquals(
      ContextCodec
        .derive(ContextCodec.Config[BeanManyIn, BeanManyOutExt].renameField(_.getA(), _.getX()))
        .decode(new BeanManyIn().tap(_.setA(1)).tap(_.setB("a")).tap(_.setC(2L)),
                shouldFailFast = false,
                path = "root"
        ),
      Right(new BeanManyOutExt().tap(_.setA(1)).tap(_.setB("a")).tap(_.setC(2L)).tap(_.setX("1")))
    )
  }

  test("fieldMatchingCaseInsensitive -> matching of input to output field names is case-insensitive") {
    // case class -> case class
    assertEquals(
      ContextCodec
        .derive(ContextCodec.Config[CaseLower, CaseUpper].fieldMatchingCaseInsensitive)
        .decode(CaseLower(1, "a", 2L), shouldFailFast = false, path = "root"),
      Right(CaseUpper(1, "a", 2L))
    )
    // case class -> Java Beans
    assertEquals(
      ContextCodec
        .derive(ContextCodec.Config[CaseLower, BeanUpper].fieldMatchingCaseInsensitive)
        .decode(CaseLower(1, "a", 2L), shouldFailFast = false, path = "root"),
      Right(new BeanUpper().tap(_.setAAA(1)).tap(_.setBBB("a")).tap(_.setCCC(2L)))
    )
    // Java Beans -> case class
    assertEquals(
      ContextCodec
        .derive(ContextCodec.Config[CaseLower, BeanUpper].fieldMatchingCaseInsensitive)
        .decode(CaseLower(1, "a", 2L), shouldFailFast = false, path = "root"),
      Right(new BeanUpper().tap(_.setAAA(1)).tap(_.setBBB("a")).tap(_.setCCC(2L)))
    )
    // Java Beans -> Java Beans
    assertEquals(
      ContextCodec
        .derive(ContextCodec.Config[BeanLower, BeanUpper].fieldMatchingCaseInsensitive)
        .decode(new BeanLower().tap(_.setAaa(1)).tap(_.setBbb("a")).tap(_.setCcc(2L)),
                shouldFailFast = false,
                path = "root"
        ),
      Right(new BeanUpper().tap(_.setAAA(1)).tap(_.setBBB("a")).tap(_.setCCC(2L)))
    )
  }

  test("enableFallbackToDefaults, no manual override -> fields with no source should use defaults") {
    assertEquals(
      ContextCodec
        .derive(ContextCodec.Config[CaseManyIn, CaseManyOutExt].enableFallbackToDefaults)
        .decode(CaseManyIn(1, "a", 2L), shouldFailFast = false, path = "root"),
      Right(CaseManyOutExt(1, "a", 2L, "test"))
    )
  }

  test("no config, auto derive elements -> use matching subtypes") {
    // case object only in ADT
    assertEquals(
      ContextCodec.derive[ADTObjectsIn, ADTObjectsOut].decode(ADTObjectsIn.B, shouldFailFast = false, path = "root"),
      Right(ADTObjectsOut.B)
    )
    // case classes in ADT
    assertEquals(
      ContextCodec.derive[ADTClassesIn, ADTClassesOut].decode(ADTClassesIn.B(1), shouldFailFast = false, path = "root"),
      Right(ADTClassesOut.B(1))
    )
    // scala 3 enum
    assertEquals(
      ContextCodec
        .derive(ContextCodec.Config[EnumIn[Int], EnumOut[Int]].enableDiagnostics)
        .decode(EnumIn.B(1), shouldFailFast = false, path = "root"),
      Right(EnumOut.B(1))
    )
  }

  test("removeSubtype, auto summon elements -> for removed use pipe, for others use matching subtypes") {
    // case object only in ADT
    assertEquals(
      ContextCodec
        .derive(
          ContextCodec
            .Config[ADTObjectsRemovedIn, ADTObjectsRemovedOut]
            .removeSubtype[ADTObjectsRemovedIn.C.type]((_, _, _) => Right(ADTObjectsRemovedOut.A))
        )
        .decode(ADTObjectsRemovedIn.C, shouldFailFast = false, path = "root"),
      Right(ADTObjectsRemovedOut.A)
    )
    // case classes in ADT
    assertEquals(
      ContextCodec
        .derive(
          ContextCodec
            .Config[ADTClassesRemovedIn, ADTClassesRemovedOut]
            .removeSubtype[ADTClassesRemovedIn.C]((c, _, _) => Right(ADTClassesRemovedOut.A(c.c)))
        )
        .decode(ADTClassesRemovedIn.C(1), shouldFailFast = false, path = "root"),
      Right(ADTClassesRemovedOut.A(1))
    )
  }

  test("renameSubtype, auto summon elements -> for renamed summon by new name, for others use matching subtypes") {
    // case object only in ADT
    assertEquals(
      ContextCodec
        .derive(
          ContextCodec
            .Config[ADTObjectsRemovedIn, ADTObjectsRemovedOut]
            .renameSubtype[ADTObjectsRemovedIn.C.type, ADTObjectsRemovedOut.A.type]
        )
        .decode(ADTObjectsRemovedIn.C, shouldFailFast = false, path = "root"),
      Right(ADTObjectsRemovedOut.A)
    )
    // case classes in ADT
    implicit val aCodec: ContextCodec[ADTClassesRemovedIn.C, ADTClassesRemovedOut.A] = ContextCodec.derive(
      ContextCodec.Config[ADTClassesRemovedIn.C, ADTClassesRemovedOut.A].renameField(_.c, _.a)
    )
    assertEquals(
      ContextCodec
        .derive(
          ContextCodec
            .Config[ADTClassesRemovedIn, ADTClassesRemovedOut]
            .renameSubtype[ADTClassesRemovedIn.C, ADTClassesRemovedOut.A]
        )
        .decode(ADTClassesRemovedIn.C(1), shouldFailFast = false, path = "root"),
      Right(ADTClassesRemovedOut.A(1))
    )
  }

  test("plugInSubtype, auto summon elements -> for selected use pipe, for others use matching subtypes") {
    // case object only in ADT
    assertEquals(
      ContextCodec
        .derive(
          ContextCodec
            .Config[ADTObjectsRemovedIn, ADTObjectsRemovedOut]
            .plugInSubtype[ADTObjectsRemovedIn.C.type, ADTObjectsRemovedOut.A.type]((_, _, _) =>
              Right(ADTObjectsRemovedOut.A)
            )
        )
        .decode(ADTObjectsRemovedIn.C, shouldFailFast = false, path = "root"),
      Right(ADTObjectsRemovedOut.A)
    )
    // case classes in ADT
    assertEquals(
      ContextCodec
        .derive(
          ContextCodec
            .Config[ADTClassesRemovedIn, ADTClassesRemovedOut]
            .plugInSubtype[ADTClassesRemovedIn.C, ADTClassesRemovedOut.A]((c, _, _) =>
              Right(ADTClassesRemovedOut.A(c.c))
            )
        )
        .decode(ADTClassesRemovedIn.C(1), shouldFailFast = false, path = "root"),
      Right(ADTClassesRemovedOut.A(1))
    )
  }
  test("enumMatchingCaseInsensitive, auto summon elements -> match subtypes by name ignoring cases") {
    assertEquals(
      ContextCodec
        .derive(ContextCodec.Config[ADTLower, ADTUpper].enumMatchingCaseInsensitive)
        .decode(ADTLower.Ccc(1), shouldFailFast = false, path = "root"),
      Right(ADTUpper.CCC(1))
    )
  }

  test("generic types -> types should be resolved") {
    implicit val aCodec: ContextCodec[String, Int] = (string: String, _: Boolean, path: String) =>
      scala.util.Try(string.toInt).fold(_ => Left(List(s"$path cannot be converted to Int")), Right(_))
    // case class -> case class
    assertEquals(
      ContextCodec
        .derive(
          ContextCodec.Config[CaseParamIn[String], CaseParamOutExt[Int]].addField(_.x, (i, _, _) => Right(i.a))
        )
        .decode(CaseParamIn(5, "test", "10"), shouldFailFast = false, path = "root"),
      Right(CaseParamOutExt(5, "test", 10, 5))
    )
    // case class -> Java Beans
    assertEquals(
      ContextCodec
        .derive(
          ContextCodec.Config[CaseParamIn[String], BeanPolyOutExt[Int]].addField(_.getX(), (i, _, _) => Right(i.a))
        )
        .decode(CaseParamIn(5, "test", "10"), shouldFailFast = false, path = "root"),
      Right(new BeanPolyOutExt[Int]().tap(_.setA(5)).tap(_.setB("test")).tap(_.setC(10)).tap(_.setX(5)))
    )
    // Java Beans -> case class
    assertEquals(
      ContextCodec
        .derive(
          ContextCodec.Config[BeanPolyIn[String], CaseParamOutExt[Int]].addField(_.x, (i, _, _) => Right(i.a))
        )
        .decode(new BeanPolyIn[String]().tap(_.setA(5)).tap(_.setB("test")).tap(_.setC("10")),
                shouldFailFast = false,
                path = "root"
        ),
      Right(CaseParamOutExt(5, "test", 10, 5))
    )
    // Java Beans -> Java Beans
    assertEquals(
      ContextCodec
        .derive(
          ContextCodec.Config[BeanPolyIn[String], BeanPolyOutExt[Int]].addField(_.getX(), (i, _, _) => Right(i.a))
        )
        .decode(new BeanPolyIn[String]().tap(_.setA(5)).tap(_.setB("test")).tap(_.setC("10")),
                shouldFailFast = false,
                path = "root"
        ),
      Right(new BeanPolyOutExt[Int]().tap(_.setA(5)).tap(_.setB("test")).tap(_.setC(10)).tap(_.setX(5)))
    )
    // type parameter used during derivation
    def test[In, Out](out: Out)(implicit
      contextCodec:        ContextCodec[In, Out]
    ): ContextCodec[CaseParamIn[In], CaseParamOutExt[Out]] = ContextCodec.derive(
      ContextCodec.Config[CaseParamIn[In], CaseParamOutExt[Out]].addField(_.x, (_, _, _) => Right(out))
    )
    assertEquals(
      test[String, Int](5).decode(CaseParamIn(5, "test", "10"), shouldFailFast = false, path = "root"),
      Right(CaseParamOutExt(5, "test", 10, 5))
    )
  }

  test("transformation of tuples should use position for field matching") {
    // case class -> tuple
    assertEquals(
      ContextCodec
        .derive(
          ContextCodec
            .Config[(Int, String, Double), CaseParamOutExt[Double]]
            .addField(_.x, (i, _, _) => Right(i._1.toDouble))
        )
        .decode((5, "test", 10.0), shouldFailFast = false, "root"),
      Right(CaseParamOutExt(5, "test", 10.0, 5.0))
    )
    // tuple -> case class
    assertEquals(
      ContextCodec
        .derive(
          ContextCodec.Config[CaseParamIn[Double], (Int, String, Double, Int)].addField(_._4, (i, _, _) => Right(i.a))
        )
        .decode(CaseParamIn(5, "test", 10.0), shouldFailFast = false, "root"),
      Right((5, "test", 10.0, 5))
    )
  }

  test("transformation should handle backticks in names") {
    assertEquals(
      ContextCodec
        .derive[`Backtick ADT In`, `Backtick ADT Out`]
        .decode(`Backtick ADT In`.`Case Class`("test"), shouldFailFast = false, "root"),
      Right(`Backtick ADT Out`.`Case Class`("test"))
    )
    assertEquals(
      ContextCodec
        .derive[`Backtick ADT In`, `Backtick ADT Out`]
        .decode(`Backtick ADT In`.`Case Class`("test"), shouldFailFast = false, "root"),
      Right(`Backtick ADT Out`.`Case Class`("test"))
    )
  }

  test("errors should appear in Left enriched with Path information") {
    implicit val aCodec: ContextCodec[String, Int] = (string: String, _: Boolean, path: String) =>
      scala.util.Try(string.toInt).fold(_ => Left(List(s"$path cannot be converted to Int")), Right(_))
    val codec: ContextCodec[Either[String, String], Either[Int, Int]] = ContextCodec.derive
    val l = Left("10x")
    val r = Right("10x")
    // Scala 2 and Scala 3 type fullName have different approach to primitives and spaces in type arguments
    assert(
      Set[Either[List[String], Either[Int, Int]]](
        Left(List(s"($l: scala.util.Left[String,String]).value cannot be converted to Int")),
        Left(List(s"($l: scala.util.Left[scala.Predef.String, scala.Predef.String]).value cannot be converted to Int"))
      ).contains(codec.decode(l, false, l.toString))
    )
    assert(
      Set[Either[List[String], Either[Int, Int]]](
        Left(List(s"($r: scala.util.Right[String,String]).value cannot be converted to Int")),
        Left(List(s"($r: scala.util.Right[scala.Predef.String, scala.Predef.String]).value cannot be converted to Int"))
      ).contains(codec.decode(r, false, r.toString))
    )
  }
}
