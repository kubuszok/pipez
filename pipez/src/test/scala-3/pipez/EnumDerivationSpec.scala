package pipez

class EnumDerivationSpec extends munit.FunSuite {

  test("no-context codec: scala 3 enums and cross-compilation with sealed hierarchies") {
    // scala 2 gadt -> scala 3 enum
    assertEquals(
      NoContextCodec.derive[GadtIn[Int], EnumOut[Int]].decode(GadtIn.A),
      Right(EnumOut.A)
    )
    assertEquals(
      NoContextCodec.derive[GadtIn[Int], EnumOut[Int]].decode(GadtIn.B(1)),
      Right(EnumOut.B(1))
    )
    // scala 3 enum -> scala 2 gadt
    assertEquals(
      NoContextCodec.derive[EnumIn[Int], GadtOut[Int]].decode(EnumIn.A),
      Right(GadtOut.A)
    )
    assertEquals(
      NoContextCodec.derive[EnumIn[Int], GadtOut[Int]].decode(EnumIn.B(1)),
      Right(GadtOut.B(1))
    )
    // scala 3 enum -> scala 3 enum
    assertEquals(
      NoContextCodec.derive[EnumIn[Int], EnumOut[Int]].decode(EnumIn.A),
      Right(EnumOut.A)
    )
    assertEquals(
      NoContextCodec.derive[EnumIn[Int], EnumOut[Int]].decode(EnumIn.B(1)),
      Right(EnumOut.B(1))
    )
  }
}

class ContextCodecEnumDerivationSpec extends munit.FunSuite {

  test("context codec: scala 3 enum derivation") {
    assertEquals(
      ContextCodec.derive[EnumIn[Int], EnumOut[Int]].decode(EnumIn.B(1), shouldFailFast = false, path = "root"),
      Right(EnumOut.B(1))
    )
  }

  test("context codec: sealed hierarchies and enum cross-compilation") {
    // scala 2 gadt -> scala 3 enum
    assertEquals(
      ContextCodec.derive[GadtIn[Int], EnumOut[Int]].decode(GadtIn.A, shouldFailFast = false, "root"),
      Right(EnumOut.A)
    )
    assertEquals(
      ContextCodec.derive[GadtIn[Int], EnumOut[Int]].decode(GadtIn.B(1), shouldFailFast = false, "root"),
      Right(EnumOut.B(1))
    )
    // scala 3 enum -> scala 2 gadt
    assertEquals(
      ContextCodec.derive[EnumIn[Int], GadtOut[Int]].decode(EnumIn.A, shouldFailFast = false, "root"),
      Right(GadtOut.A)
    )
    assertEquals(
      ContextCodec.derive[EnumIn[Int], GadtOut[Int]].decode(EnumIn.B(1), shouldFailFast = false, "root"),
      Right(GadtOut.B(1))
    )
    // scala 3 enum -> scala 3 enum
    assertEquals(
      ContextCodec.derive[EnumIn[Int], EnumOut[Int]].decode(EnumIn.A, shouldFailFast = false, "root"),
      Right(EnumOut.A)
    )
    assertEquals(
      ContextCodec.derive[EnumIn[Int], EnumOut[Int]].decode(EnumIn.B(1), shouldFailFast = false, "root"),
      Right(EnumOut.B(1))
    )
  }
}
