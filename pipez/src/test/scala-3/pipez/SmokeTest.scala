package pipez

class SmokeTest extends munit.FunSuite {

  test("zero-field case class derivation") {
    assertEquals(
      NoContextCodec.derive[ZeroIn, ZeroOut].decode(ZeroIn()),
      Right(ZeroOut())
    )
  }
}
