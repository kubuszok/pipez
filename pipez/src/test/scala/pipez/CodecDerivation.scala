package pipez

// TODO: case class -> case class
// TODO: case class -> java bean
// TODO: java bean -> case class
// TODO: java bean -> java bean
// TODO: test backticked names like `a b`

class CodecDerivation extends munit.FunSuite {

  case class FooIn(a: Int)
  case class FooOut(a: Int)

  test("simple derivation") {
    assertEquals(
      PipeDerivation.derive[NoContextCodec, FooIn, FooOut].decode(FooIn(1)),
      Right(FooOut(1))
    )
  }
}
