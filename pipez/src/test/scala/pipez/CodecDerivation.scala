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
    // given
    val input = FooIn(1)

    // when
    val derived = PipeDerivation.derive[NoContextCodec, FooIn, FooOut]
    val result = derived.decode(input)

    // then
    val expected = Right(FooOut(1))
    assertEquals(result, expected)
  }
}
