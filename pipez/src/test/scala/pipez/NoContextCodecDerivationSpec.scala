package pipez

// TODO: case class -> case class
// TODO: case class -> java bean
// TODO: java bean -> case class
// TODO: java bean -> java bean
// TODO: test backticked names like `a b`

class NoContextCodecDerivationSpec extends munit.FunSuite {

  case class CaseClassNullaryIn()
  case class CaseClassNullaryOut()

  case class CaseClassUnaryIn(a: Int)
  case class CaseClassUnaryOut(a: Int)
  case class CaseClassUnaryOutModified(a: String)

  test("case class -> case class derivation: nullary") {
    assertEquals(
      PipeDerivation.derive[NoContextCodec, CaseClassNullaryIn, CaseClassNullaryOut].decode(CaseClassNullaryIn()),
      Right(CaseClassNullaryOut())
    )
  }

  test("case class -> case class derivation: unary, no configs, no conversions") {
    assertEquals(
      PipeDerivation.derive[NoContextCodec, CaseClassUnaryIn, CaseClassUnaryOut].decode(CaseClassUnaryIn(1)),
      Right(CaseClassUnaryOut(1))
    )
  }

  test("case class -> case class derivation: unary, no configs, param conversions") {
    implicit val aCodec: NoContextCodec[Int, String] = int => Right(int.toString)
    assertEquals(
      PipeDerivation.derive[NoContextCodec, CaseClassUnaryIn, CaseClassUnaryOutModified].decode(CaseClassUnaryIn(1)),
      Right(CaseClassUnaryOutModified("1"))
    )
  }

  // TODO: change fields
  test("config test") {
    assertEquals(
      PipeDerivation
        .derive(
          PipeDerivationConfig[NoContextCodec, CaseClassUnaryIn, CaseClassUnaryOutModified]
            //.enableDiagnostics
            .addField(_.a, i => Right(i.a.toString))
            .renameField(_.a, _.a)
            //.removeSubtype()
            //.removeSubtype()
            .plugIn(_.a, _.a, (a: Int) => Right(a.toString))
            .fieldMatchingCaseInsensitive
        )
        .decode(CaseClassUnaryIn(1)),
      Right(CaseClassUnaryOutModified("1"))
    )
  }

  // TODO: case class -> case class derivation: unary output, no configs, no conversions
  // TODO: case class -> case class derivation: unary output, no configs, conversion
  // TODO: case class -> case class derivation: multiple, no configs, field conversion
  // TODO: case class -> case class derivation: multiple, no configs, field conversion

  // TODO: java bean -> case class, all above
}
