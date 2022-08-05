package pipez

// TODO: case class -> java bean
// TODO: java bean -> case class
// TODO: java bean -> java bean
// TODO: test backticked names like `a b`

class NoContextCodecDerivationSpec extends munit.FunSuite {

  test("case class -> case class derivation: no config, no conversion") {
    assertEquals(
      PipeDerivation.derive[NoContextCodec, NullaryIn, NullaryOut].decode(NullaryIn()),
      Right(NullaryOut())
    )
    assertEquals(
      PipeDerivation.derive[NoContextCodec, CaseClassUnaryIn, CaseClassUnaryOut].decode(CaseClassUnaryIn(1)),
      Right(CaseClassUnaryOut(1))
    )
    assertEquals(
      PipeDerivation
        .derive[NoContextCodec, CaseClassMultipleIn, CaseClassMultipleOut]
        .decode(CaseClassMultipleIn(1, "a", 2L)),
      Right(CaseClassMultipleOut(1, "a", 2L))
    )
  }

  test("case class -> case class derivation: param conversion") {
    implicit val aCodec: NoContextCodec[Int, String] = int => Right(int.toString)
    assertEquals(
      PipeDerivation.derive[NoContextCodec, CaseClassUnaryIn, CaseClassUnaryOutModified].decode(CaseClassUnaryIn(1)),
      Right(CaseClassUnaryOutModified("1"))
    )
    assertEquals(
      PipeDerivation
        .derive[NoContextCodec, CaseClassMultipleIn, CaseClassMultipleOutModified]
        .decode(CaseClassMultipleIn(1, "a", 2L)),
      Right(CaseClassMultipleOutModified("1", "a", 2L))
    )
  }

  test("case class -> case class derivation: addField config (create field value from whole object)") {
    assertEquals(
      PipeDerivation
        .derive(
          PipeDerivationConfig[NoContextCodec, NullaryIn, CaseClassNullaryOutExtended]
            .addField(_.extra, (in: NullaryIn) => Right(in.toString))
        )
        .decode(NullaryIn()),
      Right(CaseClassNullaryOutExtended("NullaryIn()"))
    )
    assertEquals(
      PipeDerivation
        .derive(
          PipeDerivationConfig[NoContextCodec, CaseClassUnaryIn, CaseClassUnaryOutExtended]
            .addField(_.extra, (in: CaseClassUnaryIn) => Right(in.toString))
        )
        .decode(CaseClassUnaryIn(1)),
      Right(CaseClassUnaryOutExtended(1, "CaseClassUnaryIn(1)"))
    )
    assertEquals(
      PipeDerivation
        .derive(
          PipeDerivationConfig[NoContextCodec, CaseClassMultipleIn, CaseClassMultipleOutExtended]
            .addField(_.extra, (in: CaseClassMultipleIn) => Right(in.toString))
        )
        .decode(CaseClassMultipleIn(1, "a", 2L)),
      Right(CaseClassMultipleOutExtended(1, "a", 2L, "CaseClassMultipleIn(1,a,2)"))
    )
  }

  test("case class -> case class derivation: plugIn config (create field value from some source field)") {
    assertEquals(
      PipeDerivation
        .derive(
          PipeDerivationConfig[NoContextCodec, CaseClassUnaryIn, CaseClassUnaryOutExtended]
            .plugIn(_.a, _.extra, (a: Int) => Right(a.toString))
        )
        .decode(CaseClassUnaryIn(1)),
      Right(CaseClassUnaryOutExtended(1, "1"))
    )
    assertEquals(
      PipeDerivation
        .derive(
          PipeDerivationConfig[NoContextCodec, CaseClassMultipleIn, CaseClassMultipleOutExtended]
            .plugIn(_.a, _.extra, (a: Int) => Right(a.toString))
        )
        .decode(CaseClassMultipleIn(1, "a", 2L)),
      Right(CaseClassMultipleOutExtended(1, "a", 2L, "1"))
    )
  }

  test(
    "case class -> case class derivation: renameField config + conversion (should create value, converting some source field)"
  ) {
    implicit val aCodec: NoContextCodec[Int, String] = int => Right(int.toString)
    assertEquals(
      PipeDerivation
        .derive(
          PipeDerivationConfig[NoContextCodec, CaseClassUnaryIn, CaseClassUnaryOutExtended].renameField(_.a, _.extra)
        )
        .decode(CaseClassUnaryIn(1)),
      Right(CaseClassUnaryOutExtended(1, "1"))
    )
    assertEquals(
      PipeDerivation
        .derive(
          PipeDerivationConfig[NoContextCodec, CaseClassMultipleIn, CaseClassMultipleOutExtended].renameField(_.a,
                                                                                                              _.extra
          )
        )
        .decode(CaseClassMultipleIn(1, "a", 2L)),
      Right(CaseClassMultipleOutExtended(1, "a", 2L, "1"))
    )
  }

  test(
    "case class -> case class derivation: fieldMatchingCaseInsensitive (should match fields with similar names but with different capitalisation)"
  ) {
    assertEquals(
      PipeDerivation
        .derive(
          PipeDerivationConfig[NoContextCodec, CaseClassUnaryIn, CaseClassUnaryOutDiffCase].fieldMatchingCaseInsensitive
        )
        .decode(CaseClassUnaryIn(1)),
      Right(CaseClassUnaryOutDiffCase(1))
    )
    assertEquals(
      PipeDerivation
        .derive(
          PipeDerivationConfig[NoContextCodec,
                               CaseClassMultipleIn,
                               CaseClassMultipleOutDiffCase
          ].fieldMatchingCaseInsensitive
        )
        .decode(CaseClassMultipleIn(1, "a", 2L)),
      Right(CaseClassMultipleOutDiffCase(1, "a", 2L))
    )
  }

  // TODO: java bean -> case class, all above
}
