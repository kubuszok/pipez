package pipez

class BugReportSpec extends munit.FunSuite {

  test("Bug report https://github.com/MateuszKubuszok/pipez/issues/27") {
    // Bug was related to weird behavior in Scala 3 macros where symbols for case class fields are doubled:
    // - there is one "val fieldName"
    // - and there is one "method fieldName " (with space at the end!)
    // This behavior was breaking matching by position (where input/output is a Tuple) but the workaround used
    // unreliable solution which sometimes filtered out _all_ case class fields.
    case class OvpnConfigurationFile(
      id:        String,
      createdAt: Long,
      name:      String,
      notBefore: String,
      notAfter:  String,
      contents:  String
    )

    case class OvpnConfigurationFileResponse(
      id:        String,
      createdAt: Long,
      name:      String,
      notBefore: String,
      notAfter:  String
    )
    assertEquals(
      ContextCodec
        .derive[OvpnConfigurationFile, OvpnConfigurationFileResponse]
        .decode(
          OvpnConfigurationFile(
            id = "foo",
            createdAt = 0,
            name = "bar",
            notBefore = "x",
            notAfter = "y",
            contents = "z"
          ),
          false,
          ""
        ),
      Right(
        OvpnConfigurationFileResponse(
          id = "foo",
          createdAt = 0,
          name = "bar",
          notBefore = "x",
          notAfter = "y"
        )
      )
    )
  }
}
