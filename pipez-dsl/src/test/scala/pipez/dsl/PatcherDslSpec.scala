package pipez.dsl

class PatcherDslSpec extends munit.FunSuite {

  test("patcher should update fields present in patch and leave other intact") {
    import pipez.{ CaseManyIn, CaseOnesOut }
    assertEquals(
      CaseManyIn(1, "2", 3L).patchWith(CaseOnesOut(4)),
      CaseManyIn(4, "2", 3L)
    )
  }

  test("patcher should update fields and handle provided configuration") {
    import pipez.{ CaseManyIn, CaseOnesOut }
    assertEquals(
      CaseManyIn(1, "2", 3L).patchWith(CaseOnesOut(4),
                                       PatchApplier.Config[CaseOnesOut, CaseManyIn].addField(_.c, _ => 5L)
      ),
      CaseManyIn(4, "2", 5L)
    )
  }
}
