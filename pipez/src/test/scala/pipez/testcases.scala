package pipez

// no getters, no setters, we cannot virtually distinct here between case classes and Java Beans

final case class ZeroIn()
final case class ZeroOut()

// case classes (we don't rely on companion, as inputs, we need their getters, as outputs we need their constructors)

final case class CaseZeroOutExt(x: String)

final case class CaseOnesIn(a: Int)
final case class CaseOnesOut(a: Int)
final case class CaseOnesOutMod(a: String)
final case class CaseOnesOutExt(a: Int, x: String)

final case class CaseManyIn(a: Int, b: String, c: Long)
final case class CaseManyOut(a: Int, b: String, c: Long)
final case class CaseManyOutMod(a: String, b: String, c: Long)
final case class CaseManyOutExt(a: Int, b: String, c: Long, x: String)

final case class CaseLower(aaa: Int, bbb: String, ccc: Long)
final case class CaseUpper(AAA: Int, BBB: String, CCC: Long)

// Java Beans (as inputs, we rely on their getters, as output, we rely on their default constructor and setters)

import scala.beans.BeanProperty

final case class BeanZeroOutExt private (
  @BeanProperty var x: String
) { def this() = this("") }

final case class BeanOnesIn private (
  @BeanProperty var a: Int
) { def this() = this(0) }
final case class BeanOnesOut private (
  @BeanProperty var a: Int
) { def this() = this(0) }
final case class BeanOnesOutMod private (
  @BeanProperty var a: String
) { def this() = this("") }
final case class BeanOnesOutExt private (
  @BeanProperty var a: Int,
  @BeanProperty var x: String
) { def this() = this(0, "") }
final case class BeanOnesOutUpp private (
  @BeanProperty var A: Int
) { def this() = this(0) }

final case class BeanManyIn private (
  @BeanProperty var a: Int,
  @BeanProperty var b: String,
  @BeanProperty var c: Long
) { def this() = this(0, "", 0L) }
final case class BeanManyOut private (
  @BeanProperty var a: Int,
  @BeanProperty var b: String,
  @BeanProperty var c: Long
) { def this() = this(0, "", 0L) }
final case class BeanManyOutMod private (
  @BeanProperty var a: String,
  @BeanProperty var b: String,
  @BeanProperty var c: Long
) { def this() = this("", "", 0L) }
final case class BeanManyOutExt private (
  @BeanProperty var a: Int,
  @BeanProperty var b: String,
  @BeanProperty var c: Long,
  @BeanProperty var x: String
) { def this() = this(0, "", 0L, "") }

final case class BeanLower private (
  @BeanProperty var aaa: Int,
  @BeanProperty var bbb: String,
  @BeanProperty var ccc: Long
) { def this() = this(0, "", 0L) }
final case class BeanUpper private (
  @BeanProperty var AAA: Int,
  @BeanProperty var BBB: String,
  @BeanProperty var CCC: Long
) { def this() = this(0, "", 0L) }
