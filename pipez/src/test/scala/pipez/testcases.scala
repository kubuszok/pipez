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

/*

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

// ADT

sealed trait ADTObjectsIn extends Product with Serializable
object ADTObjectsIn {

  case object A extends ADTObjectsIn
  case object B extends ADTObjectsIn
}
sealed trait ADTObjectsOut extends Product with Serializable
object ADTObjectsOut {

  final case object A extends ADTObjectsOut
  final case object B extends ADTObjectsOut
  final case object C extends ADTObjectsOut
}

sealed trait ADTClassesIn extends Product with Serializable
object ADTClassesIn {

  final case class A(a: Int) extends ADTClassesIn
  final case class B(b: Int) extends ADTClassesIn
}
sealed trait ADTClassesOut extends Product with Serializable
object ADTClassesOut {

  final case class A(a: Int) extends ADTClassesOut
  final case class B(b: Int) extends ADTClassesOut
  final case class C(c: Int) extends ADTClassesOut
}

sealed trait ADTObjectsRemovedIn extends Product with Serializable

object ADTObjectsRemovedIn {

  final case object A extends ADTObjectsRemovedIn
  final case object B extends ADTObjectsRemovedIn
  final case object C extends ADTObjectsRemovedIn
}
sealed trait ADTObjectsRemovedOut extends Product with Serializable
object ADTObjectsRemovedOut {

  case object A extends ADTObjectsRemovedOut
  case object B extends ADTObjectsRemovedOut
}

sealed trait ADTClassesRemovedIn extends Product with Serializable
object ADTClassesRemovedIn {

  final case class A(a: Int) extends ADTClassesRemovedIn
  final case class B(b: Int) extends ADTClassesRemovedIn
  final case class C(c: Int) extends ADTClassesRemovedIn
}
sealed trait ADTClassesRemovedOut extends Product with Serializable
object ADTClassesRemovedOut {

  final case class A(a: Int) extends ADTClassesRemovedOut
  final case class B(b: Int) extends ADTClassesRemovedOut
}

sealed trait ADTLower extends Product with Serializable
object ADTLower {

  final case class Aaa(a: Int) extends ADTLower
  final case class Bbb(b: Int) extends ADTLower
  final case class Ccc(c: Int) extends ADTLower
}
sealed trait ADTUpper extends Product with Serializable
object ADTUpper {

  final case class AAA(a: Int) extends ADTUpper
  final case class BBB(b: Int) extends ADTUpper
  final case class CCC(c: Int) extends ADTUpper
}

 */
