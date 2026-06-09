package pipez

// Zero-field types

case class ZeroIn()
case class ZeroOut()

// Single-field case class types

case class CaseOnesIn(a: Int)
case class CaseOnesOut(a: Int)
case class CaseOnesOutMod(a: String)
case class CaseOnesOutExt(a: Int, x: String)

// Multi-field case class types

case class CaseManyIn(a: Int, b: String, c: Long)
case class CaseManyOut(a: Int, b: String, c: Long)
case class CaseManyOutMod(a: String, b: String, c: Long)
case class CaseManyOutExt(a: Int, b: String, c: Long, x: String = "test")

// Extended zero-field types

case class CaseZeroOutExt(x: String)

// Generic/polymorphic case class types

case class CaseParamIn[A](a: Int, b: String, c: A)
case class CaseParamOutExt[A](a: Int, b: String, c: A, x: A)

// Case-insensitive matching types

case class CaseLower(aaa: Int, bbb: String, ccc: Long)
case class CaseUpper(AAA: Int, BBB: String, CCC: Long)

// Sealed trait ADTs — case objects

sealed trait ADTObjectsIn
object ADTObjectsIn {
  case object A extends ADTObjectsIn
  case object B extends ADTObjectsIn
}

sealed trait ADTObjectsOut
object ADTObjectsOut {
  case object A extends ADTObjectsOut
  case object B extends ADTObjectsOut
  case object C extends ADTObjectsOut
}

sealed trait ADTObjectsRemovedIn
object ADTObjectsRemovedIn {
  case object A extends ADTObjectsRemovedIn
  case object B extends ADTObjectsRemovedIn
  case object C extends ADTObjectsRemovedIn
}

sealed trait ADTObjectsRemovedOut
object ADTObjectsRemovedOut {
  case object A extends ADTObjectsRemovedOut
  case object B extends ADTObjectsRemovedOut
}

// Sealed trait ADTs — case classes

sealed trait ADTClassesIn
object ADTClassesIn {
  case class A(a: Int) extends ADTClassesIn
  case class B(b: Int) extends ADTClassesIn
}

sealed trait ADTClassesOut
object ADTClassesOut {
  case class A(a: Int) extends ADTClassesOut
  case class B(b: Int) extends ADTClassesOut
  case class C(c: Int) extends ADTClassesOut
}

sealed trait ADTClassesRemovedIn
object ADTClassesRemovedIn {
  case class A(a: Int) extends ADTClassesRemovedIn
  case class B(b: Int) extends ADTClassesRemovedIn
  case class C(c: Int) extends ADTClassesRemovedIn
}

sealed trait ADTClassesRemovedOut
object ADTClassesRemovedOut {
  case class A(a: Int) extends ADTClassesRemovedOut
  case class B(b: Int) extends ADTClassesRemovedOut
}

// Case-insensitive ADTs

sealed trait ADTLower
object ADTLower {
  case class Aaa(a: Int) extends ADTLower
  case class Bbb(b: Int) extends ADTLower
  case class Ccc(c: Int) extends ADTLower
}

sealed trait ADTUpper
object ADTUpper {
  case class AAA(a: Int) extends ADTUpper
  case class BBB(b: Int) extends ADTUpper
  case class CCC(c: Int) extends ADTUpper
}

// Backtick ADTs

sealed trait `Backtick ADT In`
object `Backtick ADT In` {
  case class `Case Class`(`a field`: String) extends `Backtick ADT In`
  case object `Case Object` extends `Backtick ADT In`
}

sealed trait `Backtick ADT Out`
object `Backtick ADT Out` {
  case class `Case Class`(`a field`: String) extends `Backtick ADT Out`
  case object `Case Object` extends `Backtick ADT Out`
}

// GADTs

sealed trait GadtIn[+T]
object GadtIn {
  case object A extends GadtIn[Nothing]
  case class B[+T](b: T) extends GadtIn[T]
  case class C(s: String) extends GadtIn[String]
}

sealed trait GadtOut[+T]
object GadtOut {
  case object A extends GadtOut[Nothing]
  case class B[+T](b: T) extends GadtOut[T]
  case class C(s: String) extends GadtOut[String]
}

// AnyVal types

object AnyVals {
  class ClassIn(val str: String) extends AnyVal
  class ClassOut(val str: String) extends AnyVal
  case class CaseClassIn(str: String) extends AnyVal
  case class CaseClassOut(str: String) extends AnyVal
}
