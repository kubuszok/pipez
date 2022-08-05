package pipez

import scala.beans.BeanProperty

// no getters, no setters, we cannot virtually distinct here between case classes and Java Beans

final case class NullaryIn()
final case class NullaryOut()

// case classes (we don't rely on companion, as inputs, we need their getters, as outputs we need their constructors)

final case class CaseClassNullaryOutExtended(extra: String)

final case class CaseClassUnaryIn(a: Int)
final case class CaseClassUnaryOut(a: Int)
final case class CaseClassUnaryOutModified(a: String)
final case class CaseClassUnaryOutExtended(a: Int, extra: String)
final case class CaseClassUnaryOutDiffCase(A: Int)

final case class CaseClassMultipleIn(a: Int, b: String, c: Long)
final case class CaseClassMultipleOut(a: Int, b: String, c: Long)
final case class CaseClassMultipleOutModified(a: String, b: String, c: Long)
final case class CaseClassMultipleOutExtended(a: Int, b: String, c: Long, extra: String)
final case class CaseClassMultipleOutDiffCase(A: Int, B: String, C: Long)

// Java Beans (as inputs, we rely on their getters, as output, we rely on their default constructor and setters)

final case class JavaBeansNullaryIn()
final case class JavaBeansNullaryOut()
final case class JavaBeansNullaryOutExtended(@BeanProperty var extra: String) {
  def this() = this(null.asInstanceOf[String])
}

final case class JavaBeansUnaryIn(@BeanProperty var a: Int) {
  def this() = this(null.asInstanceOf[Int])
}
final case class JavaBeansUnaryOut(@BeanProperty var a: Int) {
  def this() = this(null.asInstanceOf[Int])
}
final case class JavaBeansUnaryOutModified(@BeanProperty var a: String) {
  def this() = this(null.asInstanceOf[String])
}
final case class JavaBeansUnaryOutExtended(@BeanProperty var a: Int, @BeanProperty var extra: String) {
  def this() = this(null.asInstanceOf[Int], null.asInstanceOf[String])
}
final case class JavaBeansUnaryOutDiffCase(@BeanProperty var A: Int) {
  def this() = this(null.asInstanceOf[Int])
}

final case class JavaBeansMultipleIn(@BeanProperty var a: Int, @BeanProperty var b: String, @BeanProperty var c: Long) {
  def this() = this(null.asInstanceOf[Int], null.asInstanceOf[String], null.asInstanceOf[Long])
}
final case class JavaBeansMultipleOut(
  @BeanProperty var a: Int,
  @BeanProperty var b: String,
  @BeanProperty var c: Long
) {
  def this() = this(null.asInstanceOf[Int], null.asInstanceOf[String], null.asInstanceOf[Long])
}
final case class JavaBeansMultipleOutModified(
  @BeanProperty var a: String,
  @BeanProperty var b: String,
  @BeanProperty var c: Long
) {
  def this() = this(null.asInstanceOf[String], null.asInstanceOf[String], null.asInstanceOf[Long])
}
final case class JavaBeansMultipleOutExtended(
  @BeanProperty var a:     Int,
  @BeanProperty var b:     String,
  @BeanProperty var c:     Long,
  @BeanProperty var extra: String
) {
  def this() =
    this(null.asInstanceOf[Int], null.asInstanceOf[String], null.asInstanceOf[Long], null.asInstanceOf[String])
}
final case class JavaBeansMultipleOutDiffCase(
  @BeanProperty var A: Int,
  @BeanProperty var B: String,
  @BeanProperty var C: Long
) {
  def this() = this(null.asInstanceOf[Int], null.asInstanceOf[String], null.asInstanceOf[Long])
}
