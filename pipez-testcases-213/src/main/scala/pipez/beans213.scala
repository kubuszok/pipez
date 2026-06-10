package pipez

// Bean types with public vars (Scala-style getters/setters) and explicit Java Bean
// getter/setter methods. Works on JVM, Scala.js, and Scala Native.
// The macro detects beans by looking for setX(v: T) methods + default constructor.

class BeanOnesIn {
  var a: Int = 0
  def getA(): Int = a
  def setA(v: Int): Unit = a = v
  override def equals(obj: Any): Boolean = obj match { case o: BeanOnesIn => a == o.a; case _ => false }
  override def hashCode(): Int = a.hashCode()
  override def toString: String = s"BeanOnesIn($a)"
}

class BeanOnesOut {
  var a: Int = 0
  def getA(): Int = a
  def setA(v: Int): Unit = a = v
  override def equals(obj: Any): Boolean = obj match { case o: BeanOnesOut => a == o.a; case _ => false }
  override def hashCode(): Int = a.hashCode()
  override def toString: String = s"BeanOnesOut($a)"
}

class BeanOnesOutMod {
  var a: String = ""
  def getA(): String = a
  def setA(v: String): Unit = a = v
  override def equals(obj: Any): Boolean = obj match { case o: BeanOnesOutMod => a == o.a; case _ => false }
  override def hashCode(): Int = a.hashCode()
  override def toString: String = s"BeanOnesOutMod($a)"
}

class BeanOnesOutExt {
  var a: Int = 0
  var x: String = ""
  def getA(): Int = a; def setA(v: Int): Unit = a = v
  def getX(): String = x; def setX(v: String): Unit = x = v
  override def equals(obj: Any): Boolean = obj match {
    case o: BeanOnesOutExt => a == o.a && x == o.x; case _ => false
  }
  override def hashCode(): Int = (a, x).hashCode()
  override def toString: String = s"BeanOnesOutExt($a,$x)"
}

class BeanManyIn {
  var a: Int = 0; var b: String = ""; var c: Long = 0L
  def getA(): Int = a; def setA(v: Int): Unit = a = v
  def getB(): String = b; def setB(v: String): Unit = b = v
  def getC(): Long = c; def setC(v: Long): Unit = c = v
  override def equals(obj: Any): Boolean = obj match {
    case o: BeanManyIn => a == o.a && b == o.b && c == o.c; case _ => false
  }
  override def hashCode(): Int = (a, b, c).hashCode()
  override def toString: String = s"BeanManyIn($a,$b,$c)"
}

class BeanManyOut {
  var a: Int = 0; var b: String = ""; var c: Long = 0L
  def getA(): Int = a; def setA(v: Int): Unit = a = v
  def getB(): String = b; def setB(v: String): Unit = b = v
  def getC(): Long = c; def setC(v: Long): Unit = c = v
  override def equals(obj: Any): Boolean = obj match {
    case o: BeanManyOut => a == o.a && b == o.b && c == o.c; case _ => false
  }
  override def hashCode(): Int = (a, b, c).hashCode()
  override def toString: String = s"BeanManyOut($a,$b,$c)"
}

class BeanManyOutMod {
  var a: String = ""; var b: String = ""; var c: Long = 0L
  def getA(): String = a; def setA(v: String): Unit = a = v
  def getB(): String = b; def setB(v: String): Unit = b = v
  def getC(): Long = c; def setC(v: Long): Unit = c = v
  override def equals(obj: Any): Boolean = obj match {
    case o: BeanManyOutMod => a == o.a && b == o.b && c == o.c; case _ => false
  }
  override def hashCode(): Int = (a, b, c).hashCode()
  override def toString: String = s"BeanManyOutMod($a,$b,$c)"
}

class BeanManyOutExt {
  var a: Int = 0; var b: String = ""; var c: Long = 0L; var x: String = ""
  def getA(): Int = a; def setA(v: Int): Unit = a = v
  def getB(): String = b; def setB(v: String): Unit = b = v
  def getC(): Long = c; def setC(v: Long): Unit = c = v
  def getX(): String = x; def setX(v: String): Unit = x = v
  override def equals(obj: Any): Boolean = obj match {
    case o: BeanManyOutExt => a == o.a && b == o.b && c == o.c && x == o.x; case _ => false
  }
  override def hashCode(): Int = (a, b, c, x).hashCode()
  override def toString: String = s"BeanManyOutExt($a,$b,$c,$x)"
}

class BeanZeroOutExt {
  var x: String = ""
  def getX(): String = x; def setX(v: String): Unit = x = v
  override def equals(obj: Any): Boolean = obj match {
    case o: BeanZeroOutExt => x == o.x; case _ => false
  }
  override def hashCode(): Int = x.hashCode()
  override def toString: String = s"BeanZeroOutExt($x)"
}

class BeanPolyIn[A] {
  var a: Int = 0; var b: String = ""; var c: A = null.asInstanceOf[A]
  def getA(): Int = a; def setA(v: Int): Unit = a = v
  def getB(): String = b; def setB(v: String): Unit = b = v
  def getC(): A = c; def setC(v: A): Unit = c = v
  override def equals(obj: Any): Boolean = obj match {
    case o: BeanPolyIn[?] => a == o.a && b == o.b && c == o.c; case _ => false
  }
  override def hashCode(): Int = (a, b, c).hashCode()
  override def toString: String = s"BeanPolyIn($a,$b,$c)"
}

class BeanPolyOutExt[A] {
  var a: Int = 0; var b: String = ""; var c: A = null.asInstanceOf[A]; var x: A = null.asInstanceOf[A]
  def getA(): Int = a; def setA(v: Int): Unit = a = v
  def getB(): String = b; def setB(v: String): Unit = b = v
  def getC(): A = c; def setC(v: A): Unit = c = v
  def getX(): A = x; def setX(v: A): Unit = x = v
  override def equals(obj: Any): Boolean = obj match {
    case o: BeanPolyOutExt[?] => a == o.a && b == o.b && c == o.c && x == o.x; case _ => false
  }
  override def hashCode(): Int = (a, b, c, x).hashCode()
  override def toString: String = s"BeanPolyOutExt($a,$b,$c,$x)"
}

class BeanLower {
  var aaa: Int = 0; var bbb: String = ""; var ccc: Long = 0L
  def getAaa(): Int = aaa; def setAaa(v: Int): Unit = aaa = v
  def getBbb(): String = bbb; def setBbb(v: String): Unit = bbb = v
  def getCcc(): Long = ccc; def setCcc(v: Long): Unit = ccc = v
  override def equals(obj: Any): Boolean = obj match {
    case o: BeanLower => aaa == o.aaa && bbb == o.bbb && ccc == o.ccc; case _ => false
  }
  override def hashCode(): Int = (aaa, bbb, ccc).hashCode()
}

class BeanUpper {
  var AAA: Int = 0; var BBB: String = ""; var CCC: Long = 0L
  def getAAA(): Int = AAA; def setAAA(v: Int): Unit = AAA = v
  def getBBB(): String = BBB; def setBBB(v: String): Unit = BBB = v
  def getCCC(): Long = CCC; def setCCC(v: Long): Unit = CCC = v
  override def equals(obj: Any): Boolean = obj match {
    case o: BeanUpper => AAA == o.AAA && BBB == o.BBB && CCC == o.CCC; case _ => false
  }
  override def hashCode(): Int = (AAA, BBB, CCC).hashCode()
}

class Bean3ManyIn {
  var a: Int = 0; var b: String = ""; var c: Long = 0L
  def getA(): Int = a; def setA(v: Int): Unit = a = v
  def getB(): String = b; def setB(v: String): Unit = b = v
  def getC(): Long = c; def setC(v: Long): Unit = c = v
  override def equals(obj: Any): Boolean = obj match {
    case o: Bean3ManyIn => a == o.a && b == o.b && c == o.c; case _ => false
  }
  override def hashCode(): Int = (a, b, c).hashCode()
  override def toString: String = s"Bean3ManyIn($a,$b,$c)"
}

class Bean3ManyOut {
  var a: Int = 0; var b: String = ""; var c: Long = 0L
  def getA(): Int = a; def setA(v: Int): Unit = a = v
  def getB(): String = b; def setB(v: String): Unit = b = v
  def getC(): Long = c; def setC(v: Long): Unit = c = v
  override def equals(obj: Any): Boolean = obj match {
    case o: Bean3ManyOut => a == o.a && b == o.b && c == o.c; case _ => false
  }
  override def hashCode(): Int = (a, b, c).hashCode()
  override def toString: String = s"Bean3ManyOut($a,$b,$c)"
}
