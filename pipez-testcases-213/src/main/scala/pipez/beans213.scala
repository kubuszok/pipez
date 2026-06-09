package pipez

import scala.beans.BeanProperty

// These types must be compiled with Scala 2.13 so that @BeanProperty generates
// proper getA()/setA() methods visible from both Scala 2 and Scala 3 (via TASTy reader).

class BeanOnesIn {
  @BeanProperty var a: Int = 0
  override def equals(obj: Any): Boolean = obj match {
    case o: BeanOnesIn => a == o.a
    case _             => false
  }
  override def hashCode(): Int = a.hashCode()
  override def toString: String = s"BeanOnesIn($a)"
}

class BeanOnesOut {
  @BeanProperty var a: Int = 0
  override def equals(obj: Any): Boolean = obj match {
    case o: BeanOnesOut => a == o.a
    case _              => false
  }
  override def hashCode(): Int = a.hashCode()
  override def toString: String = s"BeanOnesOut($a)"
}

class BeanOnesOutMod {
  @BeanProperty var a: String = ""
  override def equals(obj: Any): Boolean = obj match {
    case o: BeanOnesOutMod => a == o.a
    case _                 => false
  }
  override def hashCode(): Int = a.hashCode()
  override def toString: String = s"BeanOnesOutMod($a)"
}

class BeanOnesOutExt {
  @BeanProperty var a: Int    = 0
  @BeanProperty var x: String = ""
  override def equals(obj: Any): Boolean = obj match {
    case o: BeanOnesOutExt => a == o.a && x == o.x
    case _                 => false
  }
  override def hashCode(): Int = (a, x).hashCode()
  override def toString: String = s"BeanOnesOutExt($a,$x)"
}

class BeanManyIn {
  @BeanProperty var a: Int    = 0
  @BeanProperty var b: String = ""
  @BeanProperty var c: Long   = 0L
  override def equals(obj: Any): Boolean = obj match {
    case o: BeanManyIn => a == o.a && b == o.b && c == o.c
    case _             => false
  }
  override def hashCode(): Int = (a, b, c).hashCode()
  override def toString: String = s"BeanManyIn($a,$b,$c)"
}

class BeanManyOut {
  @BeanProperty var a: Int    = 0
  @BeanProperty var b: String = ""
  @BeanProperty var c: Long   = 0L
  override def equals(obj: Any): Boolean = obj match {
    case o: BeanManyOut => a == o.a && b == o.b && c == o.c
    case _              => false
  }
  override def hashCode(): Int = (a, b, c).hashCode()
  override def toString: String = s"BeanManyOut($a,$b,$c)"
}

class BeanManyOutMod {
  @BeanProperty var a: String = ""
  @BeanProperty var b: String = ""
  @BeanProperty var c: Long   = 0L
  override def equals(obj: Any): Boolean = obj match {
    case o: BeanManyOutMod => a == o.a && b == o.b && c == o.c
    case _                 => false
  }
  override def hashCode(): Int = (a, b, c).hashCode()
  override def toString: String = s"BeanManyOutMod($a,$b,$c)"
}

class BeanManyOutExt {
  @BeanProperty var a: Int    = 0
  @BeanProperty var b: String = ""
  @BeanProperty var c: Long   = 0L
  @BeanProperty var x: String = ""
  override def equals(obj: Any): Boolean = obj match {
    case o: BeanManyOutExt => a == o.a && b == o.b && c == o.c && x == o.x
    case _                 => false
  }
  override def hashCode(): Int = (a, b, c, x).hashCode()
  override def toString: String = s"BeanManyOutExt($a,$b,$c,$x)"
}

class BeanZeroOutExt {
  @BeanProperty var x: String = ""
  override def equals(obj: Any): Boolean = obj match {
    case o: BeanZeroOutExt => x == o.x
    case _                 => false
  }
  override def hashCode(): Int = x.hashCode()
  override def toString: String = s"BeanZeroOutExt($x)"
}

class BeanPolyIn[A] {
  @BeanProperty var a: Int    = 0
  @BeanProperty var b: String = ""
  @BeanProperty var c: A      = null.asInstanceOf[A]
  override def equals(obj: Any): Boolean = obj match {
    case o: BeanPolyIn[?] => a == o.a && b == o.b && c == o.c
    case _                => false
  }
  override def hashCode(): Int = (a, b, c).hashCode()
  override def toString: String = s"BeanPolyIn($a,$b,$c)"
}

class BeanPolyOutExt[A] {
  @BeanProperty var a: Int    = 0
  @BeanProperty var b: String = ""
  @BeanProperty var c: A      = null.asInstanceOf[A]
  @BeanProperty var x: A      = null.asInstanceOf[A]
  override def equals(obj: Any): Boolean = obj match {
    case o: BeanPolyOutExt[?] => a == o.a && b == o.b && c == o.c && x == o.x
    case _                    => false
  }
  override def hashCode(): Int = (a, b, c, x).hashCode()
  override def toString: String = s"BeanPolyOutExt($a,$b,$c,$x)"
}

class BeanLower {
  @BeanProperty var aaa: Int    = 0
  @BeanProperty var bbb: String = ""
  @BeanProperty var ccc: Long   = 0L
  override def equals(obj: Any): Boolean = obj match {
    case o: BeanLower => aaa == o.aaa && bbb == o.bbb && ccc == o.ccc
    case _            => false
  }
  override def hashCode(): Int = (aaa, bbb, ccc).hashCode()
}

class BeanUpper {
  @BeanProperty var AAA: Int    = 0
  @BeanProperty var BBB: String = ""
  @BeanProperty var CCC: Long   = 0L
  override def equals(obj: Any): Boolean = obj match {
    case o: BeanUpper => AAA == o.AAA && BBB == o.BBB && CCC == o.CCC
    case _            => false
  }
  override def hashCode(): Int = (AAA, BBB, CCC).hashCode()
}

class Bean3ManyIn {
  @BeanProperty var a: Int    = 0
  @BeanProperty var b: String = ""
  @BeanProperty var c: Long   = 0L
  override def equals(obj: Any): Boolean = obj match {
    case o: Bean3ManyIn => a == o.a && b == o.b && c == o.c
    case _              => false
  }
  override def hashCode(): Int = (a, b, c).hashCode()
  override def toString: String = s"Bean3ManyIn($a,$b,$c)"
}

class Bean3ManyOut {
  @BeanProperty var a: Int    = 0
  @BeanProperty var b: String = ""
  @BeanProperty var c: Long   = 0L
  override def equals(obj: Any): Boolean = obj match {
    case o: Bean3ManyOut => a == o.a && b == o.b && c == o.c
    case _               => false
  }
  override def hashCode(): Int = (a, b, c).hashCode()
  override def toString: String = s"Bean3ManyOut($a,$b,$c)"
}
