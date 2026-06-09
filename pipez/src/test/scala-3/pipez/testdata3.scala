package pipez

// Scala 3 enums

enum EnumIn[+T] {
  case A extends EnumIn[Nothing]
  case B(b: T) extends EnumIn[T]
  case C(s: String) extends EnumIn[String]
}

enum EnumOut[+T] {
  case A extends EnumOut[Nothing]
  case B(b: T) extends EnumOut[T]
  case C(s: String) extends EnumOut[String]
}
