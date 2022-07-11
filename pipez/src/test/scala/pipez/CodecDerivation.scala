package pipez

object CodecDerivation {

  trait Codec[In, Out] {

    def decode(in: In): Either[List[String], Out]
  }
  object Codec {
    implicit val pipeDerivation = new PipeDerivation[Codec] {
      final type Context     = Unit
      final type Result[Out] = Either[List[String], Out]

      override def lift[In, Out](f: (In, Unit) => Either[List[String], Out]): Codec[In, Out] = (in: In) => f(in, ())

      override def unlift[In, Out](pipe: Codec[In, Out])(in: In, ctx: Unit): Either[List[String], Out] = pipe.decode(in)

      override def updateContext(context: Unit, path: Path): Unit = context

      override def pureResult[A](a: A): Either[List[String], A] = Right(a)

      override def mergeResults[A, B, C](ra: Either[List[String], A], rb: => Either[List[String], B])(
        f:                                   (A, B) => C
      ): Either[List[String], C] = (ra, rb) match {
        case (Right(a), Right(b)) => Right(f(a, b))
        case (Left(ea), Left(eb)) => Left(ea ++ eb)
        case (Left(e), _)         => Left(e)
        case (_, Left(e))         => Left(e)
      }
    }
  }

  case class FooIn(a: Int)
  case class FooOut(a: Int)
}
class CodecDerivation extends munit.FunSuite {

  import CodecDerivation._

  test("simple derivation") {
    assertEquals(
      PipeDerivation.derive[Codec, FooIn, FooOut].decode(FooIn(1)),
      Right(FooOut(1))
    )
  }
}
