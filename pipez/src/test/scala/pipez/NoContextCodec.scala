package pipez

trait NoContextCodec[In, Out] {

  def decode(in: In): Either[List[String], Out]
}

object NoContextCodec extends PipeSemiautoSupport[NoContextCodec] with PipeSemiautoConfiguredSupport[NoContextCodec] {
  
  implicit val pipeDerivation: PipeDerivation.NoContext[NoContextCodec] = new PipeDerivation.NoContext[NoContextCodec] {

    final type Result[Out] = Either[List[String], Out]

    override def simpleLift[In, Out](f: In => Either[List[String], Out]): NoContextCodec[In, Out] = f(_)

    override def simpleUnlift[In, Out](pipe: NoContextCodec[In, Out], in: In): Either[List[String], Out] =
      pipe.decode(in)

    override def pureResult[A](a: A): Either[List[String], A] = Right(a)

    override def mergeResults[A, B, C](
      ra: Either[List[String], A],
      rb: => Either[List[String], B],
      f:  (A, B) => C
    ): Either[List[String], C] = (ra, rb) match {
      case (Right(a), Right(b)) => Right(f(a, b))
      case (Left(ea), Left(eb)) => Left(ea ++ eb)
      case (Left(e), _)         => Left(e)
      case (_, Left(e))         => Left(e)
    }
  }

  object Auto extends PipeAutoSupport[NoContextCodec]
}
