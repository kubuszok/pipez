package pipez

/** Some Decoder that needs `In` and `Driver` <=> `(Boolean, String)` to parse `In` to `Either[List[String], Out]` */
trait ContextCodec[In, Out] {

  def decode(in: In, shouldFailFast: Boolean, path: String): Either[List[String], Out]
}

object ContextCodec extends PipeSemiautoSupport[ContextCodec] with PipeSemiautoConfiguredSupport[ContextCodec] {

  final case class Driver(shouldFailFast: Boolean, path: String)

  // Information for macros to let them now how to combine smaller instances into bigger instances
  implicit val pipeDerivation: PipeDerivation[ContextCodec] = new PipeDerivation[ContextCodec] {

    final type Context = Driver

    final type Result[Out] = Either[List[String], Out]

    override def lift[In, Out](f: (In, Driver) => Either[List[String], Out]): ContextCodec[In, Out] =
      (in, shouldFailFast, path) => f(in, Driver(shouldFailFast, path))

    override def unlift[In, Out](pipe: ContextCodec[In, Out], in: In, context: Driver): Either[List[String], Out] =
      pipe.decode(in, context.shouldFailFast, context.path)

    override def pureResult[A](a: A): Either[List[String], A] = Right(a)

    override def updateContext(context: Driver, path: Path): Driver = {
      def newPath(update: Path): String = update match {
        case Path.Root                => context.path
        case Path.Field(from, name)   => s"${newPath(from)}.$name"
        case Path.Subtype(from, name) => s"(${newPath(from)}: $name)"
      }
      context.copy(path = newPath(path))
    }

    override def mergeResults[A, B, C](
      context: Driver,
      ra:      Either[List[String], A],
      rb:      => Either[List[String], B],
      f:       (A, B) => C
    ): Either[List[String], C] =
      if (context.shouldFailFast) {
        ra match {
          case Right(a) =>
            rb match {
              case Right(b) => Right(f(a, b))
              case Left(e)  => Left(e)
            }
          case Left(e) => Left(e)
        }
      } else {
        (ra, rb) match {
          case (Right(a), Right(b)) => Right(f(a, b))
          case (Left(ea), Left(eb)) => Left(ea ++ eb)
          case (Left(e), _)         => Left(e)
          case (_, Left(e))         => Left(e)
        }
      }
  }

  // after importing `ContextCodec.Auto.*`, it provides ability to derive things automatically
  object Auto extends PipeAutoSupport[ContextCodec]
}
