package pipez.internal

trait PlatformSumCaseGeneration[Pipe[_, _], In, Out] extends SumCaseGeneration[Pipe, In, Out] {
  self: PlatformDefinitions[Pipe, In, Out] & PlatformGenerators[Pipe, In, Out] =>

  import c.universe.*

  def isSumType[A](tpe: Type[A]): Boolean =
    tpe.typeSymbol.isClass && tpe.typeSymbol.asClass.isSealed
}
