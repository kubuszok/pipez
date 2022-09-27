package pipez.internal

import pipez.internal.Definitions.{ Context, Result }

import scala.annotation.nowarn

@nowarn("msg=The outer reference in this type test cannot be checked at run time.")
private[internal] trait AnyValCaseGeneration[Pipe[_, _], In, Out] {
  self: Definitions[Pipe, In, Out] & Generators[Pipe, In, Out] =>

  /** True iff `A` is `AnyVal` */
  def isAnyVal[A: Type]: Boolean

  /** True iff `A` is a primitive type */
  def isPrimitive[A: Type]: Boolean

  final def isUsableAsAnyValConversion: Boolean = {
    val inAnyVal     = isAnyVal[In]
    val inPrimitive  = isPrimitive[In]
    val outAnyVal    = isAnyVal[Out]
    val outPrimitive = isAnyVal[Out]
    (inAnyVal || outAnyVal) && (inAnyVal || inPrimitive) && (outAnyVal || outPrimitive)
  }

  sealed trait AnyValInData[InPrimitive] extends Product with Serializable {
    def tpe: Type[InPrimitive]
  }
  object AnyValInData {

    final case class InAnyVal[InPrimitive](
      tpe:  Type[InPrimitive],
      get:  Expr[In] => Expr[InPrimitive],
      path: Path
    ) extends AnyValInData[InPrimitive]

    final case class InPrimitive[InPrimitive](
      tpe: Type[InPrimitive]
    ) extends AnyValInData[InPrimitive]
  }

  sealed trait AnyValOutData[OutPrimitive] extends Product with Serializable {
    def tpe: Type[OutPrimitive]
  }
  object AnyValOutData {

    final case class OutAnyVal[OutPrimitive](
      tpe: Type[OutPrimitive],
      put: Expr[OutPrimitive] => Expr[Out]
    ) extends AnyValOutData[OutPrimitive]

    final case class OutPrimitive[OutPrimitive](
      tpe: Type[OutPrimitive]
    ) extends AnyValOutData[OutPrimitive]
  }

  type AnyValType
  final case class AnyValGeneratorData(repack: Expr[In] => Expr[Out])

  object AnyValConversion {

    final def unapply(settings: Settings): Option[DerivationResult[Expr[Pipe[In, Out]]]] =
      if (isUsableAsAnyValConversion) Some(attemptAnyValRendering(settings)) else None
  }

  /** Platform-specific way of parsing `In` data
    *
    * Should:
    *   - obtain a single value from AnyVal
    *   - or pass primitive unchanged
    */
  def extractAnyValInData(settings: Settings): DerivationResult[AnyValInData[AnyValType]]

  /** Platform-specific way of parsing `Out` data
    *
    * Should:
    *   - wrap a single value with AnyVal type
    *   - or leave primitive unchanged
    */
  def extractAnyValOutData(settings: Settings): DerivationResult[AnyValOutData[AnyValType]]

  /** Platform-specific way of generating code from resolved information
    *
    *   - should unwrap In value if needed
    *   - wrap in Out type if needed
    *   - add pure and lift
    *
    * {{{
    * pipeDerivation.lift { (in, ctx) =>
    *   pipeDerivation.pure(wrap(unwrap(in)))
    * }
    * }}}
    */
  def generateAnyValCode(generatorData: AnyValGeneratorData): DerivationResult[Expr[Pipe[In, Out]]]

  private def attemptAnyValRendering(settings: Settings): DerivationResult[Expr[Pipe[In, Out]]] = for {
    data <- extractAnyValInData(settings).zip(extractAnyValOutData(settings))
    (inData, outData) = data
    _ <-
      if (isSubtype(inData.tpe, outData.tpe)) DerivationResult.pure(())
      else
        DerivationResult.fail(
          DerivationError.InvalidInput("AnyVal conversion impossible: types don't match after unwrapping")
        )
    generatorData <- repackAnyValData(inData, outData)
    code <- generateAnyValCode(generatorData)
  } yield code

  private def repackAnyValData(
    inData:  AnyValInData[AnyValType],
    outData: AnyValOutData[AnyValType]
  ): DerivationResult[AnyValGeneratorData] = DerivationResult.pure {
    val unwrap = inData match {
      case AnyValInData.InAnyVal(_, get, _) => (in: Expr[In]) => get(in)
      case AnyValInData.InPrimitive(_)      => (in: Expr[In]) => in.asInstanceOf[Expr[AnyValType]]
    }
    val wrap = outData match {
      case AnyValOutData.OutAnyVal(_, put) => (in: Expr[AnyValType]) => put(in)
      case AnyValOutData.OutPrimitive(_)   => (in: Expr[AnyValType]) => in.asInstanceOf[Expr[Out]]
    }
    AnyValGeneratorData(unwrap andThen wrap)
  }
}
