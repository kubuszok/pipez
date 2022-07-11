package pipez.internal

import pipez.PipeDerivation

trait ProductCaseGeneration { self: Definitions with Dispatchers =>

  def isUsableAsProductInput(tpe:  Type): Boolean
  def isUsableAsProductOutput(tpe: Type): Boolean
  def isCaseClass(tpe:             Type): Boolean
  def isJavaBean(tpe:              Type): Boolean

  trait ProductTypeConversion extends RendererExtractor {

    final def unapply[Pipe[_, _], In, Out](
      configuration: Configuration[Pipe, In, Out]
    ): Option[CodeGenerator[Pipe, In, Out]] = {
      val Configuration(inType, outType, settings, pipeDerivation) = configuration

      val productCaseInput  = isUsableAsProductInput(inType)
      val productCaseOutput = isUsableAsProductInput(outType)

      (productCaseInput, productCaseOutput) match {
        case (true, true) => Some(attemptProductRendering(inType, outType, settings, pipeDerivation))
        case (_, true)    => Some(reportMismatchingInput(inType, outType))
        case (true, _)    => Some(reportMismatchingOutput(inType, outType))
        case _            => None
      }
    }

    private def attemptProductRendering[Pipe[_, _], In, Out](
      inType:         Type,
      outType:        Type,
      settings:       Settings,
      pipeDerivation: CodeOf[PipeDerivation[Pipe]]
    ): CodeGenerator[Pipe, In, Out] = new CodeGenerator.Failing(
      DerivationError.InvalidConfiguration("YOLO")
    )

    private def reportMismatchingInput[Pipe[_, _], In, Out](
      inType:  Type,
      outType: Type
    ): CodeGenerator[Pipe, In, Out] = new CodeGenerator.Failing(
      DerivationError.InvalidConfiguration(
        s"While output type ${outType.toString} seem to be a case class or a Java bean, the input type ${inType.toString} doesn't"
      )
    )

    private def reportMismatchingOutput[Pipe[_, _], In, Out](
      inType:  Type,
      outType: Type
    ): CodeGenerator[Pipe, In, Out] = new CodeGenerator.Failing(
      DerivationError.InvalidConfiguration(
        s"While input type ${inType.toString} seem to be a case class or a Java bean, the output type ${outType.toString} doesn't"
      )
    )
  }

  val ProductTypeConversion: ProductTypeConversion
}
