package pipez.internal

trait PlatformDispatchers extends Dispatchers { self: PlatformDefinitions =>

  implicit private class TypeHelpers(tpe: Type) {

    def isCaseClass: Boolean = tpe.typeSymbol.asClass.isCaseClass

    def possibleJavaBean: Boolean = tpe.typeSymbol.isClass && !tpe.typeSymbol.isAbstract
  }

  private class BadCaseGenerator[Pipe[_, _], In, Out](error: DerivationError) extends CodeGenerator[Pipe, In, Out] {

    override def generate: DerivationResult[c.Expr[Pipe[In, Out]]] = DerivationResult.fail(error)
  }

  object ProductTypeConversion extends RendererExtractor {

    override def unapply[Pipe[_, _], In, Out](
      configuration: Configuration[Pipe, In, Out]
    ): Option[CodeGenerator[Pipe, In, Out]] = {
      val Configuration(inType, outType, settings, pipeDerivation) = configuration

      val productCaseInput  = inType.isCaseClass || (inType.possibleJavaBean && settings.isJavaBeanInputAllowed)
      val productCaseOutput = outType.isCaseClass || (outType.possibleJavaBean && settings.isJavaBeanOutputAllowed)

      (productCaseInput, productCaseOutput) match {
        case (true, true) => ???
        case (_, true)    => Some(reportMismatchingInput(inType, outType))
        case (true, _)    => Some(reportMismatchingOutput(inType, outType))
        case _            => None
      }
      // if ()
      None
    }

    private def reportMismatchingInput[Pipe[_, _], In, Out](
      inType:  Type,
      outType: Type
    ): CodeGenerator[Pipe, In, Out] =
      new BadCaseGenerator(
        DerivationError.InvalidConfiguration(
          s"While output type ${outType.toString} seem to be a case class or a Java bean, the input type ${inType.toString} doesn't"
        )
      )

    private def reportMismatchingOutput[Pipe[_, _], In, Out](
      inType:  Type,
      outType: Type
    ): CodeGenerator[Pipe, In, Out] =
      new BadCaseGenerator(
        DerivationError.InvalidConfiguration(
          s"While input type ${outType.toString} seem to be a case class or a Java bean, the output type ${inType.toString} doesn't"
        )
      )
  }

  object SumTypeConversion extends RendererExtractor {
    override def unapply[Pipe[_, _], In, Out](
      configuration: Configuration[Pipe, In, Out]
    ): Option[CodeGenerator[Pipe, In, Out]] = None
  }
}
