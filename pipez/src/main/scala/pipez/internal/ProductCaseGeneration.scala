package pipez.internal

import pipez.PipeDerivation

import scala.annotation.nowarn
import scala.collection.immutable.ListMap

@nowarn("msg=The outer reference in this type test cannot be checked at run time.")
trait ProductCaseGeneration { self: Definitions with Dispatchers =>

  def isUsableAsProductInput[In](tpe:   Type[In]):  Boolean
  def isUsableAsProductOutput[Out](tpe: Type[Out]): Boolean
  def isCaseClass[A](tpe:               Type[A]):   Boolean
  def isJavaBean[A](tpe:                Type[A]):   Boolean

  final case class InData(getters: ListMap[String, InData.GetterData[_]]) {

    def dropJavaGetterPrefix: InData =
      InData(getters.map { case (name, data) => ProductCaseGeneration.dropGetIs(name) -> data })
  }
  object InData {

    final case class GetterData[InField](
      name:   String,
      tpe:    Type[InField],
      caller: Argument => CodeOf[InField]
    )
  }

  sealed trait OutData extends Product with Serializable
  object OutData {

    final case class ConstructorParamData[OutField](
      name: String,
      tpe:  Type[OutField]
    )
    final case class CaseClassData(params: List[ListMap[String, ConstructorParamData[_]]]) extends OutData

    final case class SetterData[OutField](
      name:   String,
      tpe:    Type[OutField],
      caller: (Argument, CodeOf[OutField]) => CodeOf[Unit]
    )
    final case class JavaBeanData[Out](
      defaultConstructor: CodeOf[Out],
      setters:            ListMap[String, SetterData[_]]
    ) extends OutData
  }

  trait ProductTypeConversion extends CodeGeneratorExtractor {

    final def unapply[Pipe[_, _], In, Out](
      configuration: Configuration[Pipe, In, Out]
    ): Option[DerivationResult[CodeOf[Pipe[In, Out]]]] = {
      val Configuration(inType, outType, settings, pipeDerivation) = configuration

      val productCaseInput  = isUsableAsProductInput(inType)
      val productCaseOutput = isUsableAsProductInput(outType)

      (productCaseInput, productCaseOutput) match {
        case (true, true)  => Some(attemptProductRendering(inType, outType, settings, pipeDerivation))
        case (false, true) => Some(reportMismatchingInput(inType, outType))
        case (true, false) => Some(reportMismatchingOutput(inType, outType))
        case _             => None
      }
    }

    private def attemptProductRendering[Pipe[_, _], In, Out](
      inType:         Type[In],
      outType:        Type[Out],
      settings:       Settings,
      pipeDerivation: CodeOf[PipeDerivation[Pipe]]
    ): DerivationResult[CodeOf[Pipe[In, Out]]] =
      (inputData(inType, settings) zip outputData(outType, settings)).flatMap { _ =>
        // TODO: matching these together
        DerivationResult.fail(DerivationError.InvalidConfiguration("Work in Progress"))
      }

    def inputData[In](inType: Type[In], settings: Settings): DerivationResult[InData]

    def outputData[Out](outType: Type[Out], settings: Settings): DerivationResult[OutData]

    private def reportMismatchingInput[Pipe[_, _], In, Out](
      inType:  Type[In],
      outType: Type[Out]
    ): DerivationResult[CodeOf[Pipe[In, Out]]] = DerivationResult.fail(
      DerivationError.InvalidConfiguration(
        s"While output type ${outType.toString} seem to be a case class or a Java bean, the input type ${inType.toString} doesn't"
      )
    )

    private def reportMismatchingOutput[Pipe[_, _], In, Out](
      inType:  Type[In],
      outType: Type[Out]
    ): DerivationResult[CodeOf[Pipe[In, Out]]] = DerivationResult.fail(
      DerivationError.InvalidConfiguration(
        s"While input type ${inType.toString} seem to be a case class or a Java bean, the output type ${outType.toString} doesn't"
      )
    )
  }

  val ProductTypeConversion: ProductTypeConversion
}
object ProductCaseGeneration {

  private val getAccessor = raw"get(.)(.*)".r
  private val isAccessor  = raw"is(.)(.*)".r
  private val dropGetIs: String => String = {
    case getAccessor(head, tail) => head.toLowerCase + tail
    case isAccessor(head, tail)  => head.toLowerCase + tail
    case other                   => other
  }
}
