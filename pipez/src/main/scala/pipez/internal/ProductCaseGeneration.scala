package pipez.internal

import pipez.PipeDerivation
import pipez.internal.ProductCaseGeneration.dropGetIs

import scala.annotation.nowarn
import scala.collection.immutable.ListMap
import scala.util.chaining._

@nowarn("msg=The outer reference in this type test cannot be checked at run time.")
trait ProductCaseGeneration { self: Definitions with Dispatchers =>

  def isUsableAsProductOutput[Out](tpe: Type[Out]): Boolean
  def isCaseClass[A](tpe:               Type[A]):   Boolean
  def isJavaBean[A](tpe:                Type[A]):   Boolean

  final case class InData(getters: ListMap[String, InData.Getter[_]])
  object InData {

    final case class Getter[InField](
      name:   String,
      tpe:    Type[InField],
      caller: Argument => CodeOf[InField]
    ) {

      lazy val nonJavaBeanName: String = ProductCaseGeneration.dropGetIs(name)
    }
  }

  sealed trait OutData extends Product with Serializable
  object OutData {

    final case class ConstructorParam[OutField](
      name: String,
      tpe:  Type[OutField]
    )
    final case class CaseClass(params: List[ListMap[String, ConstructorParam[_]]]) extends OutData

    final case class Setter[OutField](
      name:   String,
      tpe:    Type[OutField],
      caller: (Argument, CodeOf[OutField]) => CodeOf[Unit]
    ) {

      lazy val nonJavaBeanName: String = ProductCaseGeneration.dropSet(name)
    }
    final case class JavaBean[Out](
      defaultConstructor: CodeOf[Out],
      setters:            ListMap[String, Setter[_]]
    ) extends OutData
  }

  sealed trait GeneratorData extends Product with Serializable
  object GeneratorData {

    final case class CaseClass(pipes: List[List[Any]]) extends GeneratorData // TODO

    final case class JavaBean() extends GeneratorData // TODO
  }

  trait ProductTypeConversion extends CodeGeneratorExtractor {

    final def unapply[Pipe[_, _], In, Out](
      configuration: Configuration[Pipe, In, Out]
    ): Option[DerivationResult[CodeOf[Pipe[In, Out]]]] = {
      val Configuration(inType, outType, settings, pipeDerivation) = configuration

      if (isUsableAsProductOutput(outType)) Some(attemptProductRendering(inType, outType, settings, pipeDerivation))
      else None
    }

    def extractInData[In](inType: Type[In], settings: Settings): DerivationResult[InData]

    def extractOutData[Out](outType: Type[Out], settings: Settings): DerivationResult[OutData]

    def generateCode[Pipe[_, _], In, Out](
      generatorData:  GeneratorData,
      pipeDerivation: CodeOf[PipeDerivation[Pipe]]
    ): DerivationResult[CodeOf[Pipe[In, Out]]]

    private def attemptProductRendering[Pipe[_, _], In, Out](
      inType:         Type[In],
      outType:        Type[Out],
      settings:       Settings,
      pipeDerivation: CodeOf[PipeDerivation[Pipe]]
    ): DerivationResult[CodeOf[Pipe[In, Out]]] =
      for {
        data <- extractInData(inType, settings) zip extractOutData(outType, settings)
        (inData, outData) = data
        generatorData <- matchFields(inData, outData, settings)
        code <- generateCode[Pipe, In, Out](generatorData, pipeDerivation)
      } yield code

    private def matchFields(inData: InData, outData: OutData, settings: Settings): DerivationResult[GeneratorData] =
      outData match {
        case OutData.CaseClass(listOfParamsList) =>
          listOfParamsList
            .map(_.map(assignConstructorParamPipe(inData, outData, settings)).toList.pipe(DerivationResult.sequence(_)))
            .pipe(DerivationResult.sequence(_))
            .map(GeneratorData.CaseClass(_))

        case OutData.JavaBean(defaultConstructor, setters) =>
          // TODO
          DerivationResult.fail(DerivationError.NotYetSupported)
      }

    private def assignConstructorParamPipe(
      inData:   InData,
      outData:  OutData,
      settings: Settings
    ): ((String, OutData.ConstructorParam[_])) => DerivationResult[Any] = { case (outParamName, constructorParam) =>
      settings.forOutputFieldUse(outParamName) match {
        case Left(pipe) =>
          println(s"for output ${outParamName} using pipe ${pipe}")
          DerivationResult.pure(())
        case Right(inField) =>
          println(s"for output ${outParamName} using field name ${inField}")
          DerivationResult.pure(())
      }
    }

    private def assignSetterPipe(
      inData:   InData,
      outData:  OutData,
      settings: Settings
    ) = {
      // TODO: here test all: name, nonJavaBeanName against name, nonJavaBeanName
    }
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

  private val setAccessor = raw"set(.)(.*)".r
  private val dropSet: String => String = {
    case setAccessor(head, tail) => head.toLowerCase + tail
    case other                   => other
  }
}
