package pipez.internal

import pipez.PipeDerivation
import pipez.internal.ProductCaseGeneration.dropGetIs

import scala.annotation.nowarn
import scala.collection.immutable.ListMap
import scala.util.chaining._

@nowarn("msg=The outer reference in this type test cannot be checked at run time.")
trait ProductCaseGeneration[Pipe[_, _], In, Out] { self: Definitions[Pipe, In, Out] with Generators[Pipe, In, Out] =>

  def isCaseClass[A](tpe:    Type[A]): Boolean
  def isJavaBean[A](tpe:     Type[A]): Boolean
  def isInstantiable[A](tpe: Type[A]): Boolean

  def isUsableAsProductOutput(tpe: Type[Out]): Boolean =
    (isCaseClass(tpe) || isJavaBean(tpe)) && isInstantiable(tpe)

  final case class ProductInData(getters: ListMap[String, ProductInData.Getter[_]])
  object ProductInData {

    final case class Getter[InField](
      name:   String,
      tpe:    Type[InField],
      caller: Argument[In] => CodeOf[InField]
    ) {

      lazy val nonJavaBeanName: String = ProductCaseGeneration.dropGetIs(name)

      lazy val names: Set[String] = Set(name, nonJavaBeanName)
    }
  }

  sealed trait ProductOutData extends Product with Serializable
  object ProductOutData {

    final case class ConstructorParam[OutField](
      name: String,
      tpe:  Type[OutField]
    )
    final case class CaseClass(params: List[ListMap[String, ConstructorParam[_]]]) extends ProductOutData

    final case class Setter[OutField](
      name:   String,
      tpe:    Type[OutField],
      caller: (Argument[Out], CodeOf[OutField]) => CodeOf[Unit]
    ) {

      lazy val nonJavaBeanName: String = ProductCaseGeneration.dropSet(name)

      lazy val names: Set[String] = Set(name, nonJavaBeanName)
    }
    final case class JavaBean(
      defaultConstructor: CodeOf[Out],
      setters:            ListMap[String, Setter[_]]
    ) extends ProductOutData
  }

  sealed trait ProductGeneratorData extends Product with Serializable
  object ProductGeneratorData {

    sealed trait ConstructorParam extends Product with Serializable
    object ConstructorParam {

      final case class Pure[A](
        caller: (Argument[In], Argument[ArbitraryContext]) => CodeOf[A]
      ) extends ConstructorParam

      final case class Result[A](
        caller: (Argument[In], Argument[ArbitraryContext]) => CodeOf[ArbitraryResult[A]]
      ) extends ConstructorParam
    }

    final case class CaseClass(
      pipes: List[List[ConstructorParam]]
    ) extends ProductGeneratorData

    final case class JavaBean() extends ProductGeneratorData // TODO
  }

  trait ProductTypeConversion extends CodeGeneratorExtractor {

    final def unapply(configuration: Configuration): Option[DerivationResult[CodeOf[Pipe[In, Out]]]] = {
      val Configuration(inType, outType, settings, pipeDerivation) = configuration

      if (isUsableAsProductOutput(outType)) Some(attemptProductRendering(inType, outType, settings, pipeDerivation))
      else None
    }

    def extractInData(inType: Type[In], settings: Settings): DerivationResult[ProductInData]

    def extractOutData(outType: Type[Out], settings: Settings): DerivationResult[ProductOutData]

    def generateCode(
      generatorData:  ProductGeneratorData,
      pipeDerivation: CodeOf[PipeDerivation[Pipe]]
    ): DerivationResult[CodeOf[Pipe[In, Out]]]

    private def attemptProductRendering(
      inType:         Type[In],
      outType:        Type[Out],
      settings:       Settings,
      pipeDerivation: CodeOf[PipeDerivation[Pipe]]
    ): DerivationResult[CodeOf[Pipe[In, Out]]] =
      for {
        data <- extractInData(inType, settings) zip extractOutData(outType, settings)
        (inData, outData) = data
        generatorData <- matchFields(inData, outData, settings)
        code <- generateCode(generatorData, pipeDerivation)
      } yield code

    private def matchFields(
      inData:   ProductInData,
      outData:  ProductOutData,
      settings: Settings
    ): DerivationResult[ProductGeneratorData] =
      outData match {
        case ProductOutData.CaseClass(listOfParamsList) =>
          listOfParamsList
            .map(
              _.values
                .map(assignConstructorParamPipe(inData, outData, settings))
                .toList
                .pipe(DerivationResult.sequence(_))
            )
            .pipe(DerivationResult.sequence(_))
            .map(ProductGeneratorData.CaseClass(_))

        case ProductOutData.JavaBean(defaultConstructor, setters) =>
          // TODO
          DerivationResult.fail(DerivationError.NotYetSupported)
      }

    private def assignConstructorParamPipe(
      inData:   ProductInData,
      outData:  ProductOutData,
      settings: Settings
    ): ProductOutData.ConstructorParam[_] => DerivationResult[ProductGeneratorData.ConstructorParam] = {
      case ProductOutData.ConstructorParam(outParamName, outType) =>
        settings.forOutputFieldUse(outParamName, outType) match {
          case OutFieldLogic.DefaultField() =>
            // TODO: match with field from inData
            // TODO: if inType <:< outType ProductGeneratorData.ConstructorParam.Pure
            // TODO: else attemptSummon
            // TODO:   then ProductGeneratorData.ConstructorParam.Result((in, ctx) => unlift(pipe)(in.field, ctx)
            // old code:
//            for {
//              getter <- DerivationResult.fromOption(inData.getters.find { case (_, getter) =>
//                getter.names.contains(outParamName)
//              })(DerivationError.MissingPublicSource(outParamName))
//              _ = println(s"for output ${outParamName} using field name ${inField}")
//              // TODO: create pipe or whatever
//              _ <- DerivationResult.fail(DerivationError.NotYetSupported)
//            } yield ProductGeneratorData.ConstructorParam(caller = ???)
            DerivationResult.fail(DerivationError.NotYetSupported)
          case OutFieldLogic.FieldAdded(pipe) =>
            // TODO: ProductGeneratorData.ConstructorParam.Result((in, ctx) => unlift(pipe)(in, ctx))
            DerivationResult.fail(DerivationError.NotYetSupported)
          case OutFieldLogic.FieldRenamed(inField, inFieldType) =>
            // TODO: summon Pipe[InField, OutField]
            // TODO: ProductGeneratorData.ConstructorParam.Result((in, ctx) => unlift(pipe)(in.inField, ctx))
            DerivationResult.fail(DerivationError.NotYetSupported)
          case OutFieldLogic.PipeProvided(inField, inFieldType, pipe) =>
            // TODO: ProductGeneratorData.ConstructorParam.Result((in, ctx) => unlift(pipe)(in.field, ctx))
            DerivationResult.fail(DerivationError.NotYetSupported)
        }
    }

    private def assignSetterPipe(
      inData:   ProductInData,
      outData:  ProductOutData,
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
