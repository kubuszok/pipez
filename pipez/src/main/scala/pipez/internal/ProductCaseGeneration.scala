package pipez.internal

import pipez.PipeDerivation
import pipez.internal.ProductCaseGeneration.dropGetIs

import scala.annotation.nowarn
import scala.collection.immutable.ListMap
import scala.util.chaining._

@nowarn("msg=The outer reference in this type test cannot be checked at run time.")
trait ProductCaseGeneration[Pipe[_, _], In, Out] { self: Definitions[Pipe, In, Out] with Generators[Pipe, In, Out] =>

  def isSubtype[A, B](lower: Type[A], higher: Type[B]): Boolean

  def isCaseClass[A](tpe:    Type[A]): Boolean
  def isJavaBean[A](tpe:     Type[A]): Boolean
  def isInstantiable[A](tpe: Type[A]): Boolean

  final def isUsableAsProductOutput: Boolean =
    (isCaseClass(outType) || isJavaBean(outType)) && isInstantiable(outType)

  final case class ProductInData(getters: ListMap[String, ProductInData.Getter[_]]) {

    def findGetter(inParamName: String, outParamName: String): DerivationResult[ProductInData.Getter[_]] =
      DerivationResult.fromOption(getters.collectFirst {
        case (_, getter) if getter.names.contains(inParamName) => getter
      })(DerivationError.MissingPublicSource(outParamName))
  }
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

  sealed trait OutFieldLogic[OutField] extends Product with Serializable
  object OutFieldLogic {

    final case class DefaultField[OutField]() extends OutFieldLogic[OutField]

    final case class FieldAdded[OutField](
      pipe: CodeOf[Pipe[In, OutField]]
    ) extends OutFieldLogic[OutField]

    final case class FieldRenamed[InField, OutField](
      inField:     String,
      inFieldType: Type[InField]
    ) extends OutFieldLogic[OutField]

    final case class PipeProvided[InField, OutField](
      inField:     String,
      inFieldType: Type[InField],
      pipe:        CodeOf[Pipe[InField, OutField]]
    ) extends OutFieldLogic[OutField]

    def resolve[OutField](
      settings:     Settings,
      outField:     String,
      outFieldType: Type[OutField]
    ): OutFieldLogic[OutField] = {
      import Path._
      import ConfigEntry._

      settings.resolve[OutFieldLogic[OutField]](DefaultField()) {
        case AddField(Field(Root, `outField`), pipe, outType) =>
          // validate that outType <:< outFieldType is correct
          FieldAdded(pipe.asInstanceOf[CodeOf[Pipe[In, OutField]]])
        case RenameField(Field(Root, inName), inType, Field(Root, `outField`), outType) =>
          // validate that outType <:< outFieldType is correct
          FieldRenamed(inName, inType)
        case PlugInField(Field(Root, inName), inType, Field(Root, `outField`), outType, pipe) =>
          // validate that outType <:< outFieldType is correct
          PipeProvided(inName, inType, pipe.asInstanceOf[CodeOf[Pipe[Any, OutField]]])
      }
    }
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

    final def unapply(settings: Settings): Option[DerivationResult[CodeOf[Pipe[In, Out]]]] =
      if (isUsableAsProductOutput) Some(attemptProductRendering(settings))
      else None

    def extractInData(settings: Settings): DerivationResult[ProductInData]

    def extractOutData(settings: Settings): DerivationResult[ProductOutData]

    def generateCode(generatorData: ProductGeneratorData): DerivationResult[CodeOf[Pipe[In, Out]]]

    private def attemptProductRendering(
      settings: Settings
    ): DerivationResult[CodeOf[Pipe[In, Out]]] =
      for {
        data <- extractInData(settings) zip extractOutData(settings)
        (inData, outData) = data
        generatorData <- matchFields(inData, outData, settings)
        code <- generateCode(generatorData)
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
        OutFieldLogic.resolve(settings, outParamName, outType) match {
          case OutFieldLogic.DefaultField() =>
            // if inField (same name as out) not found then error
            // else if inField <:< outField then (in, ctx) => in : OutField
            // else (in, ctx) => unlift(summon[InField, OutField])(in.default, ctx) : Result[OutField]
            inData.findGetter(outParamName, outParamName).flatMap(fromFieldConstructorParam(_, outType))
          case OutFieldLogic.FieldAdded(pipe) =>
            // (in, ctx) => unlift(pipe)(in, ctx) : Result[OutField]
            DerivationResult.pure(fieldAddedConstructorParam(pipe))
          case OutFieldLogic.FieldRenamed(inField, inFieldType) =>
            // if inField (name provided) not found then error
            // else if inField <:< outField then (in, ctx) => in : OutField
            // else (in, ctx) => unlift(summon[InField, OutField])(in.renamed, ctx) : Result[OutField]
            inData.findGetter(inField, outParamName).flatMap(fromFieldConstructorParam(_, outType))
          case OutFieldLogic.PipeProvided(inField, inFieldType, pipe) =>
            // if inField (name provided) not found then error
            // else (in, ctx) => unlift(summon[InField, OutField])(in.used, ctx) : Result[OutField]
            inData
              .findGetter(inField, outParamName)
              .map(g => pipeProvidedConstructorParam(g.asInstanceOf[ProductInData.Getter[Any]], pipe))
        }
    }

    // if inField <:< outField then (in, ctx) => in : OutField
    // else (in, ctx) => unlift(summon[InField, OutField])(in.inField, ctx) : Result[OutField]
    private def fromFieldConstructorParam[InField, OutField](
      getter:  ProductInData.Getter[InField],
      outType: Type[OutField]
    ): DerivationResult[ProductGeneratorData.ConstructorParam] = {
      val inType = getter.tpe
      if (isSubtype(inType, outType)) {
        DerivationResult.pure(ProductGeneratorData.ConstructorParam.Pure((in, _) => getter.caller(in)))
      } else {
        summonPipe(inType, outType).map { pipe: CodeOf[Pipe[InField, OutField]] =>
          ProductGeneratorData.ConstructorParam.Result { (in, ctx) =>
            unlift[InField, OutField](pipe, getter.caller(in), ctx)
          }
        }
      }
    }

    // (in, ctx) => unlift(pipe)(in, ctx) : Result[OutField]
    private def fieldAddedConstructorParam[OutField](
      pipe: CodeOf[Pipe[In, OutField]]
    ): ProductGeneratorData.ConstructorParam =
      ProductGeneratorData.ConstructorParam.Result((in, ctx) => unlift[In, OutField](pipe, inCode(in), ctx))

    // (in, ctx) => unlift(summon[InField, OutField])(in.used, ctx) : Result[OutField]
    private def pipeProvidedConstructorParam[InField, OutField](
      getter: ProductInData.Getter[InField],
      pipe:   CodeOf[Pipe[InField, OutField]]
    ): ProductGeneratorData.ConstructorParam =
      ProductGeneratorData.ConstructorParam.Result { (in, ctx) =>
        unlift[InField, OutField](pipe, getter.caller(in), ctx)
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
