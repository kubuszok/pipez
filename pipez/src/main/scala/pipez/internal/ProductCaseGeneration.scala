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

  type Constructor = List[List[CodeOf[_]]] => CodeOf[Out]

  final case class ProductInData(getters: ListMap[String, ProductInData.Getter[_]]) {

    def findGetter(
      inParamName:           String,
      outParamName:          String,
      caseInsensitiveSearch: Boolean
    ): DerivationResult[ProductInData.Getter[_]] =
      if (caseInsensitiveSearch)
        DerivationResult.fromOption(getters.collectFirst {
          case (_, getter) if getter.names.exists(name => name.equalsIgnoreCase(inParamName)) => getter
        })(DerivationError.MissingPublicSource(outParamName))
      else
        DerivationResult.fromOption(getters.collectFirst {
          case (_, getter) if getter.names.contains(inParamName) => getter
        })(DerivationError.MissingPublicSource(outParamName))
  }
  object ProductInData {

    final case class Getter[InField](
      name: String,
      tpe:  Type[InField],
      get:  Argument[In] => CodeOf[InField]
    ) {

      lazy val nonJavaBeanName: String = ProductCaseGeneration.dropGetIs(name)

      lazy val names: Set[String] = Set(name, nonJavaBeanName)

      override def toString: String = s"Getter($name : $tpe)"
    }
  }

  sealed trait ProductOutData extends Product with Serializable
  object ProductOutData {

    final case class ConstructorParam[OutField](
      name: String,
      tpe:  Type[OutField]
    )
    final case class CaseClass(
      caller: Constructor,
      params: List[ListMap[String, ConstructorParam[_]]]
    ) extends ProductOutData {
      override def toString: String = s"CaseClass${params.map { list =>
          "(" + list.map { case (n, p) => s"$n : ${p.tpe}" }.mkString(", ") + ")"
        }.mkString}"
    }

    final case class Setter[OutField](
      name: String,
      tpe:  Type[OutField],
      set:  (Argument[Out], CodeOf[OutField]) => CodeOf[Unit]
    ) {

      lazy val nonJavaBeanName: String = ProductCaseGeneration.dropSet(name)

      lazy val names: Set[String] = Set(name, nonJavaBeanName)

      override def toString: String = s"Setter($name : $tpe)"
    }
    final case class JavaBean(
      defaultConstructor: CodeOf[Out],
      setters:            ListMap[String, Setter[_]]
    ) extends ProductOutData {
      override def toString: String = s"JavaBean(${setters.map { case (n, p) => s"$n : $p" }.mkString(", ")})"
    }
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
        case AddField(Field(Root, `outField`), outFieldType, pipe) =>
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
        tpe:    Type[A],
        caller: (Argument[In], Argument[ArbitraryContext]) => CodeOf[A]
      ) extends ConstructorParam {
        override def toString: String = s"Pure { ($inType, Context) => $tpe }"
      }

      final case class Result[A](
        tpe:    Type[A],
        caller: (Argument[In], Argument[ArbitraryContext]) => CodeOf[ArbitraryResult[A]]
      ) extends ConstructorParam {
        override def toString: String = s"Result { ($inType, Context) => $tpe }"
      }
    }

    final case class CaseClass(
      caller: Constructor,
      pipes:  List[List[ConstructorParam]]
    ) extends ProductGeneratorData {
      override def toString: String = s"CaseClass${pipes.map(list => "(" + list.mkString(", ") + ")").mkString}"
    }

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
        case ProductOutData.CaseClass(caller, listOfParamsList) =>
          listOfParamsList
            .map(
              _.values
                .map(assignConstructorParamPipe(inData, outData, settings))
                .toList
                .pipe(DerivationResult.sequence(_))
            )
            .pipe(DerivationResult.sequence(_))
            .map(ProductGeneratorData.CaseClass(caller, _))

        case ProductOutData.JavaBean(defaultConstructor, setters) =>
          // TODO
          DerivationResult.fail(DerivationError.NotYetImplemented("JavaBean output"))
      }

    private def assignConstructorParamPipe(
      inData:   ProductInData,
      outData:  ProductOutData,
      settings: Settings
    ): ProductOutData.ConstructorParam[_] => DerivationResult[ProductGeneratorData.ConstructorParam] = {
      case ProductOutData.ConstructorParam(outParamName, outParamType) =>
        OutFieldLogic.resolve(settings, outParamName, outParamType) match {
          case OutFieldLogic.DefaultField() =>
            // if inField (same name as out) not found then error
            // else if inField <:< outField then (in, ctx) => in : OutField
            // else (in, ctx) => unlift(summon[InField, OutField])(in.outParamName, ctx) : Result[OutField]
            inData.findGetter(outParamName, outParamName, settings.isFieldCaseInsensitive).flatMap(fromFieldConstructorParam(_, outParamType))
          case OutFieldLogic.FieldAdded(pipe) =>
            // (in, ctx) => unlift(pipe)(in, ctx) : Result[OutField]
            DerivationResult.pure(fieldAddedConstructorParam(pipe, outParamType))
          case OutFieldLogic.FieldRenamed(inFieldName, inFieldType) =>
            // if inField (name provided) not found then error
            // else if inField <:< outField then (in, ctx) => in : OutField
            // else (in, ctx) => unlift(summon[InField, OutField])(in.inFieldName, ctx) : Result[OutField]
            inData.findGetter(inFieldName, outParamName, settings.isFieldCaseInsensitive).flatMap(fromFieldConstructorParam(_, outParamType))
          case OutFieldLogic.PipeProvided(inFieldName, inFieldType, pipe) =>
            // if inField (name provided) not found then error
            // else (in, ctx) => unlift(summon[InField, OutField])(in.used, ctx) : Result[OutField]
            inData
              .findGetter(inFieldName, outParamName, settings.isFieldCaseInsensitive)
              .map(g => pipeProvidedConstructorParam(g.asInstanceOf[ProductInData.Getter[Any]], pipe, outParamType))
        }
    }

    // if inField <:< outField then (in, ctx) => in : OutField
    // else (in, ctx) => unlift(summon[InField, OutField])(in.inField, ctx) : Result[OutField]
    private def fromFieldConstructorParam[InField, OutField](
      getter:       ProductInData.Getter[InField],
      outFieldType: Type[OutField]
    ): DerivationResult[ProductGeneratorData.ConstructorParam] = {
      val inFieldType = getter.tpe
      if (isSubtype(inFieldType, outFieldType)) {
        DerivationResult.pure(
          ProductGeneratorData.ConstructorParam.Pure(
            outFieldType.asInstanceOf[Type[InField]],
            (in, _) => getter.get(in)
          )
        )
      } else {
        summonPipe(inFieldType, outFieldType).map { pipe: CodeOf[Pipe[InField, OutField]] =>
          ProductGeneratorData.ConstructorParam.Result(
            outFieldType,
            (in, ctx) => unlift[InField, OutField](pipe, getter.get(in), ctx)
          )
        }
      }
    }

    // (in, ctx) => unlift(pipe)(in, ctx) : Result[OutField]
    private def fieldAddedConstructorParam[OutField](
      pipe:         CodeOf[Pipe[In, OutField]],
      outFieldType: Type[OutField]
    ): ProductGeneratorData.ConstructorParam = ProductGeneratorData.ConstructorParam.Result(
      outFieldType,
      (in, ctx) => unlift[In, OutField](pipe, inCode(in), ctx)
    )

    // (in, ctx) => unlift(summon[InField, OutField])(in.used, ctx) : Result[OutField]
    private def pipeProvidedConstructorParam[InField, OutField](
      getter:       ProductInData.Getter[InField],
      pipe:         CodeOf[Pipe[InField, OutField]],
      outFieldType: Type[OutField]
    ): ProductGeneratorData.ConstructorParam =
      ProductGeneratorData.ConstructorParam.Result(
        outFieldType,
        (in, ctx) => unlift[InField, OutField](pipe, getter.get(in), ctx)
      )

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
