package pipez.internal

import pipez.internal.ProductCaseGeneration.inputNameMatchesOutputName

import scala.annotation.nowarn
import scala.collection.immutable.ListMap
import scala.util.chaining.*

@nowarn("msg=The outer reference in this type test cannot be checked at run time.")
trait ProductCaseGeneration[Pipe[_, _], In, Out] { self: Definitions[Pipe, In, Out] & Generators[Pipe, In, Out] =>

  def isCaseClass[A](tpe:    Type[A]): Boolean
  def isJavaBean[A](tpe:     Type[A]): Boolean
  def isInstantiable[A](tpe: Type[A]): Boolean

  final def isUsableAsProductOutput: Boolean =
    (isCaseClass(outType) || isJavaBean(outType)) && isInstantiable(outType)

  type Constructor = List[List[CodeOf[?]]] => CodeOf[Out]

  final case class ProductInData(getters: ListMap[String, ProductInData.Getter[?]]) {

    def findGetter(
      inParamName:           String,
      outParamName:          String,
      caseInsensitiveSearch: Boolean
    ): DerivationResult[ProductInData.Getter[?]] =
      DerivationResult.fromOption(
        getters.collectFirst {
          case (_, getter) if inputNameMatchesOutputName(getter.name, inParamName, caseInsensitiveSearch) => getter
        }
      )(DerivationError.MissingPublicSource(outParamName))
  }
  object ProductInData {

    final case class Getter[InField](
      name: String,
      tpe:  Type[InField],
      get:  Argument[In] => CodeOf[InField]
    ) {

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
      params: List[ListMap[String, ConstructorParam[?]]]
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

      override def toString: String = s"Setter($name : $tpe)"
    }
    final case class JavaBean(
      defaultConstructor: CodeOf[Out],
      setters:            ListMap[String, Setter[?]]
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

    private def resolve[OutField](
      settings:     Settings,
      outFieldName: String,
      outFieldType: Type[OutField]
    ): OutFieldLogic[OutField] = {
      import Path.*
      import ConfigEntry.*

      // outFieldName matches what we found as setter/constructor param
      // outFieldGetter in matter matching, what we got from config - user might have used JavaBeans' getter!
      settings.resolve[OutFieldLogic[OutField]](DefaultField()) {
        case AddField(Field(Root, outFieldGetter), outFieldType, pipe)
            if inputNameMatchesOutputName(outFieldGetter, outFieldName, settings.isFieldCaseInsensitive) =>
          // TODO: validate that outType <:< outFieldType is correct
          FieldAdded(pipe.asInstanceOf[CodeOf[Pipe[In, OutField]]])
        case RenameField(Field(Root, inName), inType, Field(Root, outFieldGetter), outType)
            if inputNameMatchesOutputName(outFieldGetter, outFieldName, settings.isFieldCaseInsensitive) =>
          // TODO: validate that outType <:< outFieldType is correct
          FieldRenamed(inName, inType)
        case PlugInField(Field(Root, inName), inType, Field(Root, outFieldGetter), outType, pipe)
            if inputNameMatchesOutputName(outFieldGetter, outFieldName, settings.isFieldCaseInsensitive) =>
          // TODO: validate that outType <:< outFieldType is correct
          PipeProvided(inName, inType, pipe.asInstanceOf[CodeOf[Pipe[Any, OutField]]])
      }
    }

    def resolveField[OutField](
      settings:     Settings,
      inData:       ProductInData,
      outParamName: String,
      outParamType: Type[OutField]
    ): DerivationResult[ProductGeneratorData.OutputValue] = resolve(settings, outParamName, outParamType) match {
      case DefaultField() =>
        // if inField (same name as out) not found then error
        // else if inField <:< outField then (in, ctx) => in : OutField
        // else (in, ctx) => unlift(summon[InField, OutField])(in.outParamName, ctx) : Result[OutField]
        inData
          .findGetter(outParamName, outParamName, settings.isFieldCaseInsensitive)
          .flatMap(fromFieldConstructorParam(_, outParamType))
      case FieldAdded(pipe) =>
        // (in, ctx) => unlift(pipe)(in, ctx) : Result[OutField]
        DerivationResult.pure(fieldAddedConstructorParam(pipe, outParamType))
      case FieldRenamed(inFieldName, _) =>
        // if inField (name provided) not found then error
        // else if inField <:< outField then (in, ctx) => in : OutField
        // else (in, ctx) => unlift(summon[InField, OutField])(in.inFieldName, ctx) : Result[OutField]
        inData
          .findGetter(inFieldName, outParamName, settings.isFieldCaseInsensitive)
          .flatMap(fromFieldConstructorParam(_, outParamType))
      case PipeProvided(inFieldName, _, pipe) =>
        // if inField (name provided) not found then error
        // else (in, ctx) => unlift(summon[InField, OutField])(in.used, ctx) : Result[OutField]
        inData
          .findGetter(inFieldName, outParamName, settings.isFieldCaseInsensitive)
          .map(g => pipeProvidedConstructorParam(g.asInstanceOf[ProductInData.Getter[Any]], pipe, outParamType))
    }
  }

  sealed trait ProductGeneratorData extends Product with Serializable
  object ProductGeneratorData {

    sealed trait OutputValue extends Product with Serializable
    object OutputValue {

      final case class Pure[A](
        tpe:    Type[A],
        caller: (Argument[In], Argument[ArbitraryContext]) => CodeOf[A]
      ) extends OutputValue {
        override def toString: String = s"Pure { ($inType, Context) => $tpe }"
      }

      final case class Result[A](
        tpe:    Type[A],
        caller: (Argument[In], Argument[ArbitraryContext]) => CodeOf[ArbitraryResult[A]]
      ) extends OutputValue {
        override def toString: String = s"Result { ($inType, Context) => $tpe }"
      }
    }

    final case class CaseClass(
      caller: Constructor,
      output: List[List[OutputValue]]
    ) extends ProductGeneratorData {
      override def toString: String = s"CaseClass${output.map(list => "(" + list.mkString(", ") + ")").mkString}"
    }

    final case class JavaBean(
      caller: CodeOf[Out],
      output: List[(OutputValue, ProductOutData.Setter[?])]
    ) extends ProductGeneratorData {
      override def toString: String = s"JavaBean(${output.mkString(", ")})"
    }
  }

  object ProductTypeConversion extends CodeGeneratorExtractor {

    final def unapply(settings: Settings): Option[DerivationResult[CodeOf[Pipe[In, Out]]]] =
      if (isUsableAsProductOutput) Some(attemptProductRendering(settings))
      else None
  }

  def extractProductInData(settings: Settings): DerivationResult[ProductInData]

  def extractProductOutData(settings: Settings): DerivationResult[ProductOutData]

  def generateProductCode(generatorData: ProductGeneratorData): DerivationResult[CodeOf[Pipe[In, Out]]]

  private def attemptProductRendering(settings: Settings): DerivationResult[CodeOf[Pipe[In, Out]]] =
    for {
      data <- extractProductInData(settings) zip extractProductOutData(settings)
      (inData, outData) = data
      generatorData <- matchFields(inData, outData, settings)
      code <- generateProductCode(generatorData)
    } yield code

  // In the product derivation, the logic is driven by Out type:
  // - every field of Out should have an assigned value
  // - so we are iterating over the list of fields in Out and check the configuration for them
  // - additional fields in In can be safely ignored
  private def matchFields(
    inData:   ProductInData,
    outData:  ProductOutData,
    settings: Settings
  ): DerivationResult[ProductGeneratorData] = outData match {
    case ProductOutData.CaseClass(caller, listOfParamsList) =>
      listOfParamsList
        .map(
          _.values
            .map { case ProductOutData.ConstructorParam(outParamName, outParamType) =>
              OutFieldLogic.resolveField(settings, inData, outParamName, outParamType)
            }
            .toList
            .pipe(DerivationResult.sequence(_))
        )
        .pipe(DerivationResult.sequence(_))
        .map(ProductGeneratorData.CaseClass(caller, _))

    case ProductOutData.JavaBean(defaultConstructor, setters) =>
      setters.values
        .map { case setter @ ProductOutData.Setter(outSetterName, outSetterType, _) =>
          OutFieldLogic.resolveField(settings, inData, outSetterName, outSetterType).map(_ -> setter)
        }
        .toList
        .pipe(DerivationResult.sequence(_))
        .map(ProductGeneratorData.JavaBean(defaultConstructor, _))
  }

  // if inField <:< outField then (in, ctx) => in : OutField
  // else (in, ctx) => unlift(summon[InField, OutField])(in.inField, ctx) : Result[OutField]
  private def fromFieldConstructorParam[InField, OutField](
    getter:       ProductInData.Getter[InField],
    outFieldType: Type[OutField]
  ): DerivationResult[ProductGeneratorData.OutputValue] = {
    val inFieldType = getter.tpe
    if (isSubtype(inFieldType, outFieldType)) {
      DerivationResult.pure(
        ProductGeneratorData.OutputValue.Pure(
          outFieldType.asInstanceOf[Type[InField]],
          (in, _) => getter.get(in)
        )
      )
    } else {
      summonPipe(inFieldType, outFieldType).map { pipe: CodeOf[Pipe[InField, OutField]] =>
        ProductGeneratorData.OutputValue.Result(
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
  ): ProductGeneratorData.OutputValue = ProductGeneratorData.OutputValue.Result(
    outFieldType,
    (in, ctx) => unlift[In, OutField](pipe, inCode(in), ctx)
  )

  // (in, ctx) => unlift(summon[InField, OutField])(in.used, ctx) : Result[OutField]
  private def pipeProvidedConstructorParam[InField, OutField](
    getter:       ProductInData.Getter[InField],
    pipe:         CodeOf[Pipe[InField, OutField]],
    outFieldType: Type[OutField]
  ): ProductGeneratorData.OutputValue =
    ProductGeneratorData.OutputValue.Result(
      outFieldType,
      (in, ctx) => unlift[InField, OutField](pipe, getter.get(in), ctx)
    )
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

  private def inputNameMatchesOutputName(
    inFieldName:     String,
    outFieldName:    String,
    caseInsensitive: Boolean
  ): Boolean = {
    val in  = Set(inFieldName, dropGetIs(inFieldName))
    val out = Set(outFieldName, dropSet(outFieldName))
    if (caseInsensitive) in.exists(a => out.exists(b => a.equalsIgnoreCase(b)))
    else in.intersect(out).nonEmpty
  }
}
