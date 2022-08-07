package pipez.internal

import scala.annotation.nowarn
import scala.collection.immutable.ListMap
import scala.util.chaining.scalaUtilChainingOps

@nowarn("msg=The outer reference in this type test cannot be checked at run time.")
trait SumCaseGeneration[Pipe[_, _], In, Out] { self: Definitions[Pipe, In, Out] & Generators[Pipe, In, Out] =>

  def isADT[A](tpe:      Type[A]): Boolean
  def isJavaEnum[A](tpe: Type[A]): Boolean

  final def isSumType[A](tpe: Type[A]): Boolean =
    isADT(tpe) || isJavaEnum(tpe)
  final def isUsableAsSumTypeConversion: Boolean =
    isSumType(inType) && isSumType(outType)

  sealed trait EnumData[A] extends Product with Serializable

  object EnumData {

    final case class SumType[A](elements: List[SumType.Case[? <: A]]) extends EnumData[A]

    object SumType {

      final case class Case[A](name: String, tpe: Type[A], isCaseObject: Boolean) {
        override def toString: String = s"Case($name)"
      }
    }

    final case class Enumeration[A](values: List[Enumeration.Value[A]]) extends EnumData[A]

    object Enumeration {

      final case class Value[A](name: String, path: CodeOf[A]) {
        override def toString: String = s"Value($name)"
      }
    }
  }

  sealed trait InSubtypeLogic[InSubtype <: In] extends Product with Serializable

  object InSubtypeLogic {

    final case class DefaultSubtype[InSubtype <: In]() extends InSubtypeLogic[InSubtype]

    final case class SubtypeRemoved[InSubtype <: In](
      pipe: CodeOf[Pipe[InSubtype, Out]]
    ) extends InSubtypeLogic[InSubtype]

    final case class SubtypeRenamed[InSubtype <: In, OutSubtype <: Out](
      outSubtype: Type[OutSubtype]
    ) extends InSubtypeLogic[InSubtype]

    final case class PipeProvided[InSubtype <: In, OutSubtype <: Out](
      outSubtype: Type[OutSubtype],
      pipe:       CodeOf[Pipe[InSubtype, OutSubtype]]
    ) extends InSubtypeLogic[InSubtype]

    def resolve[InSubtype <: In](
      settings:      Settings,
      inSubtypeName: String,
      inSubtypeType: Type[InSubtype]
    ): InSubtypeLogic[InSubtype] = {
      import ConfigEntry.*

      settings.resolve[InSubtypeLogic[InSubtype]](DefaultSubtype()) {
        case RemoveSubtype(_, `inSubtypeType`, pipe) =>
          SubtypeRemoved(pipe.asInstanceOf[CodeOf[Pipe[InSubtype, Out]]])
        case RenameSubtype(_, `inSubtypeType`, _, outSubtypeType) =>
          SubtypeRenamed(outSubtypeType)
        case PlugInSubtype(_, `inSubtypeType`, _, outSubtypeType, pipe) =>
          PipeProvided[InSubtype, Out](outSubtypeType, pipe.asInstanceOf[CodeOf[Pipe[InSubtype, Out]]])
      }
    }

    // TODO: write EnumGeneratorData
    // TODO: dispatch to EnumGeneratorData
    def resolveSubtype[InSubtype <: In](
      settings:      Settings,
      outData:       EnumData[Out],
      inSubtypeName: String,
      inSubtypeType: Type[InSubtype]
    ): Unit = resolve(settings, inSubtypeName, inSubtypeType) match {
      case InSubtypeLogic.DefaultSubtype() =>
        // TODO: summon[Pipe[InSubtype, OutSubtype]] for Subtype with matching name
        ???
      case InSubtypeLogic.SubtypeRemoved(pipe) =>
        // TODO: just return the pipe
        ???
      case InSubtypeLogic.SubtypeRenamed(outSubtype) =>
        // TODO: summon[Pipe[InSubtype, OutType]] for specified subtype
        ???
      case InSubtypeLogic.PipeProvided(outSubtype, pipe) =>
        // TODO: just return the pipe
        ???
    }
  }

  sealed trait EnumGeneratorData extends Product with Serializable
  object EnumGeneratorData {

    sealed trait InputSubtype extends Product with Serializable

    object InputSubtype {

      final case class Convert[InSubtype <: In, OutSubtype <: Out](
        inSubtype:  Type[InSubtype],
        outSubtype: Type[OutSubtype],
        pipe:       CodeOf[Pipe[InSubtype, OutSubtype]]
      ) extends InputSubtype

      final case class Handle[InSubtype <: In](
        inSubtype: Type[InSubtype],
        pipe:      CodeOf[Pipe[InSubtype, Out]]
      ) extends InputSubtype
    }

    final case class Subtypes(subtypes: ListMap[String, InputSubtype]) extends EnumGeneratorData

    // TODO: Values for enum-values -> enum-values handling
  }

  object SumTypeConversion extends CodeGeneratorExtractor {

    final def unapply(settings: Settings): Option[DerivationResult[CodeOf[Pipe[In, Out]]]] =
      if (isUsableAsSumTypeConversion) Some(DerivationResult.fail(DerivationError.NotYetImplemented("Sum Types")))
      else None
  }

  def extractEnumInData(settings: Settings): DerivationResult[EnumData[In]]

  def extractEnumOutData(settings: Settings): DerivationResult[EnumData[Out]]

  def generateEnumCode(generatorData: EnumGeneratorData): DerivationResult[CodeOf[Pipe[In, Out]]]

  private def attemptEnumRendering(settings: Settings): DerivationResult[CodeOf[Pipe[In, Out]]] =
    for {
      data <- extractEnumInData(settings) zip extractEnumOutData(settings)
      (inData, outData) = data
      generatorData <- matchEnums(inData, outData, settings)
      code <- generateEnumCode(generatorData)
    } yield code

  private def matchEnums(
    inData:   EnumData[In],
    outData:  EnumData[Out],
    settings: Settings
  ): DerivationResult[EnumGeneratorData] = (inData, outData) match {
    case (EnumData.SumType(inSubtypes), EnumData.SumType(outSubtype)) =>
      inSubtypes
        .map[DerivationResult[(String, EnumGeneratorData.InputSubtype)]] {
          case EnumData.SumType.Case(inSubtypeName, inSubtypeType, isCaseObject) =>
            // TODO: resolve and match types by names
            DerivationResult.fail(DerivationError.NotYetImplemented("SumType matching"))
        }
        .pipe(DerivationResult.sequence(_))
        .map(_.to(ListMap))
        .map(EnumGeneratorData.Subtypes(_))
    case (EnumData.Enumeration(inValues), EnumData.Enumeration(outValues)) =>
      inValues
        .map[DerivationResult[(String, EnumGeneratorData.InputSubtype)]] {
          case EnumData.Enumeration.Value(name, path) =>
            // TODO: resolve and match values by names
            DerivationResult.fail(DerivationError.NotYetImplemented("SumType matching"))
        }
        .pipe(DerivationResult.sequence(_))
        .map(_.to(ListMap))
        .map(EnumGeneratorData.Subtypes(_))
    case (EnumData.SumType(_), EnumData.Enumeration(_)) =>
      // TODO: case when all sum types are case objects
      DerivationResult.fail(DerivationError.NotSupportedEnumConversion(isInSumType = true, isOutSumType = false))
    case (EnumData.Enumeration(_), EnumData.SumType(_)) =>
      // TODO: case when all sum types are case objects
      DerivationResult.fail(DerivationError.NotSupportedEnumConversion(isInSumType = false, isOutSumType = true))
  }
}
