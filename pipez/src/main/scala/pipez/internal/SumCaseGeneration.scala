package pipez.internal

import pipez.internal.SumCaseGeneration.inputNameMatchesOutputName

import scala.annotation.nowarn
import scala.collection.immutable.ListMap
import scala.util.chaining.scalaUtilChainingOps

@nowarn("msg=The outer reference in this type test cannot be checked at run time.")
trait SumCaseGeneration[Pipe[_, _], In, Out] { self: Definitions[Pipe, In, Out] & Generators[Pipe, In, Out] =>

  /** True iff `A` is `sealed` */
  def isADT[A: Type]: Boolean

  /** True if `A extends java.lang.Enum[A]` */
  def isJavaEnum[A: Type]: Boolean

  /** Check is `A =:= B` in a platform-independent code */
  def areSubtypesEqual[A: Type, B: Type]: Boolean

  final def isSumType[A: Type]: Boolean =
    isADT[A] || isJavaEnum[A]

  /** Whether both `In` and `Out` are ADTs/Java Enums */
  final def isUsableAsSumTypeConversion: Boolean =
    isSumType[In] && isSumType[Out]

  /** Stores information from what pieces `Out` is made */
  sealed trait EnumData[A] extends Product with Serializable
  object EnumData {

    final case class SumType[A](elements: List[SumType.Case[? <: A]]) extends EnumData[A] {

      def findSubtype(subtypeName: String, caseInsensitiveSearch: Boolean): DerivationResult[SumType.Case[? <: A]] =
        DerivationResult.fromOption(
          elements.collectFirst {
            case sumType @ SumType.Case(name, _, _)
                if inputNameMatchesOutputName(name, subtypeName, caseInsensitiveSearch) =>
              sumType
          }
        )(DerivationError.MissingMatchingSubType(subtypeName))
    }

    object SumType {

      final case class Case[A](name: String, tpe: Type[A], isCaseObject: Boolean) {
        override def toString: String = s"Case($name : ${previewType(tpe)})"
      }
    }

    final case class Enumeration[A](values: List[Enumeration.Value[A]]) extends EnumData[A] {

      def findValue(valueName: String, caseInsensitiveSearch: Boolean): DerivationResult[Enumeration.Value[A]] =
        DerivationResult.fromOption(
          values.collectFirst {
            case value @ EnumData.Enumeration.Value(name, _)
                if inputNameMatchesOutputName(valueName, name, caseInsensitiveSearch) =>
              value
          }
        )(DerivationError.MissingMatchingValue(valueName))
    }

    object Enumeration {

      final case class Value[A](name: String, path: CodeOf[A]) {
        override def toString: String = s"Value($name = ${previewCode(path)})"
      }
    }
  }

  /** Translation strategy for a particular input subtype */
  sealed trait InSubtypeLogic[InSubtype <: In] extends Product with Serializable
  object InSubtypeLogic {

    final case class DefaultSubtype[InSubtype <: In]() extends InSubtypeLogic[InSubtype]

    final case class SubtypeRemoved[InSubtype <: In](
      pipe: CodeOf[Pipe[InSubtype, Out]]
    ) extends InSubtypeLogic[InSubtype] {
      override def toString: String = s"SubtypeRemoved(${previewCode(pipe)})"
    }

    final case class SubtypeRenamed[InSubtype <: In, OutSubtype <: Out](
      outSubtype: Type[OutSubtype]
    ) extends InSubtypeLogic[InSubtype] {
      override def toString: String = s"SubtypeRenamed(${previewType(outSubtype)})"
    }

    final case class PipeProvided[InSubtype <: In, OutSubtype <: Out](
      outSubtype: Type[OutSubtype],
      pipe:       CodeOf[Pipe[InSubtype, OutSubtype]]
    ) extends InSubtypeLogic[InSubtype] {
      override def toString: String = s"PipeProvided(${previewType(outSubtype)}, ${previewCode(pipe)})"
    }

    def resolve[InSubtype <: In: Type](settings: Settings): InSubtypeLogic[InSubtype] = {
      import ConfigEntry.*

      settings.resolve[InSubtypeLogic[InSubtype]](DefaultSubtype()) {
        case RemoveSubtype(_, tpe, pipe) if areSubtypesEqual(tpe, typeOf[InSubtype]) =>
          SubtypeRemoved(pipe.asInstanceOf[CodeOf[Pipe[InSubtype, Out]]])
        case RenameSubtype(_, tpe, _, outSubtypeType) if areSubtypesEqual(tpe, typeOf[InSubtype]) =>
          SubtypeRenamed(outSubtypeType)
        case PlugInSubtype(_, tpe, _, outSubtypeType, pipe) if areSubtypesEqual(tpe, typeOf[InSubtype]) =>
          PipeProvided[InSubtype, Out](outSubtypeType.asInstanceOf[Type[Out]],
                                       pipe.asInstanceOf[CodeOf[Pipe[InSubtype, Out]]]
          )
      }
    }

    type OutSubtype <: Out
    def resolveSubtype[InSubtype <: In: Type](
      settings:      Settings,
      outData:       EnumData.SumType[Out],
      inSubtypeName: String
    ): DerivationResult[EnumGeneratorData.InputSubtype] = resolve[InSubtype](settings) match {
      // OutSubtype - the same (simple) name as InSubtype
      // (in, ctx) => in match { i: InSubtype => unlift(summon[InSubtype, OutSubtype), in, ctx): Result[OutSubtype] }
      case DefaultSubtype() =>
        outData
          .findSubtype(inSubtypeName, settings.isEnumCaseInsensitive)
          .flatMap(outSubtype => fromOutputSubtype(typeOf[InSubtype], outSubtype.tpe))
          .log(s"Subtype ${previewType(typeOf[InSubtype])} uses default resolution (matching output name, summoning)")
      case SubtypeRemoved(pipe) =>
        // (in, ctx) => in match { i: InSubtype => unlift(pipe, in, ctx): Result[Out] }
        fromMissingPipe[InSubtype](pipe).log(
          s"Subtype ${previewType(typeOf[InSubtype])} considered removed from input, uses provided pipe"
        )
      case SubtypeRenamed(outSubtypeType) =>
        // OutSubtype - name provided
        // (in, ctx) => in match { i: InSubtype => unlift(summon[InSubtype, OutSubtype), in, ctx): Result[OutSubtype] }
        fromOutputSubtype(typeOf[InSubtype], outSubtypeType).log(
          s"Subtype ${previewType(typeOf[InSubtype])} considered renamed to $outSubtypeType, uses summoning"
        )
      case PipeProvided(outSubtypeType, pipe) =>
        // OutSubtype - name provided
        // (in, ctx) => in match { i: InSubtype => unlift(pipe, in, ctx): Result[OutSubtype] }
        fromOutputPipe(pipe)(typeOf[InSubtype], outSubtypeType).log(
          s"Subtype ${previewType(typeOf[InSubtype])} converted to $outSubtypeType using provided pipe"
        )
    }
  }

  /** Translation strategy for a particular input value */
  sealed trait InValueLogic extends Product with Serializable
  object InValueLogic {

    final case class DefaultValue() extends InValueLogic

    def resolve(settings: Settings): InValueLogic = {
      import ConfigEntry.*

      settings.resolve[InValueLogic](DefaultValue())(PartialFunction.empty)
    }

    def resolveValue(
      settings:    Settings,
      outData:     EnumData.Enumeration[Out],
      inValueName: String,
      inValueCode: CodeOf[In]
    ): DerivationResult[EnumGeneratorData.Pairing] = resolve(settings) match {
      case DefaultValue() =>
        outData.findValue(inValueName, settings.isEnumCaseInsensitive).map {
          case EnumData.Enumeration.Value(_, outCode) => EnumGeneratorData.Pairing(inValueCode, outCode)
        }
    }
  }

  /** Final platform-independent result of matching inputs with outputs using resolved strategies */
  sealed trait EnumGeneratorData extends Product with Serializable
  object EnumGeneratorData {

    sealed trait InputSubtype extends Product with Serializable
    object InputSubtype {

      final case class Convert[InSubtype <: In, OutSubtype <: Out](
        inSubtype:  Type[InSubtype],
        outSubtype: Type[OutSubtype],
        pipe:       CodeOf[Pipe[InSubtype, OutSubtype]]
      ) extends InputSubtype {
        override def toString: String =
          s"Convert(${previewType(inSubtype)}, ${previewType(outSubtype)}, ${previewCode(pipe)})"
      }

      final case class Handle[InSubtype <: In](
        inSubtype: Type[InSubtype],
        pipe:      CodeOf[Pipe[InSubtype, Out]]
      ) extends InputSubtype {
        override def toString: String = s"Handle(${previewType(inSubtype)}, ${previewCode(pipe)})"
      }
    }

    final case class Subtypes(subtypes: ListMap[String, InputSubtype]) extends EnumGeneratorData

    final case class Pairing(in: CodeOf[In], out: CodeOf[Out]) {
      override def toString: String = s"Pairing(${previewCode(in)}, ${previewCode(out)})"
    }

    final case class Values(values: ListMap[String, Pairing]) extends EnumGeneratorData
  }

  object SumTypeConversion {

    final def unapply(settings: Settings): Option[DerivationResult[CodeOf[Pipe[In, Out]]]] =
      if (isUsableAsSumTypeConversion) Some(attemptEnumRendering(settings)) else None
  }

  /** Platform-specific way of parsing `In` data
    *
    * Should:
    *   - obtain a lift of subtypes OR enumeration values
    *   - form it into `EnumData[In]`
    */
  def extractEnumInData: DerivationResult[EnumData[In]]

  /** Platform-specific way of parsing `Out` data
    *
    * Should:
    *   - obtain a lift of subtypes OR enumeration values
    *   - form it into `EnumData[In]`
    */
  def extractEnumOutData: DerivationResult[EnumData[Out]]

  /** Platform-specific way of generating code from resolved information
    *
    * TODO: update after implementing Paths: ctx -> updateContext(ctx, path)
    *
    * For subtype input should generate code like:
    *
    * {{{
    * pipeDerivation.lift { (in: In, ctx: pipeDerivation.Context) =>
    *   in match {
    *     case inSubtype: In.Foo => pipeDerivation.unlift(fooPipe, inSubtype, ctx)
    *     case inSubtype: In.Bar => pipeDerivation.unlift(barPipe, inSubtype, ctx)
    *   }
    * }
    * }}}
    */
  def generateEnumCode(generatorData: EnumGeneratorData): DerivationResult[CodeOf[Pipe[In, Out]]]

  private def attemptEnumRendering(settings: Settings): DerivationResult[CodeOf[Pipe[In, Out]]] =
    for {
      data <- extractEnumInData zip extractEnumOutData
      (inData, outData) = data
      generatorData <- matchEnums(inData, outData, settings)
      code <- generateEnumCode(generatorData)
    } yield code

  type InSubtype <: In
  // In the enum derivation, the logic in driven by In type:
  // - every input subtype/value should be handled
  // - so we are iterating over the list of possible values of In and check the configuration for them
  // - additional subtypes/values in Out can be safely ignored
  private def matchEnums(
    inData:   EnumData[In],
    outData:  EnumData[Out],
    settings: Settings
  ): DerivationResult[EnumGeneratorData] = (inData, outData) match {
    case (EnumData.SumType(inSubtypes), outData @ EnumData.SumType(_)) =>
      inSubtypes
        .map(_.asInstanceOf[EnumData.SumType.Case[InSubtype]])
        .map { case EnumData.SumType.Case(inSubtypeName, inSubtypeType, _) =>
          implicit val inSubtypeTpe: Type[InSubtype] = inSubtypeType
          InSubtypeLogic.resolveSubtype[InSubtype](settings, outData, inSubtypeName).map(inSubtypeName -> _)
        }
        .pipe(DerivationResult.sequence(_))
        .map(_.to(ListMap))
        .map(EnumGeneratorData.Subtypes(_))
    case (EnumData.Enumeration(inValues), outData @ EnumData.Enumeration(_)) =>
      inValues
        .map { case EnumData.Enumeration.Value(inValueName, inValueCode) =>
          InValueLogic.resolveValue(settings, outData, inValueName, inValueCode).map(inValueName -> _)
        }
        .pipe(DerivationResult.sequence(_))
        .map(_.to(ListMap))
        .map(EnumGeneratorData.Values(_))
    case (inSumType @ EnumData.SumType(_), EnumData.Enumeration(_)) =>
      attemptSubtypesAsEnumeration(inSumType) match {
        case Some(inEnumeration) => matchEnums(inEnumeration, outData, settings)
        case None =>
          DerivationResult.fail(DerivationError.NotSupportedEnumConversion(isInSumType = true, isOutSumType = false))
      }
    case (EnumData.Enumeration(_), outSubtype @ EnumData.SumType(_)) =>
      attemptSubtypesAsEnumeration(outSubtype) match {
        case Some(outEnumeration) => matchEnums(inData, outEnumeration, settings)
        case None =>
          DerivationResult.fail(DerivationError.NotSupportedEnumConversion(isInSumType = false, isOutSumType = true))
      }
  }

  private def attemptSubtypesAsEnumeration[Tpe](subtype: EnumData.SumType[Tpe]): Option[EnumData.Enumeration[Tpe]] =
    // TODO: try converting all enum types to values
    None

  // OutSubtype - name provided
  // (in, ctx) => in match { i: InSubtype => unlift(summon[InSubtype, OutSubtype), in, ctx): Result[OutSubtype] }
  private def fromOutputSubtype[InSubtype <: In: Type, OutSubtype <: Out: Type]: DerivationResult[
    EnumGeneratorData.InputSubtype
  ] = summonPipe[InSubtype, OutSubtype].map(
    EnumGeneratorData.InputSubtype.Convert(typeOf[InSubtype], typeOf[OutSubtype], _)
  )

  // OutSubtype - name provided
  // (in, ctx) => in match { i: InSubtype => unlift(pipe, in, ctx): Result[OutSubtype] }
  private def fromOutputPipe[InSubtype <: In: Type, OutSubtype <: Out: Type](
    pipe: CodeOf[Pipe[InSubtype, OutSubtype]]
  ): DerivationResult[EnumGeneratorData.InputSubtype] = DerivationResult.pure(
    EnumGeneratorData.InputSubtype.Convert(typeOf[InSubtype], typeOf[OutSubtype], pipe)
  )

  // (in, ctx) => in match { i: InSubtype => unlift(pipe, in, ctx): Result[Out] }
  private def fromMissingPipe[InSubtype <: In: Type](
    pipe: CodeOf[Pipe[InSubtype, Out]]
  ): DerivationResult[EnumGeneratorData.InputSubtype] =
    DerivationResult.pure(EnumGeneratorData.InputSubtype.Handle(typeOf[InSubtype], pipe))
}
object SumCaseGeneration {

  def inputNameMatchesOutputName(inSubtype: String, outSubtype: String, caseInsensitiveSearch: Boolean): Boolean =
    if (caseInsensitiveSearch) inSubtype equalsIgnoreCase outSubtype else inSubtype == outSubtype
}
