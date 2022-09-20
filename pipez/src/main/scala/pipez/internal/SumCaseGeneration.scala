package pipez.internal

import pipez.internal.Definitions.{ Context, Result }
import pipez.internal.SumCaseGeneration.inputNameMatchesOutputName

import scala.annotation.nowarn
import scala.collection.immutable.ListMap
import scala.util.chaining.scalaUtilChainingOps

@nowarn("msg=The outer reference in this type test cannot be checked at run time.")
trait SumCaseGeneration[Pipe[_, _], In, Out] { self: Definitions[Pipe, In, Out] & Generators[Pipe, In, Out] =>

  /** True iff `A` is `sealed` */
  def isADT[A: Type]: Boolean

  /** Check is `A =:= B` in a platform-independent code */
  def areSubtypesEqual[A: Type, B: Type]: Boolean

  final def isSumType[A: Type]: Boolean =
    isADT[A]

  /** Whether both `In` and `Out` are ADTs/Java Enums */
  final def isUsableAsSumTypeConversion: Boolean =
    isSumType[In] && isSumType[Out]

  /** Stores information from what pieces `Out` is made */
  final case class EnumData[A](elements: List[EnumData.Case[? <: A]]) {

    def findSubtype(subtypeName: String, caseInsensitiveSearch: Boolean): DerivationResult[EnumData.Case[? <: A]] =
      DerivationResult.fromOption(
        elements.collectFirst {
          case sumType @ EnumData.Case(name, _, _, _)
              if inputNameMatchesOutputName(name, subtypeName, caseInsensitiveSearch) =>
            sumType
        }
      )(DerivationError.MissingMatchingSubType(subtypeName))
  }
  object EnumData {

    final case class Case[A](name: String, tpe: Type[A], isCaseObject: Boolean, path: Path) {
      override def toString: String = s"Case($name : ${previewType(tpe)})"
    }
  }

  /** Translation strategy for a particular input subtype */
  sealed trait InSubtypeLogic[InSubtype <: In] extends Product with Serializable
  object InSubtypeLogic {

    final case class DefaultSubtype[InSubtype <: In]() extends InSubtypeLogic[InSubtype]

    final case class SubtypeRemoved[InSubtype <: In](
      pipe: Expr[Pipe[InSubtype, Out]]
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
      pipe:       Expr[Pipe[InSubtype, OutSubtype]]
    ) extends InSubtypeLogic[InSubtype] {
      override def toString: String = s"PipeProvided(${previewType(outSubtype)}, ${previewCode(pipe)})"
    }

    def resolve[InSubtype <: In: Type](settings: Settings): InSubtypeLogic[InSubtype] = {
      import ConfigEntry.*

      settings.resolve[InSubtypeLogic[InSubtype]](DefaultSubtype()) {
        case RemoveSubtype(_, tpe, pipe) if areSubtypesEqual(tpe, typeOf[InSubtype]) =>
          SubtypeRemoved(pipe.asInstanceOf[Expr[Pipe[InSubtype, Out]]])
        case RenameSubtype(_, tpe, _, outSubtypeType) if areSubtypesEqual(tpe, typeOf[InSubtype]) =>
          SubtypeRenamed(outSubtypeType)
        case PlugInSubtype(_, tpe, _, outSubtypeType, pipe) if areSubtypesEqual(tpe, typeOf[InSubtype]) =>
          PipeProvided[InSubtype, Out](outSubtypeType.asInstanceOf[Type[Out]],
                                       pipe.asInstanceOf[Expr[Pipe[InSubtype, Out]]]
          )
      }
    }

    type OutSubtype <: Out
    def resolveSubtype[InSubtype <: In: Type](
      settings:      Settings,
      outData:       EnumData[Out],
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

  /** Final platform-independent result of matching inputs with outputs using resolved strategies */
  final case class EnumGeneratorData(subtypes: ListMap[String, EnumGeneratorData.InputSubtype])
  object EnumGeneratorData {

    type InSubtype <: In
    type OutSubtype <: Out

    sealed trait InputSubtype extends Product with Serializable {

      def unlifted: (Expr[In], Expr[Context]) => Expr[Result[Out]] = this match {
        case convert @ InputSubtype.Convert(_, _, _, _) =>
          val InputSubtype.Convert(inSubtype, outSubtype, pipe, path) =
            convert.asInstanceOf[InputSubtype.Convert[InSubtype, OutSubtype]]
          (in: Expr[In], ctx: Expr[Context]) =>
            unlift[InSubtype, OutSubtype](pipe, in.asInstanceOf[Expr[InSubtype]], updateContext(ctx, pathCode(path)))(
              inSubtype,
              outSubtype
            ).asInstanceOf[Expr[Result[Out]]]
        case handle @ InputSubtype.Handle(_, _, _) =>
          val InputSubtype.Handle(inSubtype, pipe, path) = handle.asInstanceOf[InputSubtype.Handle[InSubtype]]
          (in: Expr[In], ctx: Expr[Context]) =>
            unlift[InSubtype, Out](pipe, in.asInstanceOf[Expr[InSubtype]], updateContext(ctx, pathCode(path)))(
              inSubtype,
              Out
            )
      }
    }
    object InputSubtype {

      final case class Convert[InSubtype <: In, OutSubtype <: Out](
        inSubtype:  Type[InSubtype],
        outSubtype: Type[OutSubtype],
        pipe:       Expr[Pipe[InSubtype, OutSubtype]],
        path:       Path
      ) extends InputSubtype {
        override def toString: String =
          s"Convert(${previewType(inSubtype)}, ${previewType(outSubtype)}, ${previewCode(pipe)})"
      }

      final case class Handle[InSubtype <: In](
        inSubtype: Type[InSubtype],
        pipe:      Expr[Pipe[InSubtype, Out]],
        path:      Path
      ) extends InputSubtype {
        override def toString: String = s"Handle(${previewType(inSubtype)}, ${previewCode(pipe)})"
      }
    }
  }

  object SumTypeConversion {

    final def unapply(settings: Settings): Option[DerivationResult[Expr[Pipe[In, Out]]]] =
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
    * For subtype input should generate code like:
    *
    * {{{
    * pipeDerivation.lift { (in: In, ctx: pipeDerivation.Context) =>
    *   in match {
    *     case inSubtype: In.Foo => pipeDerivation.unlift(fooPipe, inSubtype, updateContext(ctx, path))
    *     case inSubtype: In.Bar => pipeDerivation.unlift(barPipe, inSubtype, updateContext(ctx, path))
    *   }
    * }
    * }}}
    */
  def generateEnumCode(generatorData: EnumGeneratorData): DerivationResult[Expr[Pipe[In, Out]]]

  private def attemptEnumRendering(settings: Settings): DerivationResult[Expr[Pipe[In, Out]]] =
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
    case (EnumData(inSubtypes), outData) =>
      inSubtypes
        .map(_.asInstanceOf[EnumData.Case[InSubtype]])
        .map { case EnumData.Case(inSubtypeName, inSubtypeType, _, path) =>
          implicit val inSubtypeTpe: Type[InSubtype] = inSubtypeType
          InSubtypeLogic.resolveSubtype[InSubtype](settings, outData, inSubtypeName).map(inSubtypeName -> _)
        }
        .pipe(DerivationResult.sequence(_))
        .map(_.to(ListMap))
        .map(EnumGeneratorData(_))
  }

  // OutSubtype - name provided
  // (in, ctx) => in match { i: InSubtype => unlift(summon[InSubtype, OutSubtype), in, updateContext(ctx, path)): Result[OutSubtype] }
  private def fromOutputSubtype[InSubtype <: In: Type, OutSubtype <: Out: Type]: DerivationResult[
    EnumGeneratorData.InputSubtype
  ] = summonPipe[InSubtype, OutSubtype].map(
    EnumGeneratorData.InputSubtype.Convert(typeOf[InSubtype],
                                           typeOf[OutSubtype],
                                           _,
                                           Path.Subtype(Path.Root, previewType[InSubtype])
    )
  )

  // OutSubtype - name provided
  // (in, ctx) => in match { i: InSubtype => unlift(pipe, in, updateContext(ctx, path)): Result[OutSubtype] }
  private def fromOutputPipe[InSubtype <: In: Type, OutSubtype <: Out: Type](
    pipe: Expr[Pipe[InSubtype, OutSubtype]]
  ): DerivationResult[EnumGeneratorData.InputSubtype] = DerivationResult.pure(
    EnumGeneratorData.InputSubtype.Convert(typeOf[InSubtype],
                                           typeOf[OutSubtype],
                                           pipe,
                                           Path.Subtype(Path.Root, previewType[InSubtype])
    )
  )

  // (in, ctx) => in match { i: InSubtype => unlift(pipe, in, updateContext(ctx, path)): Result[Out] }
  private def fromMissingPipe[InSubtype <: In: Type](
    pipe: Expr[Pipe[InSubtype, Out]]
  ): DerivationResult[EnumGeneratorData.InputSubtype] =
    DerivationResult.pure(
      EnumGeneratorData.InputSubtype.Handle(typeOf[InSubtype], pipe, Path.Subtype(Path.Root, previewType[InSubtype]))
    )
}
object SumCaseGeneration {

  def inputNameMatchesOutputName(inSubtype: String, outSubtype: String, caseInsensitiveSearch: Boolean): Boolean =
    if (caseInsensitiveSearch) inSubtype equalsIgnoreCase outSubtype else inSubtype == outSubtype
}
