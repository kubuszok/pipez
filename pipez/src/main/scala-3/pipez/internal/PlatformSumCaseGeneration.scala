package pipez.internal

import scala.quoted.{ Type as _, * }
import scala.annotation.unused

@scala.annotation.experimental // due to Quotes.reflect.Symbol.typeRef usage
trait PlatformSumCaseGeneration[Pipe[_, _], In, Out] extends SumCaseGeneration[Pipe, In, Out] {
  self: PlatformDefinitions[Pipe, In, Out] & PlatformGenerators[Pipe, In, Out] =>

  import quotes.*
  import quotes.reflect.*

  final def isADT[A: Type]: Boolean =
    val sym = TypeRepr.of[A].typeSymbol
    sym.isClassDef && sym.flags.is(Flags.Sealed)
  final def isJavaEnum[A: Type]: Boolean = scala.util
    .Try(Class.forName("java.lang.Enum"))
    .fold(_ => false, clazz => TypeRepr.of[A] <:< TypeRepr.typeConstructorOf(clazz).appliedTo(TypeRepr.of[A]))

  final def areSubtypesEqual[A: Type, B: Type]: Boolean = TypeRepr.of[A] =:= TypeRepr.of[B]

  final def extractEnumInData: DerivationResult[EnumData[In]] = extractEnumData[In]

  final def extractEnumOutData: DerivationResult[EnumData[Out]] = extractEnumData[Out]

  private def extractEnumData[A: Type]: DerivationResult[EnumData[A]] =
    if (isADT[A]) {
      def extractSubclasses(sym: Symbol): List[Symbol] =
        if (sym.flags.is(Flags.Sealed)) sym.children.flatMap(extractSubclasses)
        else List(sym)

      DerivationResult.unsafe[EnumData[A]](
        EnumData.SumType(
          extractSubclasses(TypeRepr.of[A].typeSymbol).map { subtypeType =>
            EnumData.SumType.Case(
              subtypeType.name,
              subtypeType.typeRef.qualifier.asType.asInstanceOf[Type[A]],
              isCaseObject = subtypeType.flags.is(Flags.Module)
            )
          }
        )
      )(_ => DerivationError.InvalidConfiguration(s"${typeOf[A]} seem like an ADT but cannot extract its subtypes"))
    } else DerivationResult.fail(DerivationError.NotYetImplemented("Java Enum parsing"))

  final def generateEnumCode(generatorData: EnumGeneratorData): DerivationResult[CodeOf[Pipe[In, Out]]] =
    generatorData match {
      case EnumGeneratorData.Subtypes(subtypes) => generateSubtypes(subtypes.values.toList)
      case EnumGeneratorData.Values(values)     => generateEnumeration(values.values.toList)
    }

  private def generateSubtypes(subtypes: List[EnumGeneratorData.InputSubtype]) =
    DerivationResult.fail(DerivationError.NotYetImplemented("Subtypes code emission"))

  private def generateEnumeration(@unused values: List[EnumGeneratorData.Pairing]) =
    DerivationResult.fail(DerivationError.NotYetImplemented("Enumeration code emission"))
}
