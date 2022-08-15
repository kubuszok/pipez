package pipez.internal

import scala.quoted.{ Type as _, * }

trait PlatformSumCaseGeneration[Pipe[_, _], In, Out](using Quotes) extends SumCaseGeneration[Pipe, In, Out] {
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

  final def extractEnumInData: DerivationResult[EnumData[In]] =
    ???

  final def extractEnumOutData: DerivationResult[EnumData[Out]] =
    ???

  final def generateEnumCode(generatorData: EnumGeneratorData): DerivationResult[CodeOf[Pipe[In, Out]]] =
    ???
}
