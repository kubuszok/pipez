package pipez.internal

import scala.annotation.{ nowarn, unused }

@nowarn("msg=The outer reference in this type test cannot be checked at run time.")
trait PlatformSumCaseGeneration[Pipe[_, _], In, Out] extends SumCaseGeneration[Pipe, In, Out] {
  self: PlatformDefinitions[Pipe, In, Out] & PlatformGenerators[Pipe, In, Out] =>

  import c.universe.*

  final def isADT[A: Type]: Boolean = {
    val sym = typeOf[A].typeSymbol
    sym.isClass && sym.asClass.isSealed
  }
  final def isJavaEnum[A: Type]: Boolean =
    typeOf[A].typeSymbol.isJavaEnum

  final def areSubtypesEqual[A: Type, B: Type]: Boolean = typeOf[A] =:= typeOf[B]

  final def extractEnumInData: DerivationResult[EnumData[In]] = extractEnumData[In]

  final def extractEnumOutData: DerivationResult[EnumData[Out]] = extractEnumData[Out]

  private def extractEnumData[A: Type]: DerivationResult[EnumData[A]] =
    if (isADT[A]) {
      def extractSubclasses(t: TypeSymbol): List[TypeSymbol] =
        if (t.asClass.isSealed) t.asClass.knownDirectSubclasses.toList.map(_.asType).flatMap(extractSubclasses)
        else List(t)
      DerivationResult.unsafe[EnumData[A]](
        EnumData.SumType(
          extractSubclasses(typeOf[A].typeSymbol.asType).map { subtypeType =>
            EnumData.SumType.Case(subtypeType.name.toString,
                                  subtypeType.toType,
                                  isCaseObject = subtypeType.asClass.isModule
            )
          }
        )
      )(_ => DerivationError.InvalidConfiguration(s"$tpe seem like an ADT but cannot extract its subtypes"))
    } else DerivationResult.fail(DerivationError.NotYetImplemented("Java Enum parsing"))

  final def generateEnumCode(generatorData: EnumGeneratorData): DerivationResult[CodeOf[Pipe[In, Out]]] =
    generatorData match {
      case EnumGeneratorData.Subtypes(subtypes) => generateSubtypes(subtypes.values.toList)
      case EnumGeneratorData.Values(values)     => generateEnumeration(values.values.toList)
    }

  private def generateSubtypes(subtypes: List[EnumGeneratorData.InputSubtype]) = {
    val in:  Argument[In]               = c.freshName(TermName("in"))
    val ctx: Argument[ArbitraryContext] = c.freshName(TermName("ctx"))

    val cases = subtypes.map {
      case EnumGeneratorData.InputSubtype.Convert(inSubtype, _, pipe) =>
        val arg: Argument[In] = c.freshName(TermName("arg"))
        val code = unlift(pipe, inCode(arg), ctx)
        cq"""$arg : ${inSubtype.typeSymbol} => $code.asInstanceOf[$pipeDerivation.Result[${outType.typeSymbol}]]"""
      case EnumGeneratorData.InputSubtype.Handle(inSubtype, pipe) =>
        val arg: Argument[In] = c.freshName(TermName("arg"))
        val code = unlift(pipe, inCode(arg), ctx)
        cq"""$arg : ${inSubtype.typeSymbol} => $code"""
    }

    val body = c.Expr[ArbitraryResult[Out]](q"""$in match { case ..$cases }""")

    DerivationResult
      .pure(
        lift[In, Out](
          c.Expr[(In, ArbitraryContext) => ArbitraryResult[Out]](
            q"""
            ($in : ${inType.typeSymbol}, $ctx : $pipeDerivation.Context) => $body
            """
          )
        )
      )
      .log(s"Sum types derivation, subtypes: $subtypes")
      .logSuccess(code => s"Generated code: ${previewCode(code)}")
  }

  private def generateEnumeration(@unused values: List[EnumGeneratorData.Pairing]) =
    DerivationResult.fail(DerivationError.NotYetImplemented("Enumeration code emission"))
}
