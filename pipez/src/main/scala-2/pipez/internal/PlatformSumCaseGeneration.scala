package pipez.internal

import pipez.internal.Definitions.{ Context, Result }

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
            EnumData.SumType.Case(
              subtypeType.name.toString,
              subtypeType.toType.asInstanceOf[Type[A]],
              isCaseObject = subtypeType.asClass.isModule,
              path = Path.Subtype(Path.Root, subtypeType.name.toString)
            )
          }
        )
      )(_ =>
        DerivationError.InvalidConfiguration(
          s"${previewType(typeOf[A])} seem like an ADT but cannot extract its subtypes"
        )
      )
    } else DerivationResult.fail(DerivationError.NotYetImplemented("Java Enum parsing"))

  final def generateEnumCode(generatorData: EnumGeneratorData): DerivationResult[CodeOf[Pipe[In, Out]]] =
    generatorData match {
      case EnumGeneratorData.Subtypes(subtypes) => generateSubtypes(subtypes.values.toList)
      case EnumGeneratorData.Values(values)     => generateEnumeration(values.values.toList)
    }

  private def generateSubtypes(subtypes: List[EnumGeneratorData.InputSubtype]) = {
    val in  = c.freshName(TermName("in"))
    val ctx = c.freshName(TermName("ctx"))

    def ctxE(path: Path) = updateContext(c.Expr[Context](q"$ctx"), pathCode(path))

    val cases = subtypes.map {
      case EnumGeneratorData.InputSubtype.Convert(inSubtype, _, pipe, path) =>
        val arg  = c.freshName(TermName("arg"))
        val code = unlift(pipe, c.Expr[In](q"$arg"), ctxE(path))
        cq"""$arg : ${inSubtype.typeSymbol} => $code.asInstanceOf[$pipeDerivation.Result[${Out.typeSymbol}]]"""
      case EnumGeneratorData.InputSubtype.Handle(inSubtype, pipe, path) =>
        val arg  = c.freshName(TermName("arg"))
        val code = unlift(pipe, c.Expr[In](q"$arg"), ctxE(path))
        cq"""$arg : ${inSubtype.typeSymbol} => $code"""
    }

    val body = lift[In, Out](
      c.Expr[(In, Context) => Result[Out]](
        q"""
        ($in : ${In.typeSymbol}, $ctx : $pipeDerivation.Context) => $in match { case ..$cases }
        """
      )
    )

    DerivationResult
      .pure(body)
      .log(s"Sum types derivation, subtypes: $subtypes")
      .logSuccess(code => s"Generated code: ${previewCode(code)}")
  }

  private def generateEnumeration(@unused values: List[EnumGeneratorData.Pairing]) =
    DerivationResult.fail(DerivationError.NotYetImplemented("Enumeration code emission"))
}
