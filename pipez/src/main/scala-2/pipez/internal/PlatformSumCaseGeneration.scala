package pipez.internal

import pipez.internal.Definitions.{ Context, Result }

import scala.annotation.{ nowarn, unused }
import scala.util.chaining.*

@nowarn("msg=The outer reference in this type test cannot be checked at run time.")
private[internal] trait PlatformSumCaseGeneration[Pipe[_, _], In, Out] extends SumCaseGeneration[Pipe, In, Out] {
  self: PlatformDefinitions[Pipe, In, Out] & PlatformGenerators[Pipe, In, Out] =>

  import c.universe.*

  final def isADT[A: Type]: Boolean = {
    val sym = typeOf[A].typeSymbol
    sym.isClass && sym.asClass.isSealed
  }

  final def areSubtypesEqual[A: Type, B: Type]: Boolean = typeOf[A] =:= typeOf[B]

  final def extractEnumInData: DerivationResult[EnumData[In]] = extractEnumData[In]

  final def extractEnumOutData: DerivationResult[EnumData[Out]] = extractEnumData[Out]

  private def extractEnumData[A: Type]: DerivationResult[EnumData[A]] = {
    def extractSubclasses(t: TypeSymbol): List[TypeSymbol] =
      if (t.asClass.isSealed) t.asClass.knownDirectSubclasses.toList.map(_.asType).flatMap(extractSubclasses)
      else List(t)
    DerivationResult.unsafe[EnumData[A]](
      EnumData(
        extractSubclasses(typeOf[A].typeSymbol.asType)
          .map { subtypeType =>
            subtypeType -> subtypeTypeOf(typeOf[A], subtypeType.toType).asInstanceOf[Type[A]]
          }
          .filter { case (_, subtypeTypeParamsFixed) =>
            // with GADT we can have subtypes that shouldn't appear in pattern matching
            subtypeTypeParamsFixed <:< typeOf[A]
          }
          .map { case (subtypeType, subtypeTypeParamsFixed) =>
            EnumData.Case(
              name = subtypeType.name.toString,
              tpe = subtypeTypeParamsFixed,
              isCaseObject = subtypeType.asClass.isModule,
              path = Path.Subtype(Path.Root, subtypeType.fullName)
            )
          }
      )
    )(_ =>
      DerivationError.InvalidConfiguration(
        s"${previewType(typeOf[A])} seem like an ADT but cannot extract its subtypes"
      )
    )
  }

  final def generateEnumCode(generatorData: EnumGeneratorData): DerivationResult[Expr[Pipe[In, Out]]] =
    generateSubtypes(generatorData.subtypes.values.toList)

  private def generateSubtypes(subtypes: List[EnumGeneratorData.InputSubtype]) = {
    def cases(in: Expr[In], ctx: Expr[Context]) = subtypes
      .map {
        case convert @ EnumGeneratorData.InputSubtype.Convert(inSubtype, _, _, _) => convert -> inSubtype
        case handle @ EnumGeneratorData.InputSubtype.Handle(inSubtype, _, _)      => handle -> inSubtype
      }
      .map { case (generator, inSubtype) =>
        val arg = c.freshName(TermName("arg"))
        cq"""$arg : $inSubtype => ${generator.unlifted(c.Expr[In](q"$arg"), ctx)}.asInstanceOf[${Result[Out]}]"""
      }
      .pipe(c => q"$in match { case ..$c }")
      .pipe(c.Expr[Out](_))

    val body = {
      val in  = c.freshName(TermName("in"))
      val ctx = c.freshName(TermName("ctx"))
      lift[In, Out](
        c.Expr[(In, Context) => Result[Out]](
          q"""
          ($in : $In, $ctx : $Context) => ${cases(c.Expr[In](q"$in"), c.Expr[Context](q"$ctx"))}
          """
        )
      )
    }

    DerivationResult
      .pure(body)
      .log(s"Sum types derivation, subtypes: $subtypes")
      .logSuccess(code => s"Generated code: ${previewCode(code)}")
  }

  private def subtypeTypeOf(parentType: c.Type, subtypeType: c.Type): c.Type = {
    val sEta = subtypeType.etaExpand
    sEta.finalResultType
      .substituteTypes(sEta.baseType(parentType.typeSymbol).typeArgs.map(_.typeSymbol), parentType.typeArgs)
  }
}
