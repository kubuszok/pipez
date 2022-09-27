package pipez.internal

import pipez.internal.Definitions.{ Context, Result }

import scala.annotation.unused
import scala.util.chaining.*
import scala.quoted.{ Type as _, * }

private[internal] trait PlatformSumCaseGeneration[Pipe[_, _], In, Out] extends SumCaseGeneration[Pipe, In, Out] {
  self: PlatformDefinitions[Pipe, In, Out] & PlatformGenerators[Pipe, In, Out] =>

  import quotes.*
  import quotes.reflect.*

  final def isADT[A: Type]: Boolean =
    val sym = TypeRepr.of[A].typeSymbol
    sym.isClassDef && sym.flags.is(Flags.Sealed)

  final def areSubtypesEqual[A: Type, B: Type]: Boolean = TypeRepr.of[A] =:= TypeRepr.of[B]

  final def extractEnumInData: DerivationResult[EnumData[In]] = extractEnumData[In]

  final def extractEnumOutData: DerivationResult[EnumData[Out]] = extractEnumData[Out]

  private def extractEnumData[A: Type]: DerivationResult[EnumData[A]] =
    def extractSubclasses(sym: Symbol): List[Symbol] =
      if (sym.flags.is(Flags.Sealed)) sym.children.flatMap(extractSubclasses)
      else if (sym.flags.is(Flags.Enum)) List(sym.typeRef.typeSymbol)
      else if (sym.flags.is(Flags.Module)) List(sym.typeRef.typeSymbol.moduleClass)
      else List(sym)

    DerivationResult.unsafe[EnumData[A]](
      EnumData(
        extractSubclasses(TypeRepr.of[A].typeSymbol)
          .map { subtypeType =>
            subtypeType.primaryConstructor.paramSymss match {
              // subtype takes type parameters
              case typeParamSymbols :: _ if typeParamSymbols.exists(_.isType) =>
                // we have to figure how subtypes type params map to parent type params
                val appliedTypeByParam: Map[String, TypeRepr] =
                  subtypeType.typeRef
                    .baseType(TypeRepr.of[A].typeSymbol)
                    .typeArgs
                    .map(_.typeSymbol.name)
                    .zip(TypeRepr.of[A].typeArgs)
                    .toMap
                // TODO: some better error message if child has an extra type param that doesn't come from the parent
                val typeParamReprs: List[TypeRepr] = typeParamSymbols.map(_.name).map(appliedTypeByParam)
                subtypeType -> subtypeType.typeRef.appliedTo(typeParamReprs).asType.asInstanceOf[Type[A]]
              // subtype is monomorphic
              case _ =>
                subtypeType -> subtypeType.typeRef.asType.asInstanceOf[Type[A]]
            }
          }
          .filter { case (_, subtypeTypeParamsFixed) =>
            // with GADT we can have subtypes that shouldn't appear in pattern matching
            TypeRepr.of(using subtypeTypeParamsFixed) <:< TypeRepr.of[A]
          }
          .map { case (subtypeType, subtypeTypeParamsFixed) =>
            EnumData.Case(
              name = subtypeType.name,
              tpe = subtypeTypeParamsFixed,
              isCaseObject = subtypeType.flags.is(Flags.Module),
              path = Path.Subtype(Path.Root, subtypeType.name)
            )
          }
      )
    )(err =>
      DerivationError.NotYetImplemented(
        s"${previewType(typeOf[A])} seem like an ADT but cannot extract its subtypes: ${err.getMessage}"
      )
    )

  final def generateEnumCode(generatorData: EnumGeneratorData): DerivationResult[Expr[Pipe[In, Out]]] =
    generateSubtypes(generatorData.subtypes.values.toList)

  private def generateSubtypes(subtypes: List[EnumGeneratorData.InputSubtype]) = {
    def cases(in: Expr[In], ctx: Expr[Context]) = subtypes
      .map {
        case convert @ EnumGeneratorData.InputSubtype.Convert(inSubtype, _, _, _) => convert -> inSubtype
        case handle @ EnumGeneratorData.InputSubtype.Handle(inSubtype, _, _)      => handle -> inSubtype
      }
      .map { case (generator, inSubtype) =>
        implicit val In: Type[In] = inSubtype.asInstanceOf[Type[In]]
        val arg  = Symbol.newBind(Symbol.spliceOwner, "arg", Flags.EmptyFlags, TypeRepr.of[In])
        val argE = Ident(arg.termRef).asExpr.asInstanceOf[Expr[In]]
        val body = '{ ${ generator.unlifted(argE, ctx) }.asInstanceOf[Result[Out]] }

        // Scala 3's enums' parameterless cases are vals with type erased, so w have to match them by value
        if (TypeRepr.of[In].typeSymbol.flags.is(Flags.Enum | Flags.JavaStatic))
          // case arg @ Enum.Value => ...
          CaseDef(Bind(arg, Ident(TypeRepr.of[In].typeSymbol.termRef)), None, body.asTerm)
        else
          // case arg : Enum.Value => ...
          CaseDef(Bind(arg, Typed(Wildcard(), TypeTree.of[In])), None, body.asTerm)
      }
      .pipe(Match(in.asTerm, _))
      .asExpr
      .asInstanceOf[Expr[Result[Out]]]

    val body = lift[In, Out](
      '{ (in: In, ctx: Context) => ${ cases('{ in }, '{ ctx }) } }
    )

    DerivationResult
      .pure(body)
      .log(s"Sum types derivation, subtypes: $subtypes")
      .logSuccess(code => s"Generated code: ${previewCode(code)}")
  }
}
