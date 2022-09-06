package pipez.internal

import pipez.internal.Definitions.{ Context, Result }

import scala.annotation.unused
import scala.util.chaining.*
import scala.quoted.{ Type as _, * }

trait PlatformSumCaseGeneration[Pipe[_, _], In, Out] extends SumCaseGeneration[Pipe, In, Out] {
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
      else if (sym.flags.is(Flags.Module)) List(sym.companionModule)
      else List(sym)

    DerivationResult.unsafe[EnumData[A]](
      EnumData(
        extractSubclasses(TypeRepr.of[A].typeSymbol).map { subtypeType =>
          EnumData.Case(
            subtypeType.name,
            subtypeType.typeRef.asType.asInstanceOf[Type[A]],
            isCaseObject = subtypeType.flags.is(Flags.Module),
            path = Path.Subtype(Path.Root, subtypeType.name)
          )
        }
      )
    )(_ =>
      DerivationError.InvalidConfiguration(
        s"${previewType(typeOf[A])} seem like an ADT but cannot extract its subtypes"
      )
    )

  final def generateEnumCode(generatorData: EnumGeneratorData): DerivationResult[CodeOf[Pipe[In, Out]]] =
    generateSubtypes(generatorData.subtypes.values.toList)

  private def generateSubtypes(subtypes: List[EnumGeneratorData.InputSubtype]) = {
    def cases(in: CodeOf[In], ctx: CodeOf[Context]) = subtypes
      .map {
        case EnumGeneratorData.InputSubtype.Convert(inSubtype, _, pipe, path) =>
          implicit val In: Type[In] = inSubtype.asInstanceOf[Type[In]]
          val arg  = Symbol.newBind(Symbol.spliceOwner, "arg", Flags.EmptyFlags, TypeRepr.of[In])
          val argE = Ident(arg.termRef).asExpr.asInstanceOf[CodeOf[In]]
          val pipeFix: CodeOf[Pipe[In, Out]] = '{ ${ pipe.asInstanceOf }.asInstanceOf[Pipe[In, Out]] }
          val body = unlift(pipeFix, argE, updateContext(ctx, pathCode(path))).asInstanceOf[CodeOf[Result[Out]]]
          CaseDef(Bind(arg, Typed(Wildcard(), TypeTree.of[In])), None, body.asTerm)
        case EnumGeneratorData.InputSubtype.Handle(inSubtype, pipe, path) =>
          implicit val In: Type[In] = inSubtype.asInstanceOf[Type[In]]
          val arg  = Symbol.newBind(Symbol.spliceOwner, "arg", Flags.EmptyFlags, TypeRepr.of[In])
          val argE = Ident(arg.termRef).asExpr.asInstanceOf[CodeOf[In]]
          val pipeFix: CodeOf[Pipe[In, Out]] = pipe.asInstanceOf[CodeOf[Pipe[In, Out]]]
          val body = unlift(pipeFix, argE, updateContext(ctx, pathCode(path))).asInstanceOf[CodeOf[Result[Out]]]
          CaseDef(Bind(arg, Typed(Wildcard(), TypeTree.of[In])), None, body.asTerm)
      }
      .pipe(Match(in.asTerm, _))
      .asExpr
      .asInstanceOf[CodeOf[Result[Out]]]

    val body = lift[In, Out](
      '{ (in: In, ctx: Context) => ${ cases('{ in }, '{ ctx }) } }
    )

    DerivationResult
      .pure(body)
      .log(s"Sum types derivation, subtypes: $subtypes")
      .logSuccess(code => s"Generated code: ${previewCode(code)}")
  }
}
