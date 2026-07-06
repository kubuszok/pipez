package pipez.internal.compiletime.rules

import hearth.*
import hearth.fp.effect.*
import hearth.std.*
import pipez.internal.compiletime.PipezMacrosImpl

import scala.annotation.nowarn

@nowarn("msg=The outer reference in this type test cannot be checked at run time.")
trait PipezHandleAsEnumRuleImpl { this: PipezMacrosImpl & MacroCommons & StdExtensions =>

  object PipezHandleAsEnumRule extends PipezRule("handle as enum/sealed trait") {

    def apply[In: Type, Out: Type](in: Expr[In], ctx: Expr[Ctx])(implicit
        dctx: DerivationCtx[In, Out]
    ): MIO[Rule.Applicability[Expr[Res[Out]]]] =
      Log.info(s"Attempting enum derivation for ${Type[In].prettyPrint} => ${Type[Out].prettyPrint}") >> {
        (Enum.parse[In].toEither, Enum.parse[Out].toEither) match {
          case (Right(inEnum), Right(outEnum)) =>
            deriveEnum[In, Out](in, ctx, inEnum, outEnum).map(Rule.matched)
          case (Left(reason), _) =>
            MIO.pure(Rule.yielded(s"${Type[In].prettyPrint} is not an enum: $reason"))
          case (_, Left(reason)) =>
            MIO.pure(Rule.yielded(s"${Type[Out].prettyPrint} is not an enum: $reason"))
        }
      }

    /** Widen a subtype result `Res[Sub]` to the enum result `Res[Sup]`. This is NOT a provable upcast — for a GADT, a
      * case `C` need not be `<: Sup[T]` — but the produced runtime value really is a valid `Res[Sup]`, so emit an
      * unchecked in-tree cast. `R` is a method type parameter (not the abstract `Res`), so the cross-quote is legal.
      */
    private def widenResult[Sub: Type, Sup: Type](e: Expr[Res[Sub]]): Expr[Res[Sup]] =
      castUnchecked[Res[Sup]](e.asInstanceOf[Expr[Any]])(resType[Sup])

    private def castUnchecked[R: Type](e: Expr[Any]): Expr[R] =
      Expr.quote(Expr.splice(e).asInstanceOf[R])

    private def deriveEnum[In: Type, Out: Type](
        in: Expr[In],
        ctx: Expr[Ctx],
        inEnum: Enum[In],
        outEnum: Enum[Out]
    )(implicit dctx: DerivationCtx[In, Out]): MIO[Expr[Res[Out]]] = {
      implicit val resOut: Type[Res[Out]] = resType[Out]
      val outChildTypes: Map[String, ??] = outEnum.directChildren.iterator.map { case (name, child) =>
        import child.Underlying as ChildType
        name -> Type[ChildType].as_??
      }.toMap

      inEnum
        .matchOn[MIO, Res[Out]](in) { matched =>
          import matched.{value as matchedValue, Underlying as InCase}
          val inCaseName = Type[InCase].shortName
          val inCaseFullName = Type[InCase].prettyPrint.replaceAll("\\[[0-9;]*m", "").filter(_ >= 0x20.toChar)
          resolveSubtype[InCase, Out](inCaseName, inCaseFullName, outChildTypes, matchedValue, ctx)
        }
        .map(_.getOrElse(throw new RuntimeException(s"Enum ${Type[In].prettyPrint} has no children")))
    }

    private def resolveSubtype[InCase: Type, Out: Type](
        inCaseName: String,
        inCaseFullName: String,
        outChildren: Map[String, ??],
        matchedValue: Expr[InCase],
        ctx: Expr[Ctx]
    )(implicit dctx: DerivationCtx[?, ?]): MIO[Expr[Res[Out]]] = {
      val configRemove = dctx.settings.removedSubtypes.find(e => e.inSubtypeType.Underlying =:= Type[InCase])
      val configRename = dctx.settings.renamedSubtypes.find(e => e.inSubtypeType.Underlying =:= Type[InCase])
      val configPlugIn = dctx.settings.pluggedSubtypes.find(e => e.inSubtypeType.Underlying =:= Type[InCase])

      if (configRemove.isDefined) {
        // removeSubtype[InSubtype <: In](pipe: Pipe[InSubtype, Out]) — produces the full Out directly.
        MIO.pure(unliftConfigPipe[InCase, Out](configRemove.get.pipe, matchedValue, ctx))
      } else if (configPlugIn.isDefined) {
        val entry = configPlugIn.get
        import entry.outSubtypeType.Underlying as OutCase
        val updCtx = generateUpdateContext(ctx, pathSubtypeCode(inCaseFullName))
        MIO.pure(widenResult[OutCase, Out](unliftConfigPipe[InCase, OutCase](entry.pipe, matchedValue, updCtx)))
      } else {
        val lookupName = configRename.map(_.outSubtypeType.Underlying.shortName).getOrElse(inCaseName)
        val outChild = findOutChild(lookupName, outChildren)
        import outChild.Underlying as OutCase
        val updCtx = generateUpdateContext(ctx, pathSubtypeCode(inCaseFullName))

        if (Type[InCase] <:< Type[OutCase])
          MIO.pure(generatePureResult[Out](matchedValue.upcast[Out]))
        else
          SingletonValue.parse[InCase].toEither match {
            case Right(_) =>
              SingletonValue.parse[OutCase].toEither match {
                case Right(outSv) =>
                  MIO.pure(generatePureResult[Out](outSv.singletonExpr.upcast[Out]))
                case Left(_) =>
                  summonOrDeriveAndUnlift[InCase, OutCase, Out](matchedValue, updCtx)
              }
            case Left(_) =>
              summonOrDeriveAndUnlift[InCase, OutCase, Out](matchedValue, updCtx)
          }
      }
    }

    /** A config subtype pipe (`removeSubtype`/`plugInSubtype` argument) is `Pipe[InCase, OutCase]` by the DSL's
      * signature; recover that type from the captured `Expr_??` (a DSL-boundary assertion, not value erasure).
      */
    private def unliftConfigPipe[InCase: Type, OutCase: Type](
        pipeE: Expr_??,
        value: Expr[InCase],
        ctx: Expr[Ctx]
    ): Expr[Res[OutCase]] =
      generateUnlift[InCase, OutCase](pipeE.value.asInstanceOf[Expr[Pipe[InCase, OutCase]]], value, ctx)

    private def summonOrDeriveAndUnlift[InCase: Type, OutCase: Type, Out: Type](
        value: Expr[InCase],
        ctx: Expr[Ctx]
    )(implicit dctx: DerivationCtx[?, ?]): MIO[Expr[Res[Out]]] =
      summonPipe[InCase, OutCase] match {
        case Some(pipe) =>
          MIO.pure(widenResult[OutCase, Out](generateUnlift[InCase, OutCase](pipe, value, ctx)))
        case None =>
          // Enum subtypes always derive recursively — deriving an enum inherently requires deriving each subtype.
          val nestedCtx =
            DerivationCtx(Type[InCase], Type[OutCase], dctx.settings.stripForRecursion, dctx.cache, isTopLevel = false)
          deriveResultRecursively[InCase, OutCase](Type[InCase], Type[OutCase], nestedCtx).map { pipe =>
            widenResult[OutCase, Out](generateUnlift[InCase, OutCase](pipe, value, ctx))
          }
      }

    private def findOutChild(name: String, outChildren: Map[String, ??])(implicit
        dctx: DerivationCtx[?, ?]
    ): ?? =
      outChildren
        .collectFirst {
          case (childName, child)
              if (if (dctx.settings.isEnumCaseInsensitive) childName.equalsIgnoreCase(name)
                  else childName == name) =>
            child
        }
        .getOrElse(throw new RuntimeException(s"Couldn't find corresponding subtype for $name"))
  }
}
