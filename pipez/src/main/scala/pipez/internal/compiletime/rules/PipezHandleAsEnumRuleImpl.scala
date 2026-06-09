package pipez.internal.compiletime.rules

import hearth.*
import hearth.fp.effect.*
import hearth.std.*
import pipez.internal.compiletime.PipezMacrosImpl

import scala.annotation.nowarn

@nowarn("msg=The outer reference in this type test cannot be checked at run time.")
trait PipezHandleAsEnumRuleImpl { this: PipezMacrosImpl & MacroCommons & StdExtensions =>

  object PipezHandleAsEnumRule extends PipezRule("handle as enum/sealed trait") {

    def apply[In: Type, Out: Type](using ctx: DerivationCtx[In, Out]): MIO[Rule.Applicability[Expr[Pipe[In, Out]]]] =
      Log.info(s"Attempting enum derivation for ${Type[In].prettyPrint} => ${Type[Out].prettyPrint}") >> {
        (Enum.parse[In].toEither, Enum.parse[Out].toEither) match {
          case (Right(inEnum), Right(outEnum)) =>
            deriveEnum[In, Out](inEnum, outEnum).map(Rule.matched)
          case (Left(reason), _) =>
            MIO.pure(Rule.yielded(s"${Type[In].prettyPrint} is not an enum: $reason"))
          case (_, Left(reason)) =>
            MIO.pure(Rule.yielded(s"${Type[Out].prettyPrint} is not an enum: $reason"))
        }
      }

    private def deriveEnum[In: Type, Out: Type](
        inEnum: Enum[In],
        outEnum: Enum[Out]
    )(using ctx: DerivationCtx[In, Out]): MIO[Expr[Pipe[In, Out]]] = {
      val outChildTypes: Map[String, ??] = outEnum.directChildren.toList.map { case (name, child) =>
        import child.{Underlying as ChildType}
        name -> Type[ChildType].as_??
      }.toMap

      MIO.scoped { runSafe =>
        generateLift[In, Out] { (in, ctxExpr) =>
          runSafe {
            inEnum.matchOn[MIO, Any](in.asInstanceOf[Expr[In]]) { matched =>
              import matched.{value as matchedValue, Underlying as InCase}
              val inCaseName = Type[InCase].shortName
              val inCaseFullName = Type[InCase].prettyPrint.replaceAll("\\[[0-9;]*m", "")
              resolveSubtype(inCaseName, inCaseFullName, outChildTypes, matchedValue, ctxExpr)(using Type[InCase], ctx)
            }.map(_.getOrElse(throw new RuntimeException(s"Enum ${Type[In].prettyPrint} has no children")))
              .map(_.asInstanceOf[Expr[Any]])
          }
        }
      }
    }

    private def resolveSubtype[InCase: Type](
        inCaseName: String,
        inCaseFullName: String,
        outChildren: Map[String, ??],
        matchedValue: Expr[InCase],
        ctxExpr: Expr[Any]
    )(using ctx: DerivationCtx[?, ?]): MIO[Expr[Any]] = {
      val configRemove = ctx.settings.removedSubtypes.find(e => e.inSubtypeType.Underlying =:= Type[InCase])
      val configRename = ctx.settings.renamedSubtypes.find(e => e.inSubtypeType.Underlying =:= Type[InCase])
      val configPlugIn = ctx.settings.pluggedSubtypes.find(e => e.inSubtypeType.Underlying =:= Type[InCase])

      if (configRemove.isDefined) {
        MIO.pure(generateUnlift(configRemove.get.pipe, matchedValue.asInstanceOf[Expr[Any]], ctxExpr))
      } else if (configPlugIn.isDefined) {
        val entry = configPlugIn.get
        val path  = pathSubtypeCode(inCaseFullName)
        val updCtx = generateUpdateContext(ctxExpr, path)
        MIO.pure(generateUnlift(entry.pipe, matchedValue.asInstanceOf[Expr[Any]], updCtx))
      } else {
        val lookupName = configRename.map(_.outSubtypeType.Underlying.shortName).getOrElse(inCaseName)
        val outChild = findOutChild(lookupName, outChildren)
        import outChild.{Underlying as OutCase}
        val path   = pathSubtypeCode(inCaseFullName)
        val updCtx = generateUpdateContext(ctxExpr, path)

        if (Type[InCase] <:< Type[OutCase]) {
          MIO.pure(generatePureResult(matchedValue.asInstanceOf[Expr[Any]]))
        } else {
          SingletonValue.parse[InCase].toEither match {
            case Right(_) =>
              SingletonValue.parse[OutCase].toEither match {
                case Right(outSv) =>
                  MIO.pure(generatePureResult(outSv.singletonExpr.asInstanceOf[Expr[Any]]))
                case Left(_) =>
                  summonOrDeriveAndUnlift[InCase, OutCase](matchedValue, updCtx)
              }
            case Left(_) =>
              summonOrDeriveAndUnlift[InCase, OutCase](matchedValue, updCtx)
          }
        }
      }
    }

    private def summonOrDeriveAndUnlift[InCase: Type, OutCase: Type](
        value: Expr[InCase],
        ctx: Expr[Any]
    )(using dctx: DerivationCtx[?, ?]): MIO[Expr[Any]] = {
      // First try to summon an existing implicit
      summonPipe[InCase, OutCase] match {
        case Some(pipe) =>
          MIO.pure(generateUnlift(pipe.asInstanceOf[Expr[Any]], value.asInstanceOf[Expr[Any]], ctx))
        case None =>
          // Try recursive derivation for case class subtypes
          val newCtx = DerivationCtx[InCase, OutCase](
            inType = Type[InCase],
            outType = Type[OutCase],
            settings = dctx.settings.stripForRecursion,
            derivedPipeType = dctx.derivedPipeType
          )
          deriveResultRecursively[InCase, OutCase](using Type[InCase], Type[OutCase], newCtx).map { pipe =>
            generateUnlift(pipe.asInstanceOf[Expr[Any]], value.asInstanceOf[Expr[Any]], ctx)
          }
      }
    }

    private def findOutChild(name: String, outChildren: Map[String, ??])(using ctx: DerivationCtx[?, ?]): ?? =
      outChildren
        .collectFirst {
          case (childName, child)
              if (if (ctx.settings.isEnumCaseInsensitive) childName.equalsIgnoreCase(name)
                  else childName == name) =>
            child
        }
        .getOrElse(throw new RuntimeException(s"Couldn't find corresponding subtype for $name"))
  }
}
