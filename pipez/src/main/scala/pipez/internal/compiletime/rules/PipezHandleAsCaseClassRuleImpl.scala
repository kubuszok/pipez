package pipez.internal.compiletime.rules

import hearth.*
import hearth.fp.effect.*
import hearth.std.*
import pipez.internal.compiletime.PipezMacrosImpl

import scala.annotation.nowarn
import scala.collection.immutable.ListMap

@nowarn("msg=The outer reference in this type test cannot be checked at run time.")
trait PipezHandleAsCaseClassRuleImpl { this: PipezMacrosImpl & MacroCommons & StdExtensions =>

  object PipezHandleAsCaseClassRule extends PipezRule("handle as case class") {

    def apply[In: Type, Out: Type](using ctx: DerivationCtx[In, Out]): MIO[Rule.Applicability[Expr[Pipe[In, Out]]]] =
      Log.info(s"Attempting product derivation for ${Type[In].prettyPrint} => ${Type[Out].prettyPrint}") >> {
        CaseClass.parse[Out].toEither match {
          case Right(outClass) =>
            deriveProduct[In, Out](outClass).map(Rule.matched)
          case Left(reason) =>
            MIO.pure(Rule.yielded(s"${Type[Out].prettyPrint} is not a case class: $reason"))
        }
      }

    // ---- Field resolution result ----

    sealed trait FieldResult
    object FieldResult {
      final case class Pure(name: String, tpe: ??, get: (Expr[Any], Expr[Any]) => Expr[Any]) extends FieldResult
      final case class Effectful(name: String, tpe: ??, get: (Expr[Any], Expr[Any]) => Expr[Any]) extends FieldResult
    }

    // ---- Product derivation entry ----

    private def deriveProduct[In: Type, Out: Type](outClass: CaseClass[Out])(using
        ctx: DerivationCtx[In, Out]
    ): MIO[Expr[Pipe[In, Out]]] = {
      val outParams: List[(String, Parameter)] = outClass.primaryConstructor.totalParameters.flatten

      if (outParams.isEmpty) {
        MIO.pure(deriveZeroFieldProduct[In, Out](outClass))
      } else {
        MIO {
          val fieldResults = outParams.map { case (outName, outParam) =>
            val outFieldType = outParam.tpe
            resolveField[In, Out](outName, outFieldType, outClass)
          }
          buildProduct[In, Out](fieldResults, outClass)
        }
      }
    }

    private def callPrimaryConstructor[A: Type](
        outClass: CaseClass[A],
        args: Map[String, Expr_??]
    ): Either[String, Expr[A]] =
      outClass.primaryConstructor.fold(
        onInstance = _ => throw new RuntimeException("Constructor should not need instance"),
        onTypes = _ => Map.empty,
        onValues = _ => args
      ).map { eexpr =>
        import eexpr.{Underlying as R, value as expr}
        expr.asInstanceOf[Expr[A]]
      }

    private def deriveZeroFieldProduct[In: Type, Out: Type](outClass: CaseClass[Out]): Expr[Pipe[In, Out]] =
      generateLift[In, Out] { (_, _) =>
        callPrimaryConstructor(outClass, Map.empty) match {
          case Right(outExpr) => generatePureResult(outExpr.asInstanceOf[Expr[Any]])
          case Left(err)      => throw new RuntimeException(s"Cannot construct ${Type[Out].prettyPrint}: $err")
        }
      }

    // ---- Field resolution ----

    private def resolveField[In: Type, Out: Type](
        outName: String,
        outFieldType: ??,
        outClass: CaseClass[Out]
    )(using ctx: DerivationCtx[In, Out]): FieldResult = {
      import outFieldType.{Underlying as OutField}

      val configAdd    = ctx.settings.addedFields.find(e => matchFieldName(e.outFieldName, outName))
      val configRename = ctx.settings.renamedFields.find(e => matchFieldName(e.outFieldName, outName))
      val configPlugIn = ctx.settings.pluggedFields.find(e => matchFieldName(e.outFieldName, outName))

      if (configAdd.isDefined) {
        val pipe = configAdd.get.pipe
        FieldResult.Effectful(
          outName,
          outFieldType,
          (in, ctx) => generateUnlift(pipe, in, ctx)
        )
      } else if (configPlugIn.isDefined) {
        val entry  = configPlugIn.get
        val inName = entry.inFieldName
        FieldResult.Effectful(
          outName,
          outFieldType,
          (in, ctx) => {
            val fieldExpr = extractFieldFromIn[In](in.asInstanceOf[Expr[In]], inName)
            val updCtx    = generateUpdateContext(ctx, pathFieldCode(outName))
            generateUnlift(entry.pipe, fieldExpr, updCtx)
          }
        )
      } else {
        val lookupName = configRename.map(_.inFieldName).getOrElse(outName)
        resolveByNameOrFallback[In, Out, OutField](lookupName, outName, outFieldType, outClass)
      }
    }

    private def resolveByNameOrFallback[In: Type, Out: Type, OutField: Type](
        inFieldName: String,
        outFieldName: String,
        outFieldType: ??,
        outClass: CaseClass[Out]
    )(using ctx: DerivationCtx[In, Out]): FieldResult = {
      val inFieldOpt = findInField[In](inFieldName)

      inFieldOpt match {
        case Some((inFieldTpe, getField)) =>
          import inFieldTpe.{Underlying as InField}
          if (Type[InField] <:< Type[OutField]) {
            FieldResult.Pure(outFieldName, outFieldType, (in, _) => getField(in.asInstanceOf[Expr[In]]))
          } else {
            summonPipe[InField, OutField] match {
              case Some(pipe) =>
                FieldResult.Effectful(
                  outFieldName,
                  outFieldType,
                  (in, ctx) => {
                    val fieldExpr = getField(in.asInstanceOf[Expr[In]])
                    val updCtx    = generateUpdateContext(ctx, pathFieldCode(outFieldName))
                    generateUnlift(pipe.asInstanceOf[Expr[Any]], fieldExpr, updCtx)
                  }
                )
              case None =>
                throw new RuntimeException(
                  s"Couldn't find implicit Pipe[${Type[InField].prettyPrint}, ${Type[OutField].prettyPrint}], " +
                    s"required by ${Type[In].prettyPrint}.$inFieldName to ${Type[Out].prettyPrint}.$outFieldName conversion"
                )
            }
          }

        case None =>
          resolveFallback[In, Out](outFieldName, outClass).getOrElse(
            throw new RuntimeException(
              s"Couldn't find a field/method which could be used as a source for $outFieldName from ${Type[Out].prettyPrint}"
            )
          )
      }
    }

    private def matchFieldName(configName: String, paramName: String)(using ctx: DerivationCtx[?, ?]): Boolean =
      inputNameMatchesOutputName(configName, paramName, ctx.settings.isFieldCaseInsensitive)

    // ---- In field extraction ----

    private def findInField[In: Type](name: String)(using ctx: DerivationCtx[?, ?]): Option[(??, Expr[In] => Expr[Any])] =
      CaseClass.parse[In].toEither match {
        case Right(inClass) =>
          inClass.primaryConstructor.totalParameters.flatten.collectFirst {
            case (paramName, param)
                if inputNameMatchesOutputName(paramName, name, ctx.settings.isFieldCaseInsensitive) =>
              val paramType = param.tpe
              val getter: Expr[In] => Expr[Any] = in => {
                inClass.caseFieldValuesAt(in).toList.collectFirst {
                  case (n, fv) if n == paramName => fv.value.asInstanceOf[Expr[Any]]
                }.getOrElse(throw new RuntimeException(s"Field $paramName not found"))
              }
              (paramType, getter)
          }
        case Left(_) =>
          findMethodGetter[In](name)
      }

    private def findMethodGetter[In: Type](name: String)(using
        ctx: DerivationCtx[?, ?]
    ): Option[(??, Expr[In] => Expr[Any])] = {
      val garbage = Set(
        "copy", "canEqual", "productArity", "productElement", "productElementName",
        "productElementNames", "productIterator", "productPrefix", "equals", "hashCode",
        "toString", "clone", "synchronized", "wait", "notify", "notifyAll", "getClass",
        "asInstanceOf", "isInstanceOf"
      )

      Type[In].methods.iterator.flatMap { method =>
        if (
          !garbage(method.name) &&
          !method.name.contains("$default$") &&
          method.totalParameters.flatten.isEmpty &&
          inputNameMatchesOutputName(dropGetIs(method.name), name, ctx.settings.isFieldCaseInsensitive)
        ) {
          val returnType: ?? = method.knownReturning.getOrElse(Type.of[Any].as_??)
          val getter: Expr[In] => Expr[Any] = in => {
            method.fold(
              onInstance = oi => in.asInstanceOf[Expr[oi.Instance]].as_??(using oi.Instance),
              onTypes = _ => Map.empty,
              onValues = _ => Map.empty
            ) match {
              case Right(result) => result.value.asInstanceOf[Expr[Any]]
              case Left(err)     => throw new RuntimeException(s"Cannot call getter ${method.name}: $err")
            }
          }
          Some((returnType, getter))
        } else None
      }.nextOption()
    }

    private def extractFieldFromIn[In: Type](in: Expr[In], fieldName: String)(using
        ctx: DerivationCtx[?, ?]
    ): Expr[Any] =
      findInField[In](fieldName) match {
        case Some((_, getter)) => getter(in)
        case None              => throw new RuntimeException(s"Field $fieldName not found in ${Type[In].prettyPrint}")
      }

    // ---- Fallback resolution ----

    private def resolveFallback[In: Type, Out: Type](
        outFieldName: String,
        outClass: CaseClass[Out]
    )(using ctx: DerivationCtx[In, Out]): Option[FieldResult] = {
      val fromFallbackValues = ctx.settings.fallbackValues.view.flatMap { fv =>
        import fv.fallbackType.{Underlying as FV}
        CaseClass.parse[FV].toEither.toOption.flatMap { fvClass =>
          fvClass.primaryConstructor.totalParameters.flatten.collectFirst {
            case (n, _)
                if inputNameMatchesOutputName(n, outFieldName, ctx.settings.isFieldCaseInsensitive) =>
              FieldResult.Pure(
                outFieldName,
                fvClass.primaryConstructor.totalParameters.flatten.find(_._1 == n).get._2.tpe,
                (_, _) => {
                  val fvExpr = fv.fallbackValue.asInstanceOf[Expr[FV]]
                  fvClass.caseFieldValuesAt(fvExpr).toList.find(_._1 == n).get._2.value.asInstanceOf[Expr[Any]]
                }
              )
          }
        }
      }.headOption

      fromFallbackValues.orElse {
        if (ctx.settings.isFallbackToDefaultEnabled) {
          outClass.primaryConstructor.totalParameters.flatten.toMap.get(outFieldName).flatMap { param =>
            param.defaultValue.flatMap { defaultMethod =>
              defaultMethod.fold(
                onInstance = _ => throw new RuntimeException("Default value should not need instance"),
                onTypes = _ => Map.empty,
                onValues = _ => Map.empty
              ).toOption.map { defaultExprExistential =>
                FieldResult.Pure(
                  outFieldName,
                  param.tpe,
                  (_, _) => defaultExprExistential.value.asInstanceOf[Expr[Any]]
                )
              }
            }
          }
        } else None
      }
    }

    // ---- Code building ----

    private def buildProduct[In: Type, Out: Type](
        fieldResults: List[FieldResult],
        outClass: CaseClass[Out]
    ): Expr[Pipe[In, Out]] = {
      val allPure = fieldResults.forall(_.isInstanceOf[FieldResult.Pure])

      if (allPure) {
        generateLift[In, Out] { (in, _) =>
          @nowarn("msg=Infinite loop") implicit lazy val AnyType: Type[Any] = Type.of[Any]
          val fieldMap = fieldResults.map {
            case FieldResult.Pure(name, _, get) => name -> get(in, in).as_??
            case _                              => throw new RuntimeException("Unreachable")
          }.toMap
          callPrimaryConstructor(outClass, fieldMap) match {
            case Right(outExpr) => generatePureResult(outExpr.asInstanceOf[Expr[Any]])
            case Left(err)      => throw new RuntimeException(s"Cannot construct ${Type[Out].prettyPrint}: $err")
          }
        }
      } else {
        buildProductWithEffects[In, Out](fieldResults, outClass)
      }
    }

    private def buildProductWithEffects[In: Type, Out: Type](
        fieldResults: List[FieldResult],
        outClass: CaseClass[Out]
    ): Expr[Pipe[In, Out]] = {
      @nowarn("msg=Infinite loop") implicit lazy val AnyType: Type[Any] = Type.of[Any]
      val n = fieldResults.size

      generateLift[In, Out] { (in, ctx) =>
        val initResult: Expr[Any] = generatePureResult(
          Expr.quote { new Array[Any](Expr.splice(Expr(n))) }
        )

        val merged = fieldResults.zipWithIndex.foldLeft(initResult) { case (accum, (fieldResult, index)) =>
          val fieldValue = fieldResult match {
            case FieldResult.Pure(_, _, get)      => generatePureResult(get(in, ctx))
            case FieldResult.Effectful(_, _, get) => get(in, ctx)
          }
          val merger = Expr.quote { (arr: Array[Any], value: Any) =>
            arr(Expr.splice(Expr(index))) = value
            arr
          }
          generateMergeResults(ctx, accum, fieldValue, merger.asInstanceOf[Expr[Any]])
        }

        val constructor = Expr.quote { (arr: Array[Any]) =>
          Expr.splice {
            val outParams = outClass.primaryConstructor.totalParameters.flatten
            val fieldMap = outParams.zipWithIndex.map { case ((name, param), idx) =>
              val paramType = param.tpe
              import paramType.{Underlying as P}
              name -> Expr.quote(Expr.splice(Expr.quote(null: Array[Any]))(Expr.splice(Expr(idx))).asInstanceOf[P]).as_??
            }.toMap
            callPrimaryConstructor(outClass, fieldMap) match {
              case Right(outExpr) => outExpr
              case Left(err)      => throw new RuntimeException(s"Cannot construct ${Type[Out].prettyPrint}: $err")
            }
          }
        }

        generateMergeResults(
          ctx,
          merged,
          generatePureResult(Expr.quote(()).asInstanceOf[Expr[Any]]),
          Expr.quote { (arr: Array[Any], _: Unit) =>
            Expr.splice(constructor).apply(arr)
          }.asInstanceOf[Expr[Any]]
        )
      }
    }
  }
}
