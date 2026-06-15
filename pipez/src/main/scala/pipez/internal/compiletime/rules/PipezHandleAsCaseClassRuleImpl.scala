package pipez.internal.compiletime.rules

import hearth.*
import hearth.fp.effect.*
import hearth.std.*
import pipez.internal.compiletime.PipezMacrosImpl

import scala.annotation.nowarn

@nowarn("msg=The outer reference in this type test cannot be checked at run time.")
trait PipezHandleAsCaseClassRuleImpl { this: PipezMacrosImpl & MacroCommons & StdExtensions =>

  object PipezHandleAsCaseClassRule extends PipezRule("handle as case class") {

    def apply[In: Type, Out: Type](in: Expr[In], ctx: Expr[Ctx])(implicit
        dctx: DerivationCtx[In, Out]
    ): MIO[Rule.Applicability[Expr[Res[Out]]]] =
      Log.info(s"Attempting product derivation for ${Type[In].prettyPrint} => ${Type[Out].prettyPrint}") >> {
        CaseClass.parse[Out].toEither match {
          case Right(outClass) =>
            deriveProduct[In, Out](in, ctx, outClass).map(Rule.matched)
          case Left(ccReason) =>
            detectBeanOut[Out] match {
              case Some(beanInfo) =>
                deriveBeanProduct[In, Out](in, ctx, beanInfo).map(Rule.matched)
              case None =>
                MIO.pure(Rule.yielded(s"${Type[Out].prettyPrint} is not a case class: $ccReason"))
            }
        }
      }

    // ---- Field resolution result ----
    //
    // Each field has its own output type `OutField`, recovered existentially when a `List[FieldResolution[In]]` is
    // consumed (pattern match `case p: FieldResolution.Pure[In, of] => ...`). `In` is fixed for the whole product.

    sealed trait FieldResolution[In] {
      val name: String
      val tpe: ??
    }
    object FieldResolution {
      final case class Pure[In, OutField](name: String, fieldType: Type[OutField], getValue: Expr[In] => Expr[OutField])
          extends FieldResolution[In] {
        val tpe: ?? = fieldType.as_??
      }
      final case class Effectful[In, OutField](
          name: String,
          fieldType: Type[OutField],
          getValue: (Expr[In], Expr[Ctx]) => Expr[Res[OutField]]
      ) extends FieldResolution[In] {
        val tpe: ?? = fieldType.as_??
      }
    }

    /** Existentially-typed field getter: `get(in): Expr[F]` for a field of type `F`. */
    sealed trait InFieldGetter[In] {
      type F
      implicit val tpe: Type[F]
      def get(in: Expr[In]): Expr[F]
    }
    private def inFieldGetter[In, FF: Type](g: Expr[In] => Expr[FF]): InFieldGetter[In] =
      new InFieldGetter[In] {
        type F = FF
        val tpe: Type[F] = Type[FF]
        def get(in: Expr[In]): Expr[F] = g(in)
      }

    // ---- Java Bean support ----

    final private case class BeanField(name: String, fieldType: ??, setter: Method)
    final private case class BeanInfo[A](fields: List[BeanField], defaultConstructor: Method)

    private val setterPattern = raw"set(.)(.*)".r

    private def extractSetterFieldName(name: String): Option[String] = name match {
      case setterPattern(head, tail) => Some(head.toLowerCase + tail)
      case _                         => None
    }

    private def detectBeanOut[A: Type]: Option[BeanInfo[A]] = {
      val defaultCtorOpt = Type[A].constructors.find(_.totalParameters.flatten.isEmpty)
      defaultCtorOpt.flatMap { defaultCtor =>
        val setterFields = Type[A].methods.flatMap { method =>
          extractSetterFieldName(method.name).flatMap { fieldName =>
            val params = method.totalParameters.flatten
            if (params.size == 1) {
              val (_, param) = params.head
              Some(BeanField(fieldName, param.tpe, method))
            } else None
          }
        }
        if (setterFields.nonEmpty) Some(BeanInfo[A](setterFields.toList, defaultCtor))
        else None
      }
    }

    // ---- Product derivation entry (case class) ----

    private def deriveProduct[In: Type, Out: Type](
        in: Expr[In],
        ctx: Expr[Ctx],
        outClass: CaseClass[Out]
    )(implicit dctx: DerivationCtx[In, Out]): MIO[Expr[Res[Out]]] = {
      val outParams: List[(String, Parameter)] = outClass.primaryConstructor.totalParameters.flatten
      if (outParams.isEmpty)
        MIO.pure(deriveZeroFieldProduct[In, Out](outClass))
      else {
        val inIsTuple = isTuple[In]
        val outIsTuple = isTuple[Out]
        outParams.zipWithIndex
          .foldLeft(MIO.pure(List.empty[FieldResolution[In]])) { case (accMIO, ((outName, outParam), idx)) =>
            accMIO.flatMap { acc =>
              val resolve =
                if (inIsTuple || outIsTuple) resolveFieldWithPositional[In, Out](outName, outParam.tpe, idx)
                else resolveField[In, Out](outName, outParam.tpe)
              resolve.map(fr => acc :+ fr)
            }
          }
          .map(fieldResults => buildProduct[In, Out](fieldResults, in, ctx, outClass))
      }
    }

    // ---- Bean product derivation entry ----

    private def deriveBeanProduct[In: Type, Out: Type](
        in: Expr[In],
        ctx: Expr[Ctx],
        beanInfo: BeanInfo[Out]
    )(implicit dctx: DerivationCtx[In, Out]): MIO[Expr[Res[Out]]] =
      if (beanInfo.fields.isEmpty)
        MIO.pure(deriveZeroBeanProduct[Out](beanInfo))
      else {
        val inIsTuple = isTuple[In]
        beanInfo.fields.zipWithIndex
          .foldLeft(MIO.pure(List.empty[FieldResolution[In]])) { case (accMIO, (bf, idx)) =>
            accMIO.flatMap { acc =>
              val resolve =
                if (inIsTuple) resolveFieldWithPositional[In, Out](bf.name, bf.fieldType, idx)
                else resolveField[In, Out](bf.name, bf.fieldType)
              resolve.map(fr => acc :+ fr)
            }
          }
          .map(fieldResults => buildBeanProduct[In, Out](fieldResults, in, ctx, beanInfo))
      }

    // ---- Constructor helpers ----

    private def callPrimaryConstructor[A: Type](
        outClass: CaseClass[A],
        args: Map[String, Expr_??]
    ): Either[String, Expr[A]] =
      outClass.primaryConstructor
        .fold(
          onInstance = _ => throw new RuntimeException("Constructor should not need instance"),
          onTypes = _ => Map.empty,
          onValues = _ => args
        )
        .map { eexpr =>
          import eexpr.{Underlying as R, value as expr}
          // The constructor fold yields the constructed value as an `Expr[R]` with `R =:= A`; bridge the Hearth-fold
          // existential to `Expr[A]` (a typed-construction boundary, not value erasure).
          expr.asInstanceOf[Expr[A]]
        }

    private def callDefaultConstructor[A: Type](defaultCtor: Method): Expr[A] =
      defaultCtor.fold(
        onInstance = _ => throw new RuntimeException("Default constructor should not need instance"),
        onTypes = _ => Map.empty,
        onValues = _ => Map.empty
      ) match {
        case Right(result) => result.value.asInstanceOf[Expr[A]]
        case Left(err)     =>
          throw new RuntimeException(s"Cannot call default constructor for ${Type[A].prettyPrint}: $err")
      }

    private def deriveZeroFieldProduct[In: Type, Out: Type](outClass: CaseClass[Out]): Expr[Res[Out]] =
      callPrimaryConstructor(outClass, Map.empty) match {
        case Right(outExpr) => generatePureResult[Out](outExpr)
        case Left(err)      => throw new RuntimeException(s"Cannot construct ${Type[Out].prettyPrint}: $err")
      }

    private def deriveZeroBeanProduct[Out: Type](beanInfo: BeanInfo[Out]): Expr[Res[Out]] =
      generatePureResult[Out](callDefaultConstructor[Out](beanInfo.defaultConstructor))

    // ---- Field resolution ----

    private def resolveFieldWithPositional[In: Type, Out: Type](
        outName: String,
        outFieldType: ??,
        positionIndex: Int
    )(implicit dctx: DerivationCtx[In, Out]): MIO[FieldResolution[In]] = {
      import outFieldType.Underlying as OutField

      val configAdd = dctx.settings.addedFields.find(e => matchFieldName(e.outFieldName, outName))
      val configRename = dctx.settings.renamedFields.find(e => matchFieldName(e.outFieldName, outName))
      val configPlugIn = dctx.settings.pluggedFields.find(e => matchFieldName(e.outFieldName, outName))

      if (configAdd.isDefined || configRename.isDefined || configPlugIn.isDefined)
        resolveField[In, Out](outName, outFieldType)
      else
        findInFieldByIndex[In](positionIndex) match {
          case Some(g) =>
            import g.tpe as inFieldT
            if (Type[g.F] <:< Type[OutField])
              MIO.pure(FieldResolution.Pure[In, OutField](outName, Type[OutField], in => g.get(in).upcast[OutField]))
            else
              summonOrDerive[g.F, OutField].map { pipe =>
                FieldResolution.Effectful[In, OutField](
                  outName,
                  Type[OutField],
                  (in, ctxE) => {
                    val updCtx = generateUpdateContext(ctxE, pathFieldCode(outName))
                    generateUnlift[g.F, OutField](pipe, g.get(in), updCtx)
                  }
                )
              }
          case None =>
            resolveField[In, Out](outName, outFieldType)
        }
    }

    private def resolveField[In: Type, Out: Type](
        outName: String,
        outFieldType: ??
    )(implicit dctx: DerivationCtx[In, Out]): MIO[FieldResolution[In]] = {
      import outFieldType.Underlying as OutField

      val configAdd = dctx.settings.addedFields.find(e => matchFieldName(e.outFieldName, outName))
      val configRename = dctx.settings.renamedFields.find(e => matchFieldName(e.outFieldName, outName))
      val configPlugIn = dctx.settings.pluggedFields.find(e => matchFieldName(e.outFieldName, outName))

      if (configAdd.isDefined) {
        val pipeE = configAdd.get.pipe
        MIO.pure(
          FieldResolution.Effectful[In, OutField](
            outName,
            Type[OutField],
            (in, ctxE) => unliftConfigPipe[In, OutField](pipeE, in, ctxE)
          )
        )
      } else if (configPlugIn.isDefined) {
        val entry = configPlugIn.get
        MIO.pure(
          FieldResolution.Effectful[In, OutField](
            outName,
            Type[OutField],
            (in, ctxE) => {
              val g = extractFieldFromIn[In](entry.inFieldName)
              import g.tpe
              val updCtx = generateUpdateContext(ctxE, pathFieldCode(outName))
              unliftConfigPipe[g.F, OutField](entry.pipe, g.get(in), updCtx)
            }
          )
        )
      } else {
        val lookupName = configRename.map(_.inFieldName).getOrElse(outName)
        resolveByNameOrFallback[In, Out, OutField](lookupName, outName)
      }
    }

    /** A config pipe (`addField`/`plugInField` argument) is `Pipe[InField, OutField]` by the DSL's signature; recover
      * that type from the `Expr_??` captured at the call site (a DSL-boundary assertion, not value erasure).
      */
    private def unliftConfigPipe[InField: Type, OutField: Type](
        pipeE: Expr_??,
        in: Expr[InField],
        ctx: Expr[Ctx]
    ): Expr[Res[OutField]] =
      generateUnlift[InField, OutField](pipeE.value.asInstanceOf[Expr[Pipe[InField, OutField]]], in, ctx)

    private def resolveByNameOrFallback[In: Type, Out: Type, OutField: Type](
        inFieldName: String,
        outFieldName: String
    )(implicit dctx: DerivationCtx[In, Out]): MIO[FieldResolution[In]] =
      findInField[In](inFieldName) match {
        case Some(g) =>
          import g.tpe as inFieldT
          if (Type[g.F] <:< Type[OutField])
            MIO.pure(
              FieldResolution.Pure[In, OutField](outFieldName, Type[OutField], in => g.get(in).upcast[OutField])
            )
          else
            summonOrDerive[g.F, OutField].map { pipe =>
              FieldResolution.Effectful[In, OutField](
                outFieldName,
                Type[OutField],
                (in, ctxE) => {
                  val updCtx = generateUpdateContext(ctxE, pathFieldCode(outFieldName))
                  generateUnlift[g.F, OutField](pipe, g.get(in), updCtx)
                }
              )
            }

        case None =>
          resolveFallback[In, Out, OutField](outFieldName) match {
            case Some(fr) => MIO.pure(fr)
            case None     =>
              MIO.fail(
                new RuntimeException(
                  s"Couldn't find a field/method which could be used as a source for $outFieldName from ${Type[Out].prettyPrint}"
                )
              )
          }
      }

    private def matchFieldName(configName: String, paramName: String)(implicit dctx: DerivationCtx[?, ?]): Boolean =
      inputNameMatchesOutputName(configName, paramName, dctx.settings.isFieldCaseInsensitive)

    // ---- In field extraction ----

    private def isTuple[A: Type]: Boolean =
      Type[A].shortName.startsWith("Tuple") && Type[A] <:< Type.of[Product]

    private def findInField[In: Type](
        name: String
    )(implicit dctx: DerivationCtx[?, ?]): Option[InFieldGetter[In]] =
      CaseClass.parse[In].toEither match {
        case Right(inClass) =>
          val inParams = inClass.primaryConstructor.totalParameters.flatten
          val fromCaseFields = inParams.collectFirst {
            case (paramName, param)
                if inputNameMatchesOutputName(paramName, name, dctx.settings.isFieldCaseInsensitive) =>
              import param.tpe.Underlying as F
              inFieldGetter[In, F] { in =>
                inClass
                  .caseFieldValuesAt(in)
                  .toList
                  .collectFirst { case (n, fv) if n == paramName => fv.value.asInstanceOf[Expr[F]] }
                  .getOrElse(throw new RuntimeException(s"Field $paramName not found"))
              }
          }
          fromCaseFields.orElse(findMethodGetter[In](name))
        case Left(_) =>
          findMethodGetter[In](name)
      }

    private def findInFieldByIndex[In: Type](
        index: Int
    )(implicit dctx: DerivationCtx[?, ?]): Option[InFieldGetter[In]] =
      CaseClass.parse[In].toEither match {
        case Right(inClass) =>
          val inParams = inClass.primaryConstructor.totalParameters.flatten
          if (index < inParams.size) {
            val (paramName, param) = inParams(index)
            import param.tpe.Underlying as F
            Some(inFieldGetter[In, F] { in =>
              inClass
                .caseFieldValuesAt(in)
                .toList
                .collectFirst { case (n, fv) if n == paramName => fv.value.asInstanceOf[Expr[F]] }
                .getOrElse(throw new RuntimeException(s"Field $paramName not found"))
            })
          } else None
        case Left(_) =>
          findMethodGetter[In](s"_${index + 1}")
      }

    private val ignoredMethodNames = Set(
      "copy",
      "canEqual",
      "productArity",
      "productElement",
      "productElementName",
      "productElementNames",
      "productIterator",
      "productPrefix",
      "equals",
      "hashCode",
      "toString",
      "clone",
      "synchronized",
      "wait",
      "notify",
      "notifyAll",
      "getClass",
      "asInstanceOf",
      "isInstanceOf"
    )

    private def findMethodGetter[In: Type](name: String)(implicit
        dctx: DerivationCtx[?, ?]
    ): Option[InFieldGetter[In]] =
      Type[In].methods.iterator
        .flatMap { method =>
          if (
            !ignoredMethodNames(method.name) &&
            !method.name.contains("$default$") &&
            method.totalParameters.flatten.isEmpty &&
            inputNameMatchesOutputName(dropGetIs(method.name), name, dctx.settings.isFieldCaseInsensitive)
          ) {
            def mkGetter[FF: Type]: InFieldGetter[In] = inFieldGetter[In, FF] { in =>
              method.fold(
                onInstance = oi => in.asInstanceOf[Expr[oi.Instance]].as_??(oi.Instance),
                onTypes = _ => Map.empty,
                onValues = _ => Map.empty
              ) match {
                case Right(result) => result.value.asInstanceOf[Expr[FF]]
                case Left(err)     => throw new RuntimeException(s"Cannot call getter ${method.name}: $err")
              }
            }
            method.knownReturning match {
              case Some(rt) =>
                import rt.Underlying as F
                Some(mkGetter[F])
              case None =>
                Some(mkGetter[Any](typeOfAny))
            }
          } else None
        }
        .nextOption()

    private def extractFieldFromIn[In: Type](fieldName: String)(implicit
        dctx: DerivationCtx[?, ?]
    ): InFieldGetter[In] =
      findInField[In](fieldName).getOrElse(
        throw new RuntimeException(s"Field $fieldName not found in ${Type[In].prettyPrint}")
      )

    // ---- Fallback resolution ----

    private def resolveFallback[In: Type, Out: Type, OutField: Type](
        outFieldName: String
    )(implicit dctx: DerivationCtx[In, Out]): Option[FieldResolution[In]] = {
      val fromFallbackValues = dctx.settings.fallbackValues.view.flatMap { fv =>
        import fv.fallbackType.Underlying as FV
        val fvExpr: Expr[FV] = fv.fallbackValue.value.asInstanceOf[Expr[FV]]
        CaseClass
          .parse[FV]
          .toEither
          .toOption
          .flatMap { fvClass =>
            fvClass.primaryConstructor.totalParameters.flatten.collectFirst {
              case (n, param) if inputNameMatchesOutputName(n, outFieldName, dctx.settings.isFieldCaseInsensitive) =>
                import param.tpe.Underlying as PF
                FieldResolution.Pure[In, PF](
                  outFieldName,
                  Type[PF],
                  _ => fvClass.caseFieldValuesAt(fvExpr).toList.find(_._1 == n).get._2.value.asInstanceOf[Expr[PF]]
                ): FieldResolution[In]
            }
          }
          .orElse {
            findMethodGetter[FV](outFieldName).map { g =>
              import g.tpe as gT
              FieldResolution.Pure[In, g.F](outFieldName, Type[g.F], _ => g.get(fvExpr)): FieldResolution[In]
            }
          }
      }.headOption

      fromFallbackValues.orElse {
        if (dctx.settings.isFallbackToDefaultEnabled) {
          CaseClass.parse[Out].toEither.toOption.flatMap { outClass =>
            outClass.primaryConstructor.totalParameters.flatten.toMap.get(outFieldName).flatMap { param =>
              param.defaultValue.flatMap { defaultMethod =>
                import param.tpe.Underlying as DF
                defaultMethod
                  .fold(
                    onInstance = _ => throw new RuntimeException("Default value should not need instance"),
                    onTypes = _ => Map.empty,
                    onValues = _ => Map.empty
                  )
                  .toOption
                  .map { defaultExprExistential =>
                    FieldResolution.Pure[In, DF](
                      outFieldName,
                      Type[DF],
                      _ => defaultExprExistential.value.asInstanceOf[Expr[DF]]
                    )
                  }
              }
            }
          }
        } else None
      }
    }

    // ---- Code building (case class) ----

    private def buildProduct[In: Type, Out: Type](
        fieldResults: List[FieldResolution[In]],
        in: Expr[In],
        ctx: Expr[Ctx],
        outClass: CaseClass[Out]
    ): Expr[Res[Out]] = {
      val allPure = fieldResults.forall(_.isInstanceOf[FieldResolution.Pure[?, ?]])
      if (allPure) {
        val fieldMap = fieldResults.map {
          case p: FieldResolution.Pure[In, of] =>
            implicit val ofT: Type[of] = p.fieldType
            p.name -> p.getValue(in).as_??
          case _ => throw new RuntimeException("Unreachable")
        }.toMap
        callPrimaryConstructor(outClass, fieldMap) match {
          case Right(outExpr) => generatePureResult[Out](outExpr)
          case Left(err)      => throw new RuntimeException(s"Cannot construct ${Type[Out].prettyPrint}: $err")
        }
      } else
        mergeFields[In, Out](fieldResults, in, ctx, generateArrayToConstructorFn[Out](outClass, -1, fieldResults.size))
    }

    private def buildBeanProduct[In: Type, Out: Type](
        fieldResults: List[FieldResolution[In]],
        in: Expr[In],
        ctx: Expr[Ctx],
        beanInfo: BeanInfo[Out]
    ): Expr[Res[Out]] = {
      val beanFieldsWithTypes = beanInfo.fields.map(bf => (bf.fieldType, bf.setter))
      mergeFields[In, Out](
        fieldResults,
        in,
        ctx,
        generateArrayToBeanFn[Out](beanFieldsWithTypes, beanInfo.defaultConstructor, -1, fieldResults.size)
      )
    }

    /** Accumulate every field result into a heterogeneous `Array[Any]`, then turn that array into `Out`. */
    private def mergeFields[In: Type, Out: Type](
        fieldResults: List[FieldResolution[In]],
        in: Expr[In],
        ctx: Expr[Ctx],
        constructorFn: Expr[(Array[Any], Unit) => Out]
    ): Expr[Res[Out]] = {
      implicit val arrType: Type[Array[Any]] = Type.of[Array[Any]]
      implicit val unitType: Type[Unit] = Type.of[Unit]
      val n = fieldResults.size

      val initResult: Expr[Res[Array[Any]]] =
        generatePureResult[Array[Any]](Expr.quote(new Array[Any](Expr.splice(Expr(n)))))

      val merged = fieldResults.zipWithIndex.foldLeft(initResult) { case (accum, (fieldResult, index)) =>
        fieldResult match {
          case p: FieldResolution.Pure[In, of] =>
            implicit val ofT: Type[of] = p.fieldType
            mergeOne[of](ctx, accum, generatePureResult[of](p.getValue(in)), index)
          case e: FieldResolution.Effectful[In, of] =>
            implicit val ofT: Type[of] = e.fieldType
            mergeOne[of](ctx, accum, e.getValue(in, ctx), index)
        }
      }

      generateMergeResults[Array[Any], Unit, Out](
        ctx,
        merged,
        generatePureResult[Unit](Expr.quote(())),
        constructorFn
      )
    }

    private def mergeOne[OF: Type](
        ctx: Expr[Ctx],
        accum: Expr[Res[Array[Any]]],
        fieldValue: Expr[Res[OF]],
        index: Int
    ): Expr[Res[Array[Any]]] = {
      implicit val arrType: Type[Array[Any]] = Type.of[Array[Any]]
      val merger: Expr[(Array[Any], OF) => Array[Any]] =
        LambdaBuilder.of2[Array[Any], OF]().buildWith[Array[Any]] { case (arrE, valueE) =>
          Expr.quote {
            Expr.splice(arrE).update(Expr.splice(Expr(index)), Expr.splice(valueE))
            Expr.splice(arrE)
          }
        }
      generateMergeResults[Array[Any], OF, Array[Any]](ctx, accum, fieldValue, merger)
    }
  }
}
