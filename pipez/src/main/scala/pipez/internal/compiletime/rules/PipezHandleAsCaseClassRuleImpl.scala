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

    def apply[In: Type, Out: Type](in: Expr[In], ctx: Expr[Any])(implicit
        dctx: DerivationCtx[In, Out]
    ): MIO[Rule.Applicability[Expr[Any]]] =
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

    sealed trait FieldResolution
    object FieldResolution {
      final case class Pure(name: String, tpe: ??, getValue: Expr[Any] => Expr[Any]) extends FieldResolution
      final case class Effectful(name: String, tpe: ??, getValue: (Expr[Any], Expr[Any]) => Expr[Any])
          extends FieldResolution
    }

    // ---- Java Bean support ----

    final private case class BeanField(
        name: String,
        fieldType: ??,
        setter: Method
    )

    final private case class BeanInfo[A](
        fields: List[BeanField],
        defaultConstructor: Method
    )

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
        ctx: Expr[Any],
        outClass: CaseClass[Out]
    )(implicit dctx: DerivationCtx[In, Out]): MIO[Expr[Any]] = {
      val outParams: List[(String, Parameter)] = outClass.primaryConstructor.totalParameters.flatten

      if (outParams.isEmpty)
        MIO.pure(deriveZeroFieldProduct[In, Out](outClass))
      else {
        val inIsTuple = isTuple[In]
        val outIsTuple = isTuple[Out]
        outParams.zipWithIndex
          .foldLeft(MIO.pure(List.empty[FieldResolution])) { case (accMIO, ((outName, outParam), idx)) =>
            accMIO.flatMap { acc =>
              val resolve =
                if (inIsTuple || outIsTuple) resolveFieldWithPositional[In, Out](outName, outParam.tpe, idx)
                else resolveField[In, Out](outName, outParam.tpe)
              resolve.map(fr => acc :+ fr)
            }
          }
          .map { fieldResults =>
            buildProduct[In, Out](fieldResults, in, ctx, outClass)
          }
      }
    }

    // ---- Bean product derivation entry ----

    private def deriveBeanProduct[In: Type, Out: Type](
        in: Expr[In],
        ctx: Expr[Any],
        beanInfo: BeanInfo[Out]
    )(implicit dctx: DerivationCtx[In, Out]): MIO[Expr[Any]] =
      if (beanInfo.fields.isEmpty)
        MIO.pure(deriveZeroBeanProduct[Out](beanInfo))
      else {
        val inIsTuple = isTuple[In]
        beanInfo.fields.zipWithIndex
          .foldLeft(MIO.pure(List.empty[FieldResolution])) { case (accMIO, (bf, idx)) =>
            accMIO.flatMap { acc =>
              val resolve =
                if (inIsTuple) resolveFieldWithPositional[In, Out](bf.name, bf.fieldType, idx)
                else resolveField[In, Out](bf.name, bf.fieldType)
              resolve.map(fr => acc :+ fr)
            }
          }
          .map { fieldResults =>
            buildBeanProduct[In, Out](fieldResults, in, ctx, beanInfo)
          }
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

    private def deriveZeroFieldProduct[In: Type, Out: Type](outClass: CaseClass[Out]): Expr[Any] =
      callPrimaryConstructor(outClass, Map.empty) match {
        case Right(outExpr) => generatePureResult(outExpr.asInstanceOf[Expr[Any]])
        case Left(err)      => throw new RuntimeException(s"Cannot construct ${Type[Out].prettyPrint}: $err")
      }

    private def deriveZeroBeanProduct[Out: Type](beanInfo: BeanInfo[Out]): Expr[Any] = {
      val outExpr = callDefaultConstructor[Out](beanInfo.defaultConstructor)
      generatePureResult(outExpr.asInstanceOf[Expr[Any]])
    }

    // ---- Field resolution ----

    private def resolveFieldWithPositional[In: Type, Out: Type](
        outName: String,
        outFieldType: ??,
        positionIndex: Int
    )(implicit dctx: DerivationCtx[In, Out]): MIO[FieldResolution] = {
      import outFieldType.Underlying as OutField

      val configAdd = dctx.settings.addedFields.find(e => matchFieldName(e.outFieldName, outName))
      val configRename = dctx.settings.renamedFields.find(e => matchFieldName(e.outFieldName, outName))
      val configPlugIn = dctx.settings.pluggedFields.find(e => matchFieldName(e.outFieldName, outName))

      if (configAdd.isDefined || configRename.isDefined || configPlugIn.isDefined)
        resolveField[In, Out](outName, outFieldType)
      else
        findInFieldByIndex[In](positionIndex) match {
          case Some((inFieldTpe, getField)) =>
            import inFieldTpe.Underlying as InField
            if (Type[InField] <:< Type[OutField])
              MIO.pure(FieldResolution.Pure(outName, outFieldType, in => getField(in.asInstanceOf[Expr[In]])))
            else
              summonOrDerive[InField, OutField].map { pipe =>
                FieldResolution.Effectful(
                  outName,
                  outFieldType,
                  (in, ctxE) => {
                    val fieldExpr = getField(in.asInstanceOf[Expr[In]])
                    val updCtx = generateUpdateContext(ctxE, pathFieldCode(outName))
                    generateUnlift(pipe.asInstanceOf[Expr[Any]], fieldExpr, updCtx)
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
    )(implicit dctx: DerivationCtx[In, Out]): MIO[FieldResolution] = {
      import outFieldType.Underlying as OutField

      val configAdd = dctx.settings.addedFields.find(e => matchFieldName(e.outFieldName, outName))
      val configRename = dctx.settings.renamedFields.find(e => matchFieldName(e.outFieldName, outName))
      val configPlugIn = dctx.settings.pluggedFields.find(e => matchFieldName(e.outFieldName, outName))

      if (configAdd.isDefined) {
        val pipe = configAdd.get.pipe
        MIO.pure(FieldResolution.Effectful(outName, outFieldType, (in, ctxE) => generateUnlift(pipe, in, ctxE)))
      } else if (configPlugIn.isDefined) {
        val entry = configPlugIn.get
        MIO.pure(
          FieldResolution.Effectful(
            outName,
            outFieldType,
            (in, ctxE) => {
              val fieldExpr = extractFieldFromIn[In](in.asInstanceOf[Expr[In]], entry.inFieldName)
              val updCtx = generateUpdateContext(ctxE, pathFieldCode(outName))
              generateUnlift(entry.pipe, fieldExpr, updCtx)
            }
          )
        )
      } else {
        val lookupName = configRename.map(_.inFieldName).getOrElse(outName)
        resolveByNameOrFallback[In, Out, OutField](lookupName, outName, outFieldType)
      }
    }

    private def resolveByNameOrFallback[In: Type, Out: Type, OutField: Type](
        inFieldName: String,
        outFieldName: String,
        outFieldType: ??
    )(implicit dctx: DerivationCtx[In, Out]): MIO[FieldResolution] =
      findInField[In](inFieldName) match {
        case Some((inFieldTpe, getField)) =>
          import inFieldTpe.Underlying as InField
          if (Type[InField] <:< Type[OutField])
            MIO.pure(FieldResolution.Pure(outFieldName, outFieldType, in => getField(in.asInstanceOf[Expr[In]])))
          else
            summonOrDerive[InField, OutField].map { pipe =>
              FieldResolution.Effectful(
                outFieldName,
                outFieldType,
                (in, ctxE) => {
                  val fieldExpr = getField(in.asInstanceOf[Expr[In]])
                  val updCtx = generateUpdateContext(ctxE, pathFieldCode(outFieldName))
                  generateUnlift(pipe.asInstanceOf[Expr[Any]], fieldExpr, updCtx)
                }
              )
            }

        case None =>
          resolveFallback[In, Out](outFieldName) match {
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
    )(implicit dctx: DerivationCtx[?, ?]): Option[(??, Expr[In] => Expr[Any])] =
      CaseClass.parse[In].toEither match {
        case Right(inClass) =>
          val inParams = inClass.primaryConstructor.totalParameters.flatten
          val fromCaseFields = inParams.collectFirst {
            case (paramName, _) if inputNameMatchesOutputName(paramName, name, dctx.settings.isFieldCaseInsensitive) =>
              val getter: Expr[In] => Expr[Any] = in =>
                inClass
                  .caseFieldValuesAt(in)
                  .toList
                  .collectFirst { case (n, fv) if n == paramName => fv.value.asInstanceOf[Expr[Any]] }
                  .getOrElse(throw new RuntimeException(s"Field $paramName not found"))
              val paramType: ?? = inParams.find(_._1 == paramName).get._2.tpe
              (paramType, getter)
          }
          fromCaseFields.orElse(findMethodGetter[In](name))
        case Left(_) =>
          findMethodGetter[In](name)
      }

    private def findInFieldByIndex[In: Type](
        index: Int
    )(implicit dctx: DerivationCtx[?, ?]): Option[(??, Expr[In] => Expr[Any])] =
      CaseClass.parse[In].toEither match {
        case Right(inClass) =>
          val inParams = inClass.primaryConstructor.totalParameters.flatten
          if (index < inParams.size) {
            val (paramName, param) = inParams(index)
            val getter: Expr[In] => Expr[Any] = in =>
              inClass
                .caseFieldValuesAt(in)
                .toList
                .collectFirst { case (n, fv) if n == paramName => fv.value.asInstanceOf[Expr[Any]] }
                .getOrElse(throw new RuntimeException(s"Field $paramName not found"))
            Some((param.tpe, getter))
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
    ): Option[(??, Expr[In] => Expr[Any])] =
      Type[In].methods.iterator
        .flatMap { method =>
          if (
            !ignoredMethodNames(method.name) &&
            !method.name.contains("$default$") &&
            method.totalParameters.flatten.isEmpty &&
            inputNameMatchesOutputName(dropGetIs(method.name), name, dctx.settings.isFieldCaseInsensitive)
          ) {
            val returnType: ?? = method.knownReturning.getOrElse(Type.of[Any].as_??)
            val getter: Expr[In] => Expr[Any] = in =>
              method.fold(
                onInstance = oi => in.asInstanceOf[Expr[oi.Instance]].as_??(oi.Instance),
                onTypes = _ => Map.empty,
                onValues = _ => Map.empty
              ) match {
                case Right(result) => result.value.asInstanceOf[Expr[Any]]
                case Left(err)     => throw new RuntimeException(s"Cannot call getter ${method.name}: $err")
              }
            Some((returnType, getter))
          } else None
        }
        .nextOption()

    private def extractFieldFromIn[In: Type](in: Expr[In], fieldName: String)(implicit
        dctx: DerivationCtx[?, ?]
    ): Expr[Any] =
      findInField[In](fieldName) match {
        case Some((_, getter)) => getter(in)
        case None              => throw new RuntimeException(s"Field $fieldName not found in ${Type[In].prettyPrint}")
      }

    // ---- Fallback resolution ----

    private def resolveFallback[In: Type, Out: Type](
        outFieldName: String
    )(implicit dctx: DerivationCtx[In, Out]): Option[FieldResolution] = {
      val fromFallbackValues = dctx.settings.fallbackValues.view.flatMap { fv =>
        import fv.fallbackType.Underlying as FV
        CaseClass
          .parse[FV]
          .toEither
          .toOption
          .flatMap { fvClass =>
            fvClass.primaryConstructor.totalParameters.flatten.collectFirst {
              case (n, _) if inputNameMatchesOutputName(n, outFieldName, dctx.settings.isFieldCaseInsensitive) =>
                FieldResolution.Pure(
                  outFieldName,
                  fvClass.primaryConstructor.totalParameters.flatten.find(_._1 == n).get._2.tpe,
                  _ => {
                    val fvExpr = fv.fallbackValue.asInstanceOf[Expr[FV]]
                    fvClass.caseFieldValuesAt(fvExpr).toList.find(_._1 == n).get._2.value.asInstanceOf[Expr[Any]]
                  }
                )
            }
          }
          .orElse {
            findMethodGetter[FV](outFieldName)(Type.of[FV].asInstanceOf[Type[FV]], dctx).map { case (retType, getter) =>
              FieldResolution.Pure(
                outFieldName,
                retType,
                _ => getter(fv.fallbackValue.asInstanceOf[Expr[FV]])
              )
            }
          }
      }.headOption

      fromFallbackValues.orElse {
        if (dctx.settings.isFallbackToDefaultEnabled) {
          CaseClass.parse[Out].toEither.toOption.flatMap { outClass =>
            outClass.primaryConstructor.totalParameters.flatten.toMap.get(outFieldName).flatMap { param =>
              param.defaultValue.flatMap { defaultMethod =>
                defaultMethod
                  .fold(
                    onInstance = _ => throw new RuntimeException("Default value should not need instance"),
                    onTypes = _ => Map.empty,
                    onValues = _ => Map.empty
                  )
                  .toOption
                  .map { defaultExprExistential =>
                    FieldResolution.Pure(
                      outFieldName,
                      param.tpe,
                      _ => defaultExprExistential.value.asInstanceOf[Expr[Any]]
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
        fieldResults: List[FieldResolution],
        in: Expr[In],
        ctx: Expr[Any],
        outClass: CaseClass[Out]
    ): Expr[Any] = {
      val allPure = fieldResults.forall(_.isInstanceOf[FieldResolution.Pure])
      val inAny = in.asInstanceOf[Expr[Any]]

      if (allPure) {
        val fieldMap = fieldResults.map {
          case FieldResolution.Pure(name, _, get) => name -> get(inAny).as_??
          case _                                  => throw new RuntimeException("Unreachable")
        }.toMap
        callPrimaryConstructor(outClass, fieldMap) match {
          case Right(outExpr) => generatePureResult(outExpr.asInstanceOf[Expr[Any]])
          case Left(err)      => throw new RuntimeException(s"Cannot construct ${Type[Out].prettyPrint}: $err")
        }
      } else
        buildProductWithEffects[In, Out](fieldResults, in, ctx, outClass)
    }

    private def buildProductWithEffects[In: Type, Out: Type](
        fieldResults: List[FieldResolution],
        in: Expr[In],
        ctx: Expr[Any],
        outClass: CaseClass[Out]
    ): Expr[Any] = {
      val n = fieldResults.size
      val inAny = in.asInstanceOf[Expr[Any]]

      val initResult: Expr[Any] = generatePureResult(
        Expr.quote(new Array[Any](Expr.splice(Expr(n))))
      )

      val merged = fieldResults.zipWithIndex.foldLeft(initResult) { case (accum, (fieldResult, index)) =>
        val fieldValue = fieldResult match {
          case FieldResolution.Pure(_, _, get)      => generatePureResult(get(inAny))
          case FieldResolution.Effectful(_, _, get) => get(inAny, ctx)
        }
        val merger = Expr.quote { (arr: Array[Any], value: Any) =>
          arr(Expr.splice(Expr(index))) = value
          arr
        }
        generateMergeResults(ctx, accum, fieldValue, merger.asInstanceOf[Expr[Any]])
      }

      val constructorFn = generateArrayToConstructorFn[Out](outClass, -1, n)
      generateMergeResults(
        ctx,
        merged,
        generatePureResult(Expr.quote(()).asInstanceOf[Expr[Any]]),
        constructorFn
      )
    }

    // ---- Code building (bean) ----

    private def buildBeanProduct[In: Type, Out: Type](
        fieldResults: List[FieldResolution],
        in: Expr[In],
        ctx: Expr[Any],
        beanInfo: BeanInfo[Out]
    ): Expr[Any] = {
      val n = fieldResults.size
      val inAny = in.asInstanceOf[Expr[Any]]

      val initResult: Expr[Any] = generatePureResult(
        Expr.quote(new Array[Any](Expr.splice(Expr(n))))
      )

      val merged = fieldResults.zipWithIndex.foldLeft(initResult) { case (accum, (fieldResult, index)) =>
        val fieldValue = fieldResult match {
          case FieldResolution.Pure(_, _, get)      => generatePureResult(get(inAny))
          case FieldResolution.Effectful(_, _, get) => get(inAny, ctx)
        }
        val merger = Expr.quote { (arr: Array[Any], value: Any) =>
          arr(Expr.splice(Expr(index))) = value
          arr
        }
        generateMergeResults(ctx, accum, fieldValue, merger.asInstanceOf[Expr[Any]])
      }

      val beanFieldsWithTypes = beanInfo.fields.map(bf => (bf.fieldType, bf.setter))
      val constructorFn = generateArrayToBeanFn[Out](beanFieldsWithTypes, beanInfo.defaultConstructor, -1, n)
      generateMergeResults(
        ctx,
        merged,
        generatePureResult(Expr.quote(()).asInstanceOf[Expr[Any]]),
        constructorFn
      )
    }
  }
}
