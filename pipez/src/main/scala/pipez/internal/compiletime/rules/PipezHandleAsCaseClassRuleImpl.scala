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
          case Left(ccReason) =>
            // Try Java Bean detection as fallback
            detectBeanOut[Out] match {
              case Some(beanInfo) =>
                deriveBeanProduct[In, Out](beanInfo).map(Rule.matched)
              case None =>
                MIO.pure(Rule.yielded(s"${Type[Out].prettyPrint} is not a case class: $ccReason"))
            }
        }
      }

    // ---- Field resolution result ----

    sealed trait FieldResult
    object FieldResult {
      final case class Pure(name: String, tpe: ??, get: (Expr[Any], Expr[Any]) => Expr[Any]) extends FieldResult
      final case class Effectful(name: String, tpe: ??, get: (Expr[Any], Expr[Any]) => Expr[Any]) extends FieldResult
    }

    // ---- Java Bean support ----

    /** Represents a detected Java Bean "field" via its setter method */
    private final case class BeanField(
        name: String,      // field name (e.g. "a" from setA)
        fieldType: ??,     // the setter parameter type
        setter: Method     // the setter method reference
    )

    /** Represents a detected Java Bean output type */
    private final case class BeanInfo[A](
        fields: List[BeanField],
        defaultConstructor: Method // no-arg constructor
    )

    private val setterPattern = raw"set(.)(.*)".r

    private def extractSetterFieldName(name: String): Option[String] = name match {
      case setterPattern(head, tail) => Some(head.toLowerCase + tail)
      case _                         => None
    }

    /** Detects if a type is a Java Bean: has a default (no-arg) constructor and setter methods */
    private def detectBeanOut[A: Type]: Option[BeanInfo[A]] = {
      // Look for a no-arg constructor
      val defaultCtorOpt = Type[A].constructors.find { ctor =>
        ctor.totalParameters.flatten.isEmpty
      }

      defaultCtorOpt.flatMap { defaultCtor =>
        // Find setter methods
        val setterFields = Type[A].methods.flatMap { method =>
          val methodName = method.name
          extractSetterFieldName(methodName).flatMap { fieldName =>
            val params = method.totalParameters.flatten
            if (params.size == 1) {
              val (_, param) = params.head
              Some(BeanField(fieldName, param.tpe, method))
            } else None
          }
        }

        if (setterFields.nonEmpty) {
          Some(BeanInfo[A](setterFields.toList, defaultCtor))
        } else None
      }
    }

    // ---- Product derivation entry (case class) ----

    private def deriveProduct[In: Type, Out: Type](outClass: CaseClass[Out])(using
        ctx: DerivationCtx[In, Out]
    ): MIO[Expr[Pipe[In, Out]]] = {
      val outParams: List[(String, Parameter)] = outClass.primaryConstructor.totalParameters.flatten

      if (outParams.isEmpty) {
        MIO.pure(deriveZeroFieldProduct[In, Out](outClass))
      } else {
        val inIsTuple = isTuple[In]
        val outIsTuple = isTuple[Out]
        MIO {
          val fieldResults = outParams.zipWithIndex.map { case ((outName, outParam), idx) =>
            val outFieldType = outParam.tpe
            // For tuple input/output, try positional matching first
            if (inIsTuple || outIsTuple) {
              resolveFieldWithPositional[In, Out](outName, outFieldType, idx)
            } else {
              resolveField[In, Out](outName, outFieldType)
            }
          }
          buildProduct[In, Out](fieldResults, outClass)
        }
      }
    }

    // ---- Bean product derivation entry ----

    private def deriveBeanProduct[In: Type, Out: Type](beanInfo: BeanInfo[Out])(using
        ctx: DerivationCtx[In, Out]
    ): MIO[Expr[Pipe[In, Out]]] = {
      val beanFields = beanInfo.fields

      if (beanFields.isEmpty) {
        // No-arg bean with no setters - just construct it
        MIO.pure(deriveZeroBeanProduct[In, Out](beanInfo))
      } else {
        val inIsTuple = isTuple[In]
        MIO {
          val fieldResults = beanFields.zipWithIndex.map { case (bf, idx) =>
            if (inIsTuple) {
              resolveFieldWithPositional[In, Out](bf.name, bf.fieldType, idx)
            } else {
              resolveField[In, Out](bf.name, bf.fieldType)
            }
          }
          buildBeanProduct[In, Out](fieldResults, beanInfo)
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

    private def callDefaultConstructor[A: Type](
        defaultCtor: Method
    ): Expr[A] =
      defaultCtor.fold(
        onInstance = _ => throw new RuntimeException("Default constructor should not need instance"),
        onTypes = _ => Map.empty,
        onValues = _ => Map.empty
      ) match {
        case Right(result) => result.value.asInstanceOf[Expr[A]]
        case Left(err)     => throw new RuntimeException(s"Cannot call default constructor for ${Type[A].prettyPrint}: $err")
      }

    private def deriveZeroFieldProduct[In: Type, Out: Type](outClass: CaseClass[Out]): Expr[Pipe[In, Out]] =
      generateLift[In, Out] { (_, _) =>
        callPrimaryConstructor(outClass, Map.empty) match {
          case Right(outExpr) => generatePureResult(outExpr.asInstanceOf[Expr[Any]])
          case Left(err)      => throw new RuntimeException(s"Cannot construct ${Type[Out].prettyPrint}: $err")
        }
      }

    private def deriveZeroBeanProduct[In: Type, Out: Type](beanInfo: BeanInfo[Out]): Expr[Pipe[In, Out]] =
      generateLift[In, Out] { (_, _) =>
        val outExpr = callDefaultConstructor[Out](beanInfo.defaultConstructor)
        generatePureResult(outExpr.asInstanceOf[Expr[Any]])
      }

    // ---- Field resolution ----

    /** Resolve a field with positional matching for tuples. Config overrides still apply by name. */
    private def resolveFieldWithPositional[In: Type, Out: Type](
        outName: String,
        outFieldType: ??,
        positionIndex: Int
    )(using ctx: DerivationCtx[In, Out]): FieldResult = {
      import outFieldType.{Underlying as OutField}

      // Config operations still use name-based matching
      val configAdd    = ctx.settings.addedFields.find(e => matchFieldName(e.outFieldName, outName))
      val configRename = ctx.settings.renamedFields.find(e => matchFieldName(e.outFieldName, outName))
      val configPlugIn = ctx.settings.pluggedFields.find(e => matchFieldName(e.outFieldName, outName))

      if (configAdd.isDefined || configRename.isDefined || configPlugIn.isDefined) {
        // Delegate to name-based resolution for config overrides
        resolveField[In, Out](outName, outFieldType)
      } else {
        // Try positional matching first, then fall back to name-based
        findInFieldByIndex[In](positionIndex) match {
          case Some((inFieldTpe, getField)) =>
            import inFieldTpe.{Underlying as InField}
            if (Type[InField] <:< Type[OutField]) {
              FieldResult.Pure(outName, outFieldType, (in, _) => getField(in.asInstanceOf[Expr[In]]))
            } else {
              summonPipe[InField, OutField] match {
                case Some(pipe) =>
                  FieldResult.Effectful(
                    outName,
                    outFieldType,
                    (in, ctx) => {
                      val fieldExpr = getField(in.asInstanceOf[Expr[In]])
                      val updCtx    = generateUpdateContext(ctx, pathFieldCode(outName))
                      generateUnlift(pipe.asInstanceOf[Expr[Any]], fieldExpr, updCtx)
                    }
                  )
                case None =>
                  throw new RuntimeException(
                    s"Couldn't find implicit Pipe[${Type[InField].prettyPrint}, ${Type[OutField].prettyPrint}], " +
                      s"required by ${Type[In].prettyPrint} position $positionIndex to ${Type[Out].prettyPrint}.$outName conversion"
                  )
              }
            }
          case None =>
            // Fall back to name-based resolution
            resolveField[In, Out](outName, outFieldType)
        }
      }
    }

    private def resolveField[In: Type, Out: Type](
        outName: String,
        outFieldType: ??
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
        resolveByNameOrFallback[In, Out, OutField](lookupName, outName, outFieldType)
      }
    }

    private def resolveByNameOrFallback[In: Type, Out: Type, OutField: Type](
        inFieldName: String,
        outFieldName: String,
        outFieldType: ??
    )(using ctx: DerivationCtx[In, Out]): FieldResult = {
      val inFieldOpt = findInField[In](inFieldName)

      inFieldOpt match {
        case Some((inFieldTpe, getField)) =>
          import inFieldTpe.{Underlying as InField}
          if (Type[InField] <:< Type[OutField]) {
            FieldResult.Pure(outFieldName, outFieldType, (in, _) => getField(in.asInstanceOf[Expr[In]]))
          } else {
            // Try summoning an implicit pipe, or derive recursively if enabled
            val pipeExpr = summonPipe[InField, OutField].getOrElse {
              if (ctx.settings.isRecursiveDerivationEnabled) {
                // Recursively derive Pipe[InField, OutField]
                val newCtx = DerivationCtx[InField, OutField](
                  inType = Type[InField],
                  outType = Type[OutField],
                  settings = ctx.settings.stripForRecursion,
                  derivedPipeType = ctx.derivedPipeType
                )
                val mio = deriveResultRecursively[InField, OutField](using Type[InField], Type[OutField], newCtx)
                // Execute the MIO synchronously - we're already inside an MIO { } block
                val (_, result) = mio.unsafe.runSync
                result.fold(
                  errors => throw errors.head,
                  identity
                )
              } else {
                throw new RuntimeException(
                  s"Couldn't find implicit Pipe[${Type[InField].prettyPrint}, ${Type[OutField].prettyPrint}], " +
                    s"required by ${Type[In].prettyPrint}.$inFieldName to ${Type[Out].prettyPrint}.$outFieldName conversion"
                )
              }
            }
            FieldResult.Effectful(
              outFieldName,
              outFieldType,
              (in, ctx) => {
                val fieldExpr = getField(in.asInstanceOf[Expr[In]])
                val updCtx    = generateUpdateContext(ctx, pathFieldCode(outFieldName))
                generateUnlift(pipeExpr.asInstanceOf[Expr[Any]], fieldExpr, updCtx)
              }
            )
          }

        case None =>
          resolveFallback[In, Out](outFieldName).getOrElse(
            throw new RuntimeException(
              s"Couldn't find a field/method which could be used as a source for $outFieldName from ${Type[Out].prettyPrint}"
            )
          )
      }
    }

    private def matchFieldName(configName: String, paramName: String)(using ctx: DerivationCtx[?, ?]): Boolean =
      inputNameMatchesOutputName(configName, paramName, ctx.settings.isFieldCaseInsensitive)

    // ---- In field extraction ----

    private def isTuple[A: Type]: Boolean =
      Type[A].shortName.startsWith("Tuple") && Type[A] <:< Type.of[Product]

    private def findInField[In: Type](name: String)(using ctx: DerivationCtx[?, ?]): Option[(??, Expr[In] => Expr[Any])] =
      CaseClass.parse[In].toEither match {
        case Right(inClass) =>
          val inParams = inClass.primaryConstructor.totalParameters.flatten
          val fromCaseFields = inParams.collectFirst {
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
          // If case class field not found, also try method getters
          fromCaseFields.orElse(findMethodGetter[In](name))
        case Left(_) =>
          findMethodGetter[In](name)
      }

    /** For tuple output: find the corresponding tuple field from input by position index */
    private def findInFieldByIndex[In: Type](index: Int)(using ctx: DerivationCtx[?, ?]): Option[(??, Expr[In] => Expr[Any])] =
      CaseClass.parse[In].toEither match {
        case Right(inClass) =>
          val inParams = inClass.primaryConstructor.totalParameters.flatten
          if (index < inParams.size) {
            val (paramName, param) = inParams(index)
            val paramType = param.tpe
            val getter: Expr[In] => Expr[Any] = in => {
              inClass.caseFieldValuesAt(in).toList.collectFirst {
                case (n, fv) if n == paramName => fv.value.asInstanceOf[Expr[Any]]
              }.getOrElse(throw new RuntimeException(s"Field $paramName not found"))
            }
            Some((paramType, getter))
          } else None
        case Left(_) =>
          // Try method getter by _N name
          findMethodGetter[In](s"_${index + 1}")
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
        outFieldName: String
    )(using ctx: DerivationCtx[In, Out]): Option[FieldResult] = {
      val fromFallbackValues = ctx.settings.fallbackValues.view.flatMap { fv =>
        import fv.fallbackType.{Underlying as FV}
        // Try case class fields first
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
        }.orElse {
          // Try bean getters for fallback values
          findMethodGetter[FV](outFieldName)(using Type.of[FV].asInstanceOf[Type[FV]], ctx).map { case (retType, getter) =>
            FieldResult.Pure(
              outFieldName,
              retType,
              (_, _) => getter(fv.fallbackValue.asInstanceOf[Expr[FV]])
            )
          }
        }
      }.headOption

      fromFallbackValues.orElse {
        if (ctx.settings.isFallbackToDefaultEnabled) {
          // Try case class defaults (bean types don't have constructor defaults)
          CaseClass.parse[Out].toEither.toOption.flatMap { outClass =>
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
          }
        } else None
      }
    }

    // ---- Code building (case class) ----

    private def buildProduct[In: Type, Out: Type](
        fieldResults: List[FieldResult],
        outClass: CaseClass[Out]
    ): Expr[Pipe[In, Out]] = {
      val allPure = fieldResults.forall(_.isInstanceOf[FieldResult.Pure])

      if (allPure) {
        generateLift[In, Out] { (in, _) =>
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
      val n = fieldResults.size

      generateLift[In, Out] { (in, ctx) =>
        val initResult: Expr[Any] = generatePureResult(
          Expr.quote { new Array[Any](Expr.splice(Expr(n))) }
        )

        // Fold all fields: each merge stores a value in the array and returns the array
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

        // Final merge: extract all values from array and construct the output.
        // The constructor function receives the array and a dummy Unit.
        val constructorFn = generateArrayToConstructorFn[Out](outClass, -1, n)
        generateMergeResults(
          ctx,
          merged,
          generatePureResult(Expr.quote(()).asInstanceOf[Expr[Any]]),
          constructorFn
        )
      }
    }

    // ---- Code building (bean) ----

    private def buildBeanProduct[In: Type, Out: Type](
        fieldResults: List[FieldResult],
        beanInfo: BeanInfo[Out]
    ): Expr[Pipe[In, Out]] =
      // Always use the array-based effectful path for beans.
      // The pure path has issues with bean expressions being evaluated multiple times
      // (creating separate instances for setter calls vs the returned value).
      // The effectful path properly creates a single bean instance via the
      // array-to-bean constructor function in the bridge.
      buildBeanProductWithEffects[In, Out](fieldResults, beanInfo)

    private def buildBeanProductWithEffects[In: Type, Out: Type](
        fieldResults: List[FieldResult],
        beanInfo: BeanInfo[Out]
    ): Expr[Pipe[In, Out]] = {
      val n = fieldResults.size

      generateLift[In, Out] { (in, ctx) =>
        val initResult: Expr[Any] = generatePureResult(
          Expr.quote { new Array[Any](Expr.splice(Expr(n))) }
        )

        // Fold all fields: each merge stores a value in the array and returns the array
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

        // Final merge: extract all values from array and construct the bean.
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
}
