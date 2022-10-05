package pipez.internal

import pipez.internal.Definitions.{ Context, Result }
import pipez.internal.ProductCaseGeneration.{ inputNameMatchesOutputName, setAccessor }

import scala.annotation.nowarn
import scala.collection.immutable.ListMap
import scala.util.chaining.*

@nowarn("msg=The outer reference in this type test cannot be checked at run time.")
private[internal] trait ProductCaseGeneration[Pipe[_, _], In, Out] {
  self: Definitions[Pipe, In, Out] & Generators[Pipe, In, Out] =>

  /** True iff `A` is a tuple */
  def isTuple[A: Type]: Boolean

  /** True iff `A` is defined as `case class`, is NOT abstract and has a public constructor */
  def isCaseClass[A: Type]: Boolean

  /** True iff `A` is defined as `case object`, and is public.
    *
    * Exception's are Scala 3's cases without parameters - Scala 3 sees then as vals and Scala as non-case modules
    */
  def isCaseObject[A: Type]: Boolean

  /** True iff `A` has a (public) default constructor and at least one (public) method starting with `set`.
    *
    * The exceptions are Java Beans compiled with Scala 3 which expose `@BeanProperty var a: Int` as `var` as opposed to
    * pair of `getA` and `setA` - for that reason we need a special handling of Scala 3's beans
    */
  def isJavaBean[A: Type]: Boolean

  /** Whether `Out` type could be constructed as "product case" */
  final def isUsableAsProductOutput: Boolean =
    isCaseClass[Out] || isCaseObject[Out] || isJavaBean[Out]

  /** Should create `Out` expression from the constructor arguments grouped in parameter lists */
  type Constructor = List[List[Expr[Any]]] => Expr[Out]

  sealed trait FieldFallback[+OutField] extends Product with Serializable
  object FieldFallback {
    final case class Value[OutField, Value](tpe: Type[Value], expr: Expr[Value]) extends FieldFallback[OutField]
    final case class Default[OutField](expr: Expr[OutField]) extends FieldFallback[OutField]
    case object Unavailable extends FieldFallback[Nothing]
  }

  /** Stores information how each attribute/getter could be extracted from `In` value */
  final case class ProductInData(getters: ListMap[String, ProductInData.Getter[?]]) {

    def findGetter(
      inParamName:           String,
      outParamName:          String,
      caseInsensitiveSearch: Boolean
    ): DerivationResult[ProductInData.Getter[?]] =
      DerivationResult.fromOption(
        getters.collectFirst {
          case (_, getter) if inputNameMatchesOutputName(getter.name, inParamName, caseInsensitiveSearch) => getter
        }
      )(DerivationError.MissingPublicSource(outParamName))

    def findIndex(index: Int, outParamName: String): DerivationResult[ProductInData.Getter[?]] =
      DerivationResult.fromOption(
        getters.toList.lift(index).map(_._2)
      )(DerivationError.MissingPublicSource(outParamName))
  }
  object ProductInData {

    final case class Getter[InField](
      name: String,
      tpe:  Type[InField],
      get:  Expr[In] => Expr[InField],
      path: Path
    ) {

      override def toString: String = s"Getter($name : ${previewType(tpe)})"
    }
  }

  /** Stores information how `Out` value could be constructed from values of constructor parameters/passed to setters */
  sealed trait ProductOutData extends Product with Serializable
  object ProductOutData {

    final case class ConstructorParam[OutField](
      name:     String,
      tpe:      Type[OutField],
      fallback: FieldFallback[OutField]
    )
    final case class CaseClass(
      caller: Constructor,
      params: List[ListMap[String, ConstructorParam[?]]]
    ) extends ProductOutData {
      override def toString: String = s"CaseClass${params.map { list =>
          "(" + list.map { case (n, p) => s"$n : ${previewType(p.tpe)}" }.mkString(", ") + ")"
        }.mkString}"
    }

    final case class Setter[OutField](
      name:     String,
      tpe:      Type[OutField],
      set:      (Expr[Out], Expr[OutField]) => Expr[Unit],
      fallback: FieldFallback[OutField]
    ) {

      override def toString: String = s"Setter($name : ${previewType(tpe)})"
    }
    final case class JavaBean(
      defaultConstructor: Expr[Out],
      setters:            ListMap[String, Setter[?]]
    ) extends ProductOutData {
      override def toString: String = s"JavaBean(${setters.map { case (n, p) => s"$n : $p" }.mkString(", ")})"
    }
  }

  /** Value generation strategy for a particular output parameter/setter */
  sealed trait OutFieldLogic[OutField] extends Product with Serializable
  object OutFieldLogic {

    final case class DefaultField[OutField]() extends OutFieldLogic[OutField]

    final case class FieldAdded[OutField](
      pipe: Expr[Pipe[In, OutField]]
    ) extends OutFieldLogic[OutField] {
      override def toString: String = s"FieldAdded(${previewCode(pipe)})"
    }

    final case class FieldRenamed[InField, OutField](
      inField:     String,
      inFieldType: Type[InField]
    ) extends OutFieldLogic[OutField] {
      override def toString: String = s"FieldRenamed($inField : ${previewType(inFieldType)})"
    }

    final case class PipeProvided[InField, OutField](
      inField:     String,
      inFieldType: Type[InField],
      pipe:        Expr[Pipe[InField, OutField]]
    ) extends OutFieldLogic[OutField] {
      override def toString: String = s"PipeProvided($inField : ${previewType(inFieldType)}, ${previewCode(pipe)})"
    }

    private def resolve[OutField: Type](
      settings:     Settings,
      outFieldName: String
    ): OutFieldLogic[OutField] = {
      import Path.*
      import ConfigEntry.*

      // outFieldName matches what we found as setter/constructor param
      // outFieldGetter in matter matching, what we got from config - user might have used JavaBeans' getter!
      settings.resolve[OutFieldLogic[OutField]](DefaultField()) {
        case AddField(Field(Root, outFieldGetter), outFieldType, pipe)
            if inputNameMatchesOutputName(outFieldGetter, outFieldName, settings.isFieldCaseInsensitive) =>
          // TODO: validate that Out <:< outFieldType is correct
          FieldAdded(pipe.asInstanceOf[Expr[Pipe[In, OutField]]])
        case RenameField(Field(Root, inName), in, Field(Root, outFieldGetter), out)
            if inputNameMatchesOutputName(outFieldGetter, outFieldName, settings.isFieldCaseInsensitive) =>
          // TODO: validate that Out <:< outFieldType is correct
          FieldRenamed(inName, In)
        case PlugInField(Field(Root, inName), in, Field(Root, outFieldGetter), out, pipe)
            if inputNameMatchesOutputName(outFieldGetter, outFieldName, settings.isFieldCaseInsensitive) =>
          // TODO: validate that Out <:< outFieldType is correct
          PipeProvided[Any, OutField](inName, in.asInstanceOf[Type[Any]], pipe.asInstanceOf[Expr[Pipe[Any, OutField]]])
      }
    }

    type InField
    def resolveField[OutField: Type](
      settings:     Settings,
      inData:       ProductInData,
      outParamName: String,
      indexOpt:     Option[Int],
      fallback:     FieldFallback[OutField]
    ): DerivationResult[ProductGeneratorData.OutputValue] = resolve[OutField](settings, outParamName) match {
      case DefaultField() =>
        // if inField (same name as out - same index if tuple) not found then error
        // else if inField <:< outField then (in, ctx) => pure(in : OutField)
        // else (in, ctx) => unlift(summon[InField, OutField], in.outParamName, updateContext(ctx, path)) : Result[OutField]
        indexOpt
          .fold {
            inData.findGetter(outParamName, outParamName, settings.isFieldCaseInsensitive) // match by name
          } { index =>
            inData.findIndex(index, outParamName) // match by index
          }
          .map(_.asInstanceOf[ProductInData.Getter[InField]])
          .flatMap { getter =>
            implicit val tpe: Type[InField] = getter.tpe
            fromFieldConstructorParam[InField, OutField](getter, settings)
          }
          .orElse(fromFallbackValue(outParamName, fallback, settings))
          .log(
            s"Field $outParamName uses default resolution (${
                if (indexOpt.isEmpty) "matching input name" else "matching field position"
              }, summoning)"
          )
      case FieldAdded(pipe) =>
        // (in, ctx) => unlift(pipe, in, ctx) : Result[OutField]
        DerivationResult
          .pure(fieldAddedConstructorParam[OutField](pipe))
          .log(s"Field $outParamName considered added to output, uses provided pipe")
      case FieldRenamed(inFieldName, _) =>
        // if inField (name provided) not found then error
        // else if inField <:< outField then (in, ctx) => pure(in : OutField)
        // else (in, ctx) => unlift(summon[InField, OutField], in.inFieldName, updateContext(ctx, path)) : Result[OutField]
        inData
          .findGetter(inFieldName, outParamName, settings.isFieldCaseInsensitive)
          .map(_.asInstanceOf[ProductInData.Getter[InField]])
          .flatMap { getter =>
            implicit val tpe: Type[InField] = getter.tpe
            fromFieldConstructorParam[InField, OutField](getter, settings)
          }
          .log(s"Field $outParamName is considered renamed from $inFieldName, uses summoning if types differ")
      case PipeProvided(inFieldName, _, pipe) =>
        // if inField (name provided) not found then error
        // else (in, ctx) => unlift(summon[InField, OutField], in.used, updateContext(ctx, path)) : Result[OutField]
        inData
          .findGetter(inFieldName, outParamName, settings.isFieldCaseInsensitive)
          .map(_.asInstanceOf[ProductInData.Getter[InField]])
          .map { getter =>
            implicit val tpe: Type[InField] = getter.tpe
            pipeProvidedConstructorParam[InField, OutField](getter, pipe.asInstanceOf[Expr[Pipe[InField, OutField]]])
          }
          .log(s"Field $outParamName converted from $inFieldName using provided pipe")
    }
  }

  /** Final platform-independent result of matching inputs with outputs using resolved strategies */
  sealed trait ProductGeneratorData extends Product with Serializable
  object ProductGeneratorData {

    sealed trait OutputValue extends Product with Serializable
    object OutputValue {

      final case class Pure[A](
        tpe:    Type[A],
        caller: (Expr[In], Expr[Context]) => Expr[A]
      ) extends OutputValue {
        override def toString: String = s"Pure { (${previewType[In]}, Context) => ${previewType(tpe)} }"
      }

      final case class Result[A](
        tpe:    Type[A],
        caller: (Expr[In], Expr[Context]) => Expr[Definitions.Result[A]]
      ) extends OutputValue {
        override def toString: String = s"Result { (${previewType[In]}, Context) => ${previewType(tpe)} }"
      }
    }

    final case class CaseClass(
      constructor: Constructor,
      output:      List[List[OutputValue]]
    ) extends ProductGeneratorData {
      override def toString: String = s"CaseClass${output.map(list => "(" + list.mkString(", ") + ")").mkString}"
    }

    final case class JavaBean(
      defaultConstructor: Expr[Out],
      output:             List[(OutputValue, ProductOutData.Setter[?])]
    ) extends ProductGeneratorData {
      override def toString: String = s"JavaBean(${output.mkString(", ")})"
    }
  }

  object ProductTypeConversion {

    final def unapply(settings: Settings): Option[DerivationResult[Expr[Pipe[In, Out]]]] =
      if (isUsableAsProductOutput) Some(attemptProductRendering(settings)) else None
  }

  /** Platform-specific way of parsing `In` data
    *
    * Should:
    *   - obtain all methods which are Scala's getters (vals, nullary defs)
    *   - obtain all methods which are Java Bean getters (starting with is- or get-)
    *   - for each create an `InField` factory which takes `In` argument and returns `InField` expression
    *   - form obtained collection into `ProductInData`
    */
  def extractProductInData(settings: Settings): DerivationResult[ProductInData]

  /** Platform-specific way of parsing `Out` data
    *
    * Should:
    *   - verify whether output is a case class, a case object or a Java Bean
    *   - obtain respectively:
    *     - a constructor taking all arguments
    *     - expression containing case object value
    *     - a default constructor and collection of setters respectively
    *   - form obtained data into `ProductOutData`
    */
  def extractProductOutData(settings: Settings): DerivationResult[ProductOutData]

  /** Platform-specific way of generating code from resolved information
    *
    * For case class output should generate code like:
    *
    * {{{
    * pipeDerivation.lift { (in: In, ctx: pipeDerivation.Context) =>
    *   pipeDerivation.mergeResult(
    *      ctx,
    *     pipeDerivation.mergeResult(
    *        ctx,
    *       pipeDerivation.pure(Array[Any](2)),
    *       pipeDerivation.unlift(fooPipe, in.foo, pipeDerivation.updateContext(ctx, Path.root.field("foo"))),
    *       { (left, right) =>
    *         left(0) = right
    *         left
    *        }
    *     ),
    *     pipeDerivation.unlift(barPipe, in.bar, pipeDerivation.updateContext(ctx, Path.root.field("bar"))),
    *     { (left, right) =>
    *       left(1) = right
    *       new Out(
    *         foo = left(0).asInstanceOf[Foo2],
    *         bar = left(1).asInstanceOf[Bar2],
    *       )
    *     }
    *   )
    * }
    * }}}
    *
    * For case object output should generate code like:
    *
    * {{{
    * pipeDerivation.lift { (in: In, ctx: pipeDerivation.Context) =>
    *   pipeDerivation.pure(CaseObject)
    * }
    * }}}
    *
    * For Java Bean should generate code like:
    *
    * {{{
    * pipeDerivation.lift { (in: In, ctx: pipeDerivation.Context) =>
    *   pipeDerivation.mergeResult(
    *     ctx,
    *     pipeDerivation.mergeResult(
    *       ctx,
    *       pipeDerivation.pure {
    *         val result = new Out()
    *         result
    *       },
    *       pipeDerivation.unlift(fooPipe, in.foo, pipeDerivation.updateContext(ctx, Path.root.field("foo"))),
    *       { (left, right) =>
    *         left.setFoo(right)
    *         left
    *       }
    *     ),
    *     pipeDerivation.unlift(barPipe, in.bar, pipeDerivation.updateContext(ctx, Path.root.field("bar"))),
    *     { (left, right) =>
    *       left.setBar(right)
    *       left
    *     }
    *   )
    * }
    * }}}
    */
  def generateProductCode(generatorData: ProductGeneratorData): DerivationResult[Expr[Pipe[In, Out]]]

  private def attemptProductRendering(settings: Settings): DerivationResult[Expr[Pipe[In, Out]]] =
    for {
      data <- extractProductInData(settings) zip extractProductOutData(settings)
      (inData, outData) = data
      generatorData <-
        if (isTuple[In] || isTuple[Out]) matchFieldsByPosition(inData, outData, settings)
        else matchFieldsByName(inData, outData, settings)
      code <- generateProductCode(generatorData)
    } yield code

  // In the product derivation, the logic is driven by `Out` type:
  // - every field of Out should have an assigned value
  // - so we are iterating over the list of fields in Out and check the configuration for them
  // - additional fields in In can be safely ignored
  // - field by default are matched by their name
  private def matchFieldsByName(
    inData:   ProductInData,
    outData:  ProductOutData,
    settings: Settings
  ): DerivationResult[ProductGeneratorData] = outData match {
    case ProductOutData.CaseClass(caller, listOfParamsList) =>
      listOfParamsList
        .map(
          _.values
            .map { case ProductOutData.ConstructorParam(outParamName, outParamType, fallback) =>
              OutFieldLogic.resolveField(settings, inData, outParamName, None, fallback)(outParamType)
            }
            .toList
            .pipe(DerivationResult.sequence(_))
        )
        .pipe(DerivationResult.sequence(_))
        .map(ProductGeneratorData.CaseClass(caller, _))
        .logSuccess(gen => s"Case generation: $gen")

    case ProductOutData.JavaBean(defaultConstructor, setters) =>
      setters.values
        .map { case setter @ ProductOutData.Setter(outSetterName, outSetterType, _, fallback) =>
          OutFieldLogic.resolveField(settings, inData, outSetterName, None, fallback)(outSetterType).map(_ -> setter)
        }
        .toList
        .pipe(DerivationResult.sequence(_))
        .map(ProductGeneratorData.JavaBean(defaultConstructor, _))
        .logSuccess(gen => s"Case generation: $gen")
  }

  // In the product derivation, the logic is driven by `Out` type:
  // - every field of Out should have an assigned value
  // - so we are iterating over the list of fields in Out and check the configuration for them
  // - additional fields in In can be safely ignored
  // - field by default are matched by their position
  private def matchFieldsByPosition(
    inData:   ProductInData,
    outData:  ProductOutData,
    settings: Settings
  ): DerivationResult[ProductGeneratorData] = outData match {
    case ProductOutData.CaseClass(caller, listOfParamsList) =>
      listOfParamsList
        .map(
          _.values.zipWithIndex
            .map { case (ProductOutData.ConstructorParam(outParamName, outParamType, default), index) =>
              OutFieldLogic.resolveField(settings, inData, outParamName, Some(index), default)(outParamType)
            }
            .toList
            .pipe(DerivationResult.sequence(_))
        )
        .pipe(DerivationResult.sequence(_))
        .map(ProductGeneratorData.CaseClass(caller, _))
        .logSuccess(gen => s"Case generation: $gen")

    case ProductOutData.JavaBean(_, _) =>
      DerivationResult.fail(
        DerivationError.InvalidInput(
          "Conversion from tuple can only be performed into another tuple or case class"
        )
      )
  }

  // if inField <:< outField then (in, ctx) => pure(in : OutField)
  // else (in, ctx) => unlift(summon[InField, OutField], in.inField, updateContext(ctx, path)) : Result[OutField]
  private def fromFieldConstructorParam[InField: Type, OutField: Type](
    getter:   ProductInData.Getter[InField],
    settings: Settings
  ): DerivationResult[ProductGeneratorData.OutputValue] =
    if (isSubtype[InField, OutField]) {
      DerivationResult.pure(
        ProductGeneratorData.OutputValue.Pure(
          typeOf[InField],
          (in, _) => getter.get(in)
        )
      )
    } else {
      summonOrDerive[InField, OutField](settings, alwaysAllowDerivation = false).map {
        (pipe: Expr[Pipe[InField, OutField]]) =>
          ProductGeneratorData.OutputValue.Result(
            typeOf[OutField],
            (in, ctx) => unlift[InField, OutField](pipe, getter.get(in), updateContext(ctx, pathCode(getter.path)))
          )
      }
    }

  // (in, ctx) => unlift(pipe, in, ctx) : Result[OutField]
  private def fieldAddedConstructorParam[OutField: Type](
    pipe: Expr[Pipe[In, OutField]]
  ): ProductGeneratorData.OutputValue = ProductGeneratorData.OutputValue.Result(
    typeOf[OutField],
    (in, ctx) => unlift[In, OutField](pipe, in, ctx)
  )

  // (in, ctx) => unlift(summon[InField, OutField], in.used, updateContext(ctx, path)) : Result[OutField]
  private def pipeProvidedConstructorParam[InField: Type, OutField: Type](
    getter: ProductInData.Getter[InField],
    pipe:   Expr[Pipe[InField, OutField]]
  ): ProductGeneratorData.OutputValue =
    ProductGeneratorData.OutputValue.Result(
      typeOf[OutField],
      (in, ctx) => unlift[InField, OutField](pipe, getter.get(in), updateContext(ctx, pathCode(getter.path)))
    )

  type FallbackField
  private def fromFallbackValue[OutField: Type](
    outParamName: String,
    fallback:     FieldFallback[OutField],
    settings:     Settings
  ): DerivationResult[ProductGeneratorData.OutputValue] = fallback match {
    case value @ FieldFallback.Value(_, _) =>
      implicit val fallbackType: Type[FallbackField] = value.tpe.asInstanceOf[Type[FallbackField]]
      val fallbackValue = value.expr.asInstanceOf[Expr[FallbackField]]
      if (isSubtype[FallbackField, OutField])
        // (in, out) => pure(providedValue.field)
        DerivationResult
          .pure(
            ProductGeneratorData.OutputValue
              .Pure(typeOf[OutField], (_, _) => fallbackValue.asInstanceOf[Expr[OutField]])
          )
          .log(s"Successful fallback of $outParamName to provided value ${previewCode(fallbackValue)}")
      else
        // (in, out) => unlift(summon[FallbackField, OutField], providedValue.field, ctx)
        summonOrDerive[FallbackField, OutField](settings, alwaysAllowDerivation = false)
          .map { pipe =>
            ProductGeneratorData.OutputValue.Result(typeOf[OutField], (_, ctx) => unlift(pipe, fallbackValue, ctx))
          }
          .log(s"Successful fallback of $outParamName to provided value ${previewCode(fallbackValue)} with conversion")
    case FieldFallback.Default(code) if settings.isFallbackToDefaultEnabled =>
      // (in, out) => pure(Out.apply$default$n)
      DerivationResult
        .pure(
          ProductGeneratorData.OutputValue.Pure(typeOf[OutField], (_, _) => code.asInstanceOf[Expr[OutField]])
        )
        .log(s"Successful fallback of $outParamName to default value ${previewCode(code)}")
    case FieldFallback.Default(_) =>
      DerivationResult.fail(DerivationError.InvalidInput(s"Couldn't fallback on default value for $outParamName"))
    case FieldFallback.Unavailable if settings.isFallbackToDefaultEnabled =>
      DerivationResult.fail(
        DerivationError.InvalidInput(
          s"No value could be resolve for field $outParamName, although it has a default so you might try enableFallbackToDefaults option"
        )
      )
    case FieldFallback.Unavailable =>
      DerivationResult.fail(DerivationError.InvalidInput(s"Couldn't fallback on default value for $outParamName"))
  }
}
object ProductCaseGeneration {

  private val getAccessor = raw"(?i)get(.)(.*)".r
  private val isAccessor  = raw"(?i)is(.)(.*)".r
  private val dropGetIs: String => String = {
    case getAccessor(head, tail) => head.toLowerCase + tail
    case isAccessor(head, tail)  => head.toLowerCase + tail
    case other                   => other
  }
  val isGetterName: String => Boolean = name => getAccessor.matches(name) || isAccessor.matches(name)

  private val setAccessor = raw"(?i)set(.)(.*)".r
  private val dropSet: String => String = {
    case setAccessor(head, tail) => head.toLowerCase + tail
    case other                   => other
  }
  val isSetterName: String => Boolean = name => setAccessor.matches(name)

  def inputNameMatchesOutputName(
    inFieldName:     String,
    outFieldName:    String,
    caseInsensitive: Boolean
  ): Boolean = {
    val in  = Set(inFieldName, dropGetIs(inFieldName))
    val out = Set(outFieldName, dropSet(outFieldName))
    if (caseInsensitive) in.exists(a => out.exists(b => a.equalsIgnoreCase(b)))
    else in.intersect(out).nonEmpty
  }
}
