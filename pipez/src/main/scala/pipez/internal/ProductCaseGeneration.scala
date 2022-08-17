package pipez.internal

import pipez.internal.ProductCaseGeneration.inputNameMatchesOutputName

import scala.annotation.nowarn
import scala.collection.immutable.ListMap
import scala.util.chaining.*

@nowarn("msg=The outer reference in this type test cannot be checked at run time.")
trait ProductCaseGeneration[Pipe[_, _], In, Out] { self: Definitions[Pipe, In, Out] & Generators[Pipe, In, Out] =>

  /** True iff `A` is defined as `case class` */
  def isCaseClass[A: Type]: Boolean

  /** True iff `A` is defined as `case object` */
  def isCaseObject[A: Type]: Boolean

  /** True iff `A` has a (public) default constructor and at least one (public) method starting with `set` */
  def isJavaBean[A: Type]: Boolean

  /** True iff `A` is not abstract */
  def isInstantiable[A: Type]: Boolean

  /** Whether `Out` type could be constructed as "product case" */
  final def isUsableAsProductOutput: Boolean =
    (isCaseClass[Out] || isCaseObject[Out] || isJavaBean[Out]) && isInstantiable[Out]

  /** Should create `Out` expression from the constructor arguments grouped in parameter lists */
  type Constructor = List[List[CodeOf[Any]]] => CodeOf[Out]

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
  }
  object ProductInData {

    final case class Getter[InField](
      name: String,
      tpe:  Type[InField],
      get:  Argument[In] => CodeOf[InField]
    ) {

      override def toString: String = s"Getter($name : $tpe)"
    }
  }

  /** Stores information how `Out` value could be constructed from values of constructor parameters/passed to setters */
  sealed trait ProductOutData extends Product with Serializable
  object ProductOutData {

    final case class ConstructorParam[OutField](
      name: String,
      tpe:  Type[OutField]
    )
    final case class CaseClass(
      caller: Constructor,
      params: List[ListMap[String, ConstructorParam[?]]]
    ) extends ProductOutData {
      override def toString: String = s"CaseClass${params.map { list =>
          "(" + list.map { case (n, p) => s"$n : ${p.tpe}" }.mkString(", ") + ")"
        }.mkString}"
    }

    final case class Setter[OutField](
      name: String,
      tpe:  Type[OutField],
      set:  (Argument[Out], CodeOf[OutField]) => CodeOf[Unit]
    ) {

      override def toString: String = s"Setter($name : $tpe)"
    }
    final case class JavaBean(
      defaultConstructor: CodeOf[Out],
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
      pipe: CodeOf[Pipe[In, OutField]]
    ) extends OutFieldLogic[OutField]

    final case class FieldRenamed[InField, OutField](
      inField:     String,
      inFieldType: Type[InField]
    ) extends OutFieldLogic[OutField]

    final case class PipeProvided[InField, OutField](
      inField:     String,
      inFieldType: Type[InField],
      pipe:        CodeOf[Pipe[InField, OutField]]
    ) extends OutFieldLogic[OutField]

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
          FieldAdded(pipe.asInstanceOf[CodeOf[Pipe[In, OutField]]])
        case RenameField(Field(Root, inName), In, Field(Root, outFieldGetter), Out)
            if inputNameMatchesOutputName(outFieldGetter, outFieldName, settings.isFieldCaseInsensitive) =>
          // TODO: validate that Out <:< outFieldType is correct
          FieldRenamed(inName, In)
        case PlugInField(Field(Root, inName), In, Field(Root, outFieldGetter), Out, pipe)
            if inputNameMatchesOutputName(outFieldGetter, outFieldName, settings.isFieldCaseInsensitive) =>
          // TODO: validate that Out <:< outFieldType is correct
          PipeProvided[Any, OutField](inName,
                                      In.asInstanceOf[Type[Any]],
                                      pipe.asInstanceOf[CodeOf[Pipe[Any, OutField]]]
          )
      }
    }

    def resolveField[OutField: Type](
      settings:     Settings,
      inData:       ProductInData,
      outParamName: String
    ): DerivationResult[ProductGeneratorData.OutputValue] = resolve(settings, outParamName) match {
      case DefaultField() =>
        // if inField (same name as out) not found then error
        // else if inField <:< outField then (in, ctx) => in : OutField
        // else (in, ctx) => unlift(summon[InField, OutField])(in.outParamName, ctx) : Result[OutField]
        type InField
        inData
          .findGetter(outParamName, outParamName, settings.isFieldCaseInsensitive)
          .map(_.asInstanceOf[ProductInData.Getter[InField]])
          .flatMap { getter =>
            implicit val tpe: Type[InField] = getter.tpe
            fromFieldConstructorParam[InField, OutField](getter)
          }
          .log(s"Field $outParamName uses default resolution (matching input name, summoning)")
      case FieldAdded(pipe) =>
        // (in, ctx) => unlift(pipe)(in, ctx) : Result[OutField]
        DerivationResult
          .pure(fieldAddedConstructorParam[OutField](pipe))
          .log(s"Field $outParamName considered added to output, uses provided pipe")
      case FieldRenamed(inFieldName, _) =>
        // if inField (name provided) not found then error
        // else if inField <:< outField then (in, ctx) => in : OutField
        // else (in, ctx) => unlift(summon[InField, OutField])(in.inFieldName, ctx) : Result[OutField]
        type InField
        inData
          .findGetter(inFieldName, outParamName, settings.isFieldCaseInsensitive)
          .map(_.asInstanceOf[ProductInData.Getter[InField]])
          .flatMap { getter =>
            implicit val tpe: Type[InField] = getter.tpe
            fromFieldConstructorParam[InField, OutField](getter)
          }
          .log(s"Field $outParamName is considered renamed from $inFieldName, uses summoning if types differ")
      case PipeProvided(inFieldName, _, pipe) =>
        // if inField (name provided) not found then error
        // else (in, ctx) => unlift(summon[InField, OutField])(in.used, ctx) : Result[OutField]
        type InField
        inData
          .findGetter(inFieldName, outParamName, settings.isFieldCaseInsensitive)
          .map(_.asInstanceOf[ProductInData.Getter[InField]])
          .map { getter =>
            implicit val tpe: Type[InField] = getter.tpe
            pipeProvidedConstructorParam[InField, OutField](getter, pipe.asInstanceOf[CodeOf[Pipe[InField, OutField]]])
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
        caller: (Argument[In], Argument[Context]) => CodeOf[A]
      ) extends OutputValue {
        override def toString: String = s"Pure { ($In, Context) => $tpe }"
      }

      final case class Result[A](
        tpe:    Type[A],
        caller: (Argument[In], Argument[Context]) => CodeOf[self.Result[A]]
      ) extends OutputValue {
        override def toString: String = s"Result { ($In, Context) => $tpe }"
      }
    }

    final case class CaseClass(
      caller: Constructor,
      output: List[List[OutputValue]]
    ) extends ProductGeneratorData {
      override def toString: String = s"CaseClass${output.map(list => "(" + list.mkString(", ") + ")").mkString}"
    }

    final case class JavaBean(
      caller: CodeOf[Out],
      output: List[(OutputValue, ProductOutData.Setter[?])]
    ) extends ProductGeneratorData {
      override def toString: String = s"JavaBean(${output.mkString(", ")})"
    }
  }

  object ProductTypeConversion {

    final def unapply(settings: Settings): Option[DerivationResult[CodeOf[Pipe[In, Out]]]] =
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
    * TODO: update after implementing Paths: ctx -> updateContext(ctx, path)
    *
    * For case class output should generate code like:
    *
    * {{{
    * pipeDerivation.lift { (in: In, ctx: pipeDerivation.Context) =>
    *   pipeDerivation.mergeResult(
    *     pipeDerivation.mergeResult(
    *       pipeDerivation.pure(Array[Any](2)),
    *       pipeDerivation.unlift(fooPipe, in.foo, ctx)
    *     ) { (left, right) =>
    *       left(0) = right
    *       left
    *     },
    *     pipeDerivation.unlift(barPipe, in.bar, ctx)
    *   ) { (left, right) =>
    *     left(1) = right
    *     new Out(
    *       foo = left(0).asInstanceOf[Foo2],
    *       bar = left(1).asInstanceOf[Bar2],
    *     )
    *   }
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
    *     pipeDerivation.mergeResult(
    *       pipeDerivation.pure {
    *         val result = new Out()
    *         result
    *       },
    *       pipeDerivation.unlift(fooPipe, in.foo, ctx)
    *     ) { (left, right) =>
    *       left.setFoo(right)
    *       left
    *     },
    *     pipeDerivation.unlift(barPipe, in.bar, ctx)
    *   ) { (left, right) =>
    *     left.setBar(right)
    *     left
    *   }
    * }
    * }}}
    */
  def generateProductCode(generatorData: ProductGeneratorData): DerivationResult[CodeOf[Pipe[In, Out]]]

  private def attemptProductRendering(settings: Settings): DerivationResult[CodeOf[Pipe[In, Out]]] =
    for {
      data <- extractProductInData(settings) zip extractProductOutData(settings).logSuccess(data => s"Resolved Java Bean output: $data")
      (inData, outData) = data
      generatorData <- matchFields(inData, outData, settings)
      code <- generateProductCode(generatorData)
    } yield code

  // In the product derivation, the logic is driven by `Out` type:
  // - every field of Out should have an assigned value
  // - so we are iterating over the list of fields in Out and check the configuration for them
  // - additional fields in In can be safely ignored
  private def matchFields(
    inData:   ProductInData,
    outData:  ProductOutData,
    settings: Settings
  ): DerivationResult[ProductGeneratorData] = outData match {
    case ProductOutData.CaseClass(caller, listOfParamsList) =>
      listOfParamsList
        .map(
          _.values
            .map { case ProductOutData.ConstructorParam(outParamName, outParamType) =>
              OutFieldLogic.resolveField(settings, inData, outParamName)(outParamType)
            }
            .toList
            .pipe(DerivationResult.sequence(_))
        )
        .pipe(DerivationResult.sequence(_))
        .map(ProductGeneratorData.CaseClass(caller, _))

    case ProductOutData.JavaBean(defaultConstructor, setters) =>
      setters.values
        .map { case setter @ ProductOutData.Setter(outSetterName, outSetterType, _) =>
          OutFieldLogic.resolveField(settings, inData, outSetterName)(outSetterType).map(_ -> setter)
        }
        .toList
        .pipe(DerivationResult.sequence(_))
        .map(ProductGeneratorData.JavaBean(defaultConstructor, _))
  }

  // if inField <:< outField then (in, ctx) => in : OutField
  // else (in, ctx) => unlift(summon[InField, OutField])(in.inField, ctx) : Result[OutField]
  private def fromFieldConstructorParam[InField: Type, OutField: Type](
    getter: ProductInData.Getter[InField]
  ): DerivationResult[ProductGeneratorData.OutputValue] =
    if (isSubtype[InField, OutField]) {
      DerivationResult.pure(
        ProductGeneratorData.OutputValue.Pure(
          typeOf[InField],
          (in, _) => getter.get(in)
        )
      )
    } else {
      summonPipe[InField, OutField].map { (pipe: CodeOf[Pipe[InField, OutField]]) =>
        ProductGeneratorData.OutputValue.Result(
          typeOf[OutField],
          (in, ctx) => unlift[InField, OutField](pipe, getter.get(in), ctxCode(ctx))
        )
      }
    }

  // (in, ctx) => unlift(pipe)(in, ctx) : Result[OutField]
  private def fieldAddedConstructorParam[OutField: Type](
    pipe: CodeOf[Pipe[In, OutField]]
  ): ProductGeneratorData.OutputValue = ProductGeneratorData.OutputValue.Result(
    typeOf[OutField],
    (in, ctx) => unlift[In, OutField](pipe, inCode(in), ctxCode(ctx))
  )

  // (in, ctx) => unlift(summon[InField, OutField])(in.used, ctx) : Result[OutField]
  private def pipeProvidedConstructorParam[InField: Type, OutField: Type](
    getter: ProductInData.Getter[InField],
    pipe:   CodeOf[Pipe[InField, OutField]]
  ): ProductGeneratorData.OutputValue =
    ProductGeneratorData.OutputValue.Result(
      typeOf[OutField],
      (in, ctx) => unlift[InField, OutField](pipe, getter.get(in), ctxCode(ctx))
    )
}
object ProductCaseGeneration {

  private val getAccessor = raw"get(.)(.*)".r
  private val isAccessor  = raw"is(.)(.*)".r
  private val dropGetIs: String => String = {
    case getAccessor(head, tail) => head.toLowerCase + tail
    case isAccessor(head, tail)  => head.toLowerCase + tail
    case other                   => other
  }

  private val setAccessor = raw"set(.)(.*)".r
  private val dropSet: String => String = {
    case setAccessor(head, tail) => head.toLowerCase + tail
    case other                   => other
  }

  private def inputNameMatchesOutputName(
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
