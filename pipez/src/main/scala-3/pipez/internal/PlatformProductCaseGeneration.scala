package pipez.internal

import scala.collection.immutable.ListMap
import scala.util.chaining.*
import scala.quoted.{ Type as _, * }

@scala.annotation.experimental // due to Quotes.reflect.Symbol.typeRef usage
trait PlatformProductCaseGeneration[Pipe[_, _], In, Out] extends ProductCaseGeneration[Pipe, In, Out] {
  self: PlatformDefinitions[Pipe, In, Out] & PlatformGenerators[Pipe, In, Out] =>

  import quotes.*
  import quotes.reflect.*

  final def isCaseClass[A: Type]: Boolean =
    val sym = TypeRepr.of[A].typeSymbol
    sym.isClassDef && sym.flags.is(Flags.Case)
  final def isCaseObject[A: Type]: Boolean =
    TypeRepr.of[A].typeSymbol.flags.is(Flags.Module) && isCaseClass[A]
  final def isJavaBean[A: Type]: Boolean =
    val sym = TypeRepr.of[A].typeSymbol
    sym.isClassDef &&
    sym.declaredMethods.exists(m => m.name.toLowerCase.startsWith("set")) &&
    sym.declarations.exists(m => m.isClassConstructor && m.paramSymss.flatten.isEmpty) // TODO: check for public?
  final def isInstantiable[A: Type]: Boolean =
    val sym = TypeRepr.of[A].typeSymbol
    !sym.flags.is(Flags.Abstract) && sym.declarations.exists(m => m.isClassConstructor) // TODO: check for public?

  final def extractProductInData(settings: Settings): DerivationResult[ProductInData] = {
    val sym = TypeRepr.of[In].typeSymbol
    (sym.caseFields ++ sym.declaredMethods.filter { m =>
      val n = m.name.toLowerCase
      n.startsWith("is") || n.startsWith("get")
    }).map { method =>
      method.name.toString -> ProductInData.Getter[Any](
        name = method.name.toString,
        tpe = method.typeRef.qualifier.asType.asInstanceOf[Type[Any]],
        get =
          if (method.paramSymss.isEmpty)
            (in: Argument[In]) => in.select(method).appliedToNone.asExpr // TODO: or Argss(Nil) ?
          else (in: Argument[In]) => in.select(method).appliedToNone.asExpr
      )
    }.to(ListMap)
      .pipe(ProductInData(_))
      .pipe(DerivationResult.pure)
      .logSuccess(data => s"Resolved inputs: $data")
  }

  final def extractProductOutData(settings: Settings): DerivationResult[ProductOutData] =
    if (isJavaBean[Out]) {
      // Java Bean case

      val sym = TypeRepr.of[Out].typeSymbol

      val defaultConstructor = DerivationResult.fromOption(
        sym.declarations.collectFirst {
          case member if member.isClassConstructor && member.paramSymss.flatten.isEmpty =>
            New(TypeTree.of[Out]).appliedToNone.asExpr.asExprOf[Out]
        }
      )(DerivationError.MissingPublicConstructor)

      val setters = sym.declaredMethods
        .collect {
          case method if method.name.toLowerCase.startsWith("set") && method.paramSymss.flatten.size == 1 =>
            method.name -> ProductOutData.Setter[Any](
              name = method.name.toString,
              tpe = method.typeRef.qualifier.asType.asInstanceOf[Type[Any]],
              set = (out: Argument[Out], value: CodeOf[Any]) =>
                out.select(method).appliedTo(value.asTerm).asExpr.asExprOf[Unit]
            )
        }
        .to(ListMap)
        .pipe(DerivationResult.pure(_))

      defaultConstructor
        .map2(setters)(ProductOutData.JavaBean(_, _))
        .logSuccess(data => s"Resolved Java Bean output: $data")
    } else if (isCaseObject[Out]) {
      // case object case

      ProductOutData
        .CaseClass(
          params => Ref(TypeRepr.of[Out].typeSymbol).asExpr.asExprOf[Out],
          List.empty
        )
        .pipe(DerivationResult.pure(_))
        .logSuccess(data => s"Resolved case object output: $data")
    } else {
      // case class case

      val sym = TypeRepr.of[Out].typeSymbol

      sym.declaredMethods
        .collectFirst {
          case method if method.isClassConstructor =>
            ProductOutData.CaseClass(
              params => New(TypeTree.of[Out]).appliedToArgss(params.map(_.map(_.asTerm))).asExpr.asExprOf[Out],
              method.paramSymss.map { params =>
                params
                  .map { param =>
                    param.name.toString -> ProductOutData.ConstructorParam(
                      name = param.name,
                      tpe = param.typeRef.qualifier.asType.asInstanceOf[Type[Any]]
                    )
                  }
                  .to(ListMap)
              }
            )
        }
        .get
        .pipe(DerivationResult.pure(_))
        .logSuccess(data => s"Resolved case class output: $data")
    }

  final def generateProductCode(generatorData: ProductGeneratorData): DerivationResult[CodeOf[Pipe[In, Out]]] =
    generatorData match {
      case ProductGeneratorData.CaseClass(caller, results)            => generateCaseClass(caller, results)
      case ProductGeneratorData.JavaBean(defaultConstructor, results) => generateJavaBean(defaultConstructor, results)
    }

  private def generateCaseClass(
    constructor:          Constructor,
    outputParameterLists: List[List[ProductGeneratorData.OutputValue]]
  ) = DerivationResult.fail(DerivationError.NotYetImplemented("Generate Case Class"))

  private def generateJavaBean(
    defaultConstructor: CodeOf[Out],
    outputSettersList:  List[(ProductGeneratorData.OutputValue, ProductOutData.Setter[?])]
  ) = DerivationResult.fail(DerivationError.NotYetImplemented("Generate Java Bean"))
}
