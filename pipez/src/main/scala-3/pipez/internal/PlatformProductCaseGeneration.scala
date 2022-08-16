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
    sym.declaredMethods.exists(m => m.name.startsWith("set")) &&
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
    ???

  final def generateProductCode(generatorData: ProductGeneratorData): DerivationResult[CodeOf[Pipe[In, Out]]] =
    ???
}
