package pipez.internal

import scala.quoted.{Type as _, *}

trait PlatformProductCaseGeneration[Pipe[_, _], In, Out](using Quotes) extends ProductCaseGeneration[Pipe, In, Out] {
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
    sym.declaredMethods.exists(m => m.name.toString.startsWith("set")) &&
    sym.declarations.exists(m => m.isClassConstructor && m.paramSymss.flatten.isEmpty) // TODO: check for public?
  final def isInstantiable[A: Type]: Boolean =
    val sym = TypeRepr.of[A].typeSymbol
    !sym.flags.is(Flags.Abstract) && sym.declarations.exists(m => m.isClassConstructor) // TODO: check for public?

  final def extractProductInData(settings: Settings): DerivationResult[ProductInData] = {
    ???
  }

  final def extractProductOutData(settings: Settings): DerivationResult[ProductOutData] = {
    ???
  }

  final def generateProductCode(generatorData: ProductGeneratorData): DerivationResult[CodeOf[Pipe[In, Out]]] = {
    ???
  }
}
