package pipez.internal

import pipez.internal.Definitions.{ Context, Result }

import scala.annotation.nowarn
import scala.util.chaining.*
import scala.language.existentials

@nowarn("msg=The outer reference in this type test cannot be checked at run time.")
private[internal] trait PlatformAnyValCaseGeneration[Pipe[_, _], In, Out] extends AnyValCaseGeneration[Pipe, In, Out] {
  self: PlatformDefinitions[Pipe, In, Out] & PlatformGenerators[Pipe, In, Out] =>

  import quotes.*
  import quotes.reflect.*

  final def isAnyVal[A: Type]: Boolean = ???

  final def isPrimitive[A: Type]: Boolean = ???

  final def extractAnyValInData(settings: Settings): DerivationResult[AnyValInData[AnyValType]] = ???

  final def extractAnyValOutData(settings: Settings): DerivationResult[AnyValOutData[AnyValType]] = ???

  final def generateAnyValCode(generatorData: AnyValGeneratorData): DerivationResult[Expr[Pipe[In, Out]]] = ???
}
