package pipez.internal

import pipez.{ PipeDerivation, PipeDerivationConfig }

import scala.annotation.{ nowarn, unused }
import scala.quoted.{ Type as _, * }

@scala.annotation.experimental // due to Quotes.reflect.Symbol.typeRef usage
trait PlatformDefinitions[Pipe[_, _], In, Out](using val quotes: Quotes) extends Definitions[Pipe, In, Out] {

  import quotes.*
  import quotes.reflect.*

  override type Type[A]   = scala.quoted.Type[A]
  override type CodeOf[A] = Expr[A]

  final def previewCode[A](code: CodeOf[A]): String = code.show

  final def summonPipe[Input: Type, Output: Type]: DerivationResult[CodeOf[Pipe[Input, Output]]] =
    DerivationResult
      .fromOption(scala.quoted.Expr.summon[Pipe[Input, Output]])(
        DerivationError.RequiredImplicitNotFound(typeOf[Input], typeOf[Output])
      )
      .logSuccess(i => s"Summoned implicit value: ${previewCode(i)}")

  final def singleAbstractMethodExpansion[SAM: Type](code: CodeOf[SAM]): CodeOf[SAM] =
    val SAM = typeOf[SAM]
    '{ scala.Predef.identity[SAM.Underlying](${ code }) }

  final def readConfig(code: CodeOf[PipeDerivationConfig[Pipe, In, Out]]): DerivationResult[Settings] =
    DerivationResult.fail(DerivationError.NotYetImplemented("read config"))

  // Scala 3-macro specific instances, required because code-generation needs these types

  implicit val Pipe:         scala.quoted.Type[Pipe]
  implicit val Context: scala.quoted.Type[Context]
  implicit val Result:  scala.quoted.Type[Result]
}
