package pipez.internal

import pipez.PipeDerivationConfig

import scala.annotation.{ nowarn, unused }
import scala.quoted.{ Type as _, * }

trait PlatformDefinitions[Pipe[_, _], In, Out](using val quotes: Quotes) extends Definitions[Pipe, In, Out] {

  import quotes.*
  import quotes.reflect.*

  override type Type[A] = scala.quoted.Type[A]

  override type Argument[@unused A] = Term
  override type CodeOf[A]           = Expr[A]

  // Scala 3 macro specific
  implicit val pipeTypeConstructor: scala.quoted.Type[Pipe]
  implicit val contextType:         scala.quoted.Type[ArbitraryContext]
  implicit val resultType:          scala.quoted.Type[ArbitraryResult]

  final val inCode:  Argument[In] => CodeOf[In]                             = _.asExpr.asExprOf[In]
  final val ctxCode: Argument[ArbitraryContext] => CodeOf[ArbitraryContext] = _.asExpr.asExprOf[ArbitraryContext]

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
}
