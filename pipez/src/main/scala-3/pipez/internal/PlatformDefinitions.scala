package pipez.internal

import pipez.PipeDerivationConfig

import scala.quoted.{Type as _, *}

trait PlatformDefinitions[Pipe[_, _], In, Out](using Quotes) extends Definitions[Pipe, In, Out] {

  import quotes.*
  import quotes.reflect.*

  override type Type[A] = scala.quoted.Type[A]

  override type Argument[A] = Expr[A] // TODO: TermName or sth
  override type CodeOf[A]   = Expr[A]

  // Scala 3 macro specific
  implicit val pipeTypeConstructor: scala.quoted.Type[Pipe]
  implicit val contextType:         scala.quoted.Type[ArbitraryContext]
  implicit val resultType:          scala.quoted.Type[ArbitraryResult]

  final val inCode: Argument[In] => CodeOf[In] = a => a

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
