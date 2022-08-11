package pipez.internal

import pipez.PipeDerivationConfig

import scala.annotation.unused

trait PlatformDefinitions[Pipe[_, _], In, Out](using quotes: scala.quoted.Quotes) extends Definitions[Pipe, In, Out] {

  import quotes.*

  override type Type[A] = scala.quoted.Type[A]

  override type Argument[@unused A] = scala.quoted.Expr[A]
  override type CodeOf[A]           = scala.quoted.Expr[A]

  final val inCode: Argument[In] => CodeOf[In] = a => a

  final def previewCode[A](code: CodeOf[A]): String = code.show

  final def summonPipe[Input, Output](
    inputType:  Type[Input],
    outputType: Type[Output]
  ): DerivationResult[CodeOf[Pipe[Input, Output]]] =
    DerivationResult.fail(DerivationError.NotYetImplemented("pipe summoning"))

  final def singleAbstractMethodExpansion[SAM](tpe: Type[SAM], code: CodeOf[SAM]): CodeOf[SAM] = '{ ??? }

  final def readConfig(code: CodeOf[PipeDerivationConfig[Pipe, In, Out]]): DerivationResult[Settings] =
    DerivationResult.fail(DerivationError.NotYetImplemented("read config"))
}
