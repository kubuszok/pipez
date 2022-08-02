package pipez.internal

import pipez.PipeDerivationConfig

import scala.reflect.macros.blackbox

trait PlatformDefinitions[Pipe[_, _], In, Out] extends Definitions[Pipe, In, Out] {

  val c: blackbox.Context

  import c.universe._

  override type Type[A] = c.Type

  override type Argument[A] = Ident
  override type CodeOf[A]   = Expr[A]

  final val inCode: Argument[In] => CodeOf[In] =
    id => c.Expr[In](id)

  final def summonPipe[InField, OutField](
    inType:  Type[InField],
    outType: Type[OutField]
  ): DerivationResult[CodeOf[Pipe[InField, OutField]]] =
    DerivationResult.fail(DerivationError.NotYetSupported)

  final def readConfig(code: CodeOf[PipeDerivationConfig[Pipe, In, Out]]): DerivationResult[Settings] =
    DerivationResult.fail(DerivationError.NotYetSupported)
}
