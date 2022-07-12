package pipez.internal

import pipez.PipeDerivationConfig

import scala.reflect.macros.blackbox

trait PlatformDefinitions extends Definitions {

  val c: blackbox.Context

  import c.universe._

  override type Type[A] = c.Type

  override type Code      = Tree
  override type CodeOf[A] = Expr[A]

  override type Argument = Ident
  override type Field    = Tree
  override type Subtype  = c.Type
  override type KeyValue = Tree

  override def readConfig[Pipe[_, _], In, Out](
    code: CodeOf[PipeDerivationConfig[Pipe, In, Out]]
  ): DerivationResult[Settings] =
    DerivationResult.fail(DerivationError.NotYetSupported)
}
