package pipez.internal

import pipez.PipeDerivationConfig

import scala.reflect.macros.blackbox

trait PlatformDefinitions extends Definitions {

  val c: blackbox.Context

  override type Type = c.Type

  override type Code      = c.Tree
  override type CodeOf[A] = c.Expr[A]

  override type Field    = c.Tree
  override type Subtype  = c.Tree
  override type KeyValue = c.Tree

  override def readConfig[Pipe[_, _], In, Out](
    code: CodeOf[PipeDerivationConfig[Pipe, In, Out]]
  ): DerivationResult[Settings] =
    DerivationResult.Failure(List(DerivationError.InvalidConfiguration("WOLOLO"))) // TODO
}
