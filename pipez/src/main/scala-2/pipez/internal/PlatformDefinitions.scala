package pipez.internal

import pipez.PipeDerivationConfig

import scala.reflect.macros.blackbox

trait PlatformDefinitions[Pipe[_, _], In, Out] extends Definitions[Pipe, In, Out] {

  val c: blackbox.Context

  import c.universe._

  override type Type[A] = c.Type

  override type Argument[A] = TermName
  override type CodeOf[A]   = Expr[A]

  final val inCode: Argument[In] => CodeOf[In] =
    id => c.Expr[In](q"$id")

  final def previewCode[A](code: c.universe.Expr[A]): String = showCode(code.tree)

  final def summonPipe[InField, OutField](
    inType:  Type[InField],
    outType: Type[OutField]
  ): DerivationResult[CodeOf[Pipe[InField, OutField]]] =
    scala.util
      .Try(c.Expr[Pipe[InField, OutField]](c.inferImplicitValue(pipeType(inType, outType))))
      .fold(
        _ => DerivationResult.fail(DerivationError.RequiredImplicitNotFound(inType, outType)),
        va => DerivationResult.pure(va)
      )
      .logSuccess(i => s"Summoned implicit value: ${previewCode(i)}")

  final def readConfig(code: CodeOf[PipeDerivationConfig[Pipe, In, Out]]): DerivationResult[Settings] =
    DerivationResult.fail(DerivationError.NotYetImplemented("readConfig macro"))
}
