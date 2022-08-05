package pipez.internal

import pipez.PipeDerivationConfig

import scala.annotation.nowarn
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
    DerivationResult
      .unsafe(c.Expr[Pipe[InField, OutField]](c.inferImplicitValue(pipeType(inType, outType), silent = false)))(_ =>
        DerivationError.RequiredImplicitNotFound(inType, outType)
      )
      .logSuccess(i => s"Summoned implicit value: ${previewCode(i)}")

  final def singleAbstractMethodExpansion[SAM](tpe: Type[SAM], code: CodeOf[SAM]): CodeOf[SAM] =
    c.Expr(q"_root_.scala.Predef.identity[$tpe]($code)")

  final def readConfig(code: CodeOf[PipeDerivationConfig[Pipe, In, Out]]): DerivationResult[Settings] = {
    @nowarn("cat=unused")
    def extractPath(in: Tree): Either[String, Path] = in match {
      case Function(_, expr)             => extractPath(expr)
      case Select(expr, TermName(field)) => extractPath(expr).map(Path.Field(_, field)) // extract .field
      case Ident(TermName(_))            => Right(Path.Root) // drop argName from before .field
      case els                           => Left(s"Path ${showRaw(in)} is not in format _.field1.field2")
    }

    def extract(tree: Tree, acc: List[ConfigEntry]): Either[String, Settings] = tree match {
      // matches PipeDerivationConfig[Pipe, In, Out]
      case TypeApply(Select(Ident(cfg), TermName("apply")), _) if cfg.decodedName.toString == "PipeDerivationConfig" =>
        Right(new Settings(acc))
      // matches {cfg}.fieldMatchingCaseInsensitive
      case Select(expr, TermName("enableDiagnostics")) =>
        extract(expr, ConfigEntry.EnableDiagnostics :: acc)
      // matches {cfg}.addField(in, out)
      case Apply(TypeApply(Select(expr, TermName("addField")), _), List(outputField, pipe)) =>
        for {
          outFieldPath <- extractPath(outputField)
          TypeRef(_, _, List(_, outFieldType)) = outputField.tpe
          result <- extract(
            expr,
            ConfigEntry.AddField(outFieldPath,
                                 outFieldType,
                                 singleAbstractMethodExpansion(pipeType[In, Any](inType, outFieldType), c.Expr(pipe))
            ) :: acc
          )
        } yield result
      // matches {cfg}.renameField(in, out)
      case Apply(TypeApply(Select(expr, TermName("renameField")), _), List(inputField, outputField)) =>
        for {
          inPath <- extractPath(inputField)
          outPath <- extractPath(outputField)
          result <- extract(
            expr,
            ConfigEntry.RenameField(inPath, inputField.tpe.resultType, outPath, outputField.tpe.resultType) :: acc
          )
        } yield result
      // TODO: removeSubtype
      // TODO: renameSubtype
      // matches {cfg}.plugIn(in, out)
      case Apply(TypeApply(Select(expr, TermName("plugIn")), _), List(inputField, outputField, pipe)) =>
        for {
          inFieldPath <- extractPath(inputField)
          outFieldPath <- extractPath(outputField)
          TypeRef(_, _, List(_, inFieldType))  = inputField.tpe
          TypeRef(_, _, List(_, outFieldType)) = outputField.tpe
          result <- extract(
            expr,
            ConfigEntry.PlugInField(
              inFieldPath,
              inFieldType,
              outFieldPath,
              outFieldType,
              singleAbstractMethodExpansion(pipeType[Any, Any](inFieldType, outFieldType), c.Expr(pipe))
            ) :: acc
          )
        } yield result
      // matches {cfg}.fieldMatchingCaseInsensitive
      case Select(expr, TermName("fieldMatchingCaseInsensitive")) =>
        extract(expr, ConfigEntry.FieldCaseInsensitive :: acc)
      case els =>
        Left(s"${previewCode(code)} is not a right PipeDerivationConfig")
    }

    DerivationResult.fromEither(extract(code.tree, Nil))(DerivationError.InvalidConfiguration(_))
  }
}
