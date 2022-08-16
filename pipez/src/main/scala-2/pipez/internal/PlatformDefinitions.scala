package pipez.internal

import pipez.PipeDerivationConfig

import scala.annotation.{ nowarn, unused }
import scala.reflect.macros.blackbox

trait PlatformDefinitions[Pipe[_, _], In, Out] extends Definitions[Pipe, In, Out] {

  val c: blackbox.Context

  import c.universe.*

  override type Type[@unused A] = c.Type

  override type Argument[@unused A] = TermName
  override type CodeOf[A]           = Expr[A]

  final val inCode:  Argument[In] => CodeOf[In]                             = id => c.Expr[In](q"$id")
  final val ctxCode: Argument[Context] => CodeOf[Context] = id => c.Expr[Context](q"$id")

  final def previewCode[A](code: CodeOf[A]): String = showCode(code.tree)

  final def summonPipe[Input: Type, Output: Type]: DerivationResult[CodeOf[Pipe[Input, Output]]] =
    DerivationResult
      .unsafe(c.Expr[Pipe[Input, Output]](c.inferImplicitValue(PipeOf[Input, Output], silent = false)))(_ =>
        DerivationError.RequiredImplicitNotFound(typeOf[Input], typeOf[Output])
      )
      .logSuccess(i => s"Summoned implicit value: ${previewCode(i)}")

  final def singleAbstractMethodExpansion[SAM: Type](code: CodeOf[SAM]): CodeOf[SAM] =
    c.Expr(q"_root_.scala.Predef.identity[${typeOf[SAM]}]($code)")

  final def readConfig(code: CodeOf[PipeDerivationConfig[Pipe, In, Out]]): DerivationResult[Settings] = {
    @nowarn("cat=unused")
    def extractPath(in: Tree): Either[String, Path] = in match {
      case Function(_, expr)                          => extractPath(expr)
      case Select(expr, TermName(field))              => extractPath(expr).map(Path.Field(_, field)) // extract .field
      case Apply(Select(expr, TermName(get)), List()) => extractPath(expr).map(Path.Field(_, get)) // extract .getField
      case Ident(TermName(_))                         => Right(Path.Root) // drop argName from before .field
      case tt: TypeTree => Right(Path.Subtype(Path.Root, tt.tpe))
      case _ => Left(s"Path ${showCode(in)} is not in format _.field1.field2")
    }

    def extract(tree: Tree, acc: List[ConfigEntry]): Either[String, Settings] = tree match {
      // matches PipeDerivationConfig[Pipe, In, Out]
      case TypeApply(Select(Ident(cfg), TermName("apply")), _) if cfg.decodedName.toString == "PipeDerivationConfig" =>
        Right(new Settings(acc))
      // matches PipeCompanion.Config[In, Out]
      case TypeApply(Select(Select(Ident(_), cfg), TermName("apply")), _) if cfg.decodedName.toString == "Config" =>
        Right(new Settings(acc))
      // matches {cfg}.enableDiagnostics
      case Select(expr, TermName("enableDiagnostics")) =>
        extract(expr, ConfigEntry.EnableDiagnostics :: acc)
      // matches {cfg}.addField(_.in, pipe)
      case Apply(TypeApply(Select(expr, TermName("addField")), _), List(outputField, pipe)) =>
        for {
          outFieldPath <- extractPath(outputField)
          TypeRef(_, _, List(_, outFieldType)) = outputField.tpe
          result <- extract(
            expr,
            ConfigEntry.AddField(outFieldPath,
                                 outFieldType,
                                 singleAbstractMethodExpansion(c.Expr(pipe))(PipeOf[In, Any](In, outFieldType))
            ) :: acc
          )
        } yield result
      // matches {cfg}.renameField(_.in, _.out)
      case Apply(TypeApply(Select(expr, TermName("renameField")), _), List(inputField, outputField)) =>
        for {
          inPath <- extractPath(inputField)
          outPath <- extractPath(outputField)
          result <- extract(
            expr,
            ConfigEntry.RenameField(inPath, inputField.tpe.resultType, outPath, outputField.tpe.resultType) :: acc
          )
        } yield result
      // matches {cfg}.plugIn(_.in, _.out, pipe)
      case Apply(TypeApply(Select(expr, TermName("plugInField")), _), List(inputField, outputField, pipe)) =>
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
              singleAbstractMethodExpansion(c.Expr(pipe))(PipeOf[Any, Any](inFieldType, outFieldType))
            ) :: acc
          )
        } yield result
      // matches {cfg}.fieldMatchingCaseInsensitive
      case Select(expr, TermName("fieldMatchingCaseInsensitive")) =>
        extract(expr, ConfigEntry.FieldCaseInsensitive :: acc)
      // matches {cfg}.removeSubtype[InSubtype](pipe)
      case Apply(TypeApply(Select(expr, TermName("removeSubtype")), List(inputSubtype)), List(pipe)) =>
        for {
          inputSubtypePath <- extractPath(inputSubtype)
          inputSubtypeType = inputSubtype.tpe
          result <- extract(
            expr,
            ConfigEntry.RemoveSubtype(
              inputSubtypePath,
              inputSubtypeType,
              singleAbstractMethodExpansion(c.Expr(pipe))(PipeOf[In, Out](inputSubtypeType, Out))
            ) :: acc
          )
        } yield result
      // matches {cfg}.renameSubtype[InSubtype, OutSubtype]
      case TypeApply(Select(expr, TermName("renameSubtype")), List(inputSubtype, outputSubtype)) =>
        for {
          inputSubtypePath <- extractPath(inputSubtype)
          inputSubtypeType = inputSubtype.tpe
          outputSubtypePath <- extractPath(outputSubtype)
          outputSubtypeType = outputSubtype.tpe
          result <- extract(
            expr,
            ConfigEntry.RenameSubtype(
              inputSubtypePath,
              inputSubtypeType,
              outputSubtypePath,
              outputSubtypeType
            ) :: acc
          )
        } yield result
      // matches {cfg}.pipeInSubtype[InSubtype, OutSubtype](pipe)
      case Apply(TypeApply(Select(expr, TermName("plugInSubtype")), List(inputSubtype, outputSubtype)), List(pipe)) =>
        for {
          inputSubtypePath <- extractPath(inputSubtype)
          inputSubtypeType = inputSubtype.tpe
          outputSubtypePath <- extractPath(outputSubtype)
          outputSubtypeType = outputSubtype.tpe
          result <- extract(
            expr,
            ConfigEntry.PlugInSubtype(
              inputSubtypePath,
              inputSubtypeType,
              outputSubtypePath,
              outputSubtypeType,
              singleAbstractMethodExpansion(c.Expr(pipe))(PipeOf[In, Out](inputSubtypeType, outputSubtypeType))
            ) :: acc
          )
        } yield result
      // matches {cfg}.enumMatchingCaseInsensitive
      case Select(expr, TermName("enumMatchingCaseInsensitive")) =>
        extract(expr, ConfigEntry.EnumCaseInsensitive :: acc)
      case els =>
        Left(s"${previewCode(code)} is not a right PipeDerivationConfig")
    }

    DerivationResult.fromEither(extract(code.tree, Nil))(DerivationError.InvalidConfiguration(_))
  }
}
