package pipez.internal.compiletime

import hearth.MacroCommonsScala3
import pipez.PipeDerivationConfig
import scala.quoted.Expr as SQExpr

trait PipezConfigParserScala3 { this: PipezMacrosImpl & PipezMacros[?, ?, ?] & MacroCommonsScala3 =>

  import quotes.*
  import quotes.reflect.*

  def readConfig[In: Type, Out: Type](code: Expr[PipeDerivationConfig[Pipe, In, Out]]): Settings = {

    def extractPath(in: Tree): Either[String, (String, Boolean)] = in match {
      case Block(List(DefDef(_, _, _, Some(term))), _) => extractPath(term)
      case Select(_, field)                            => Right((field, false))
      case Apply(Select(_, get), List())               => Right((get, false))
      case Ident(_)                                    => Right(("", true))
      case _                                           => Left(s"Unsupported path expression")
    }

    def toHExpr(tree: Tree): Expr[Any] = tree.asExpr.asInstanceOf[Expr[Any]]
    def toHType(tree: TypeTree): ?? = tree.tpe.asType.asInstanceOf[??]

    def extract(tree: Term, acc: List[ConfigEntry]): Either[String, Settings] = tree match {
      case Inlined(_, List(), expr)         => extract(expr, acc)
      case TypeApply(Select(_, "apply"), _) =>
        Right(new Settings(acc))
      case TypeApply(Select(Select(_, "Config"), "apply"), _) =>
        Right(new Settings(acc))
      case Select(expr, "enableDiagnostics") =>
        extract(expr, ConfigEntry.EnableDiagnostics :: acc)
      case Apply(TypeApply(Select(expr, "addField"), List(_)), List(outputField, pipe)) =>
        extractPath(outputField) match {
          case Right((fieldName, _)) =>
            extract(expr, ConfigEntry.AddField(fieldName, toHExpr(pipe)) :: acc)
          case Left(err) => Left(err)
        }
      case Apply(TypeApply(Select(expr, "renameField"), _), List(inputField, outputField)) =>
        (extractPath(inputField), extractPath(outputField)) match {
          case (Right((inName, _)), Right((outName, _))) =>
            extract(expr, ConfigEntry.RenameField(inName, outName) :: acc)
          case _ => Left("Unsupported renameField arguments")
        }
      case Apply(TypeApply(Select(expr, "plugInField"), _), List(inputField, outputField, pipe)) =>
        (extractPath(inputField), extractPath(outputField)) match {
          case (Right((inName, _)), Right((outName, _))) =>
            extract(expr, ConfigEntry.PlugInField(inName, outName, toHExpr(pipe)) :: acc)
          case _ => Left("Unsupported plugInField arguments")
        }
      case Select(expr, "fieldMatchingCaseInsensitive") =>
        extract(expr, ConfigEntry.FieldCaseInsensitive :: acc)
      case Apply(TypeApply(Select(expr, "addFallbackToValue"), List(fallbackType)), List(fallbackValue)) =>
        extract(
          expr,
          ConfigEntry.AddFallbackValue(toHType(fallbackType), toHExpr(fallbackValue)) :: acc
        )
      case Select(expr, "enableFallbackToDefaults") =>
        extract(expr, ConfigEntry.EnableFallbackToDefaults :: acc)
      case Apply(TypeApply(Select(expr, "removeSubtype"), List(inputSubtype)), List(pipe)) =>
        extract(
          expr,
          ConfigEntry.RemoveSubtype(toHType(inputSubtype), toHExpr(pipe)) :: acc
        )
      case TypeApply(Select(expr, "renameSubtype"), List(inputSubtype, outputSubtype)) =>
        extract(
          expr,
          ConfigEntry.RenameSubtype(toHType(inputSubtype), toHType(outputSubtype)) :: acc
        )
      case Apply(TypeApply(Select(expr, "plugInSubtype"), List(inputSubtype, outputSubtype)), List(pipe)) =>
        extract(
          expr,
          ConfigEntry.PlugInSubtype(toHType(inputSubtype), toHType(outputSubtype), toHExpr(pipe)) :: acc
        )
      case Select(expr, "enumMatchingCaseInsensitive") =>
        extract(expr, ConfigEntry.EnumCaseInsensitive :: acc)
      case Select(expr, "recursiveDerivation") =>
        extract(expr, ConfigEntry.EnableRecursiveDerivation :: acc)
      case _ =>
        Left(s"Unsupported PipeDerivationConfig expression")
    }

    val term = code.asInstanceOf[SQExpr[?]].asTerm
    extract(term, Nil) match {
      case Right(settings) => settings
      case Left(err)       => throw new RuntimeException(s"Invalid configuration: $err")
    }
  }
}
