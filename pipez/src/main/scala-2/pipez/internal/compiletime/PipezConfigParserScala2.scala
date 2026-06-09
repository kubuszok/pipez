package pipez.internal.compiletime

import hearth.MacroCommonsScala2
import pipez.PipeDerivationConfig
import scala.reflect.macros.blackbox

trait PipezConfigParserScala2 { this: PipezMacrosImpl & MacroCommonsScala2 =>

  import c.universe.{Expr as _, Type as _, *}

  def readConfig[In: Type, Out: Type](code: Expr[PipeDerivationConfig[Pipe, In, Out]]): Settings = {
    val tree = code.asInstanceOf[c.Expr[Any]].tree

    def extractPath(in: Tree): Either[String, String] = in match {
      case Block(List(defn: DefDef), _) => extractPath(defn.rhs)
      case Function(_, body)            => extractPath(body)
      case Select(_, field)             => Right(field.decodedName.toString)
      case Apply(Select(_, get), Nil)   => Right(get.decodedName.toString)
      case Ident(_)                     => Right("")
      case Typed(expr, _)               => extractPath(expr)
      case _                            => Left(s"Unsupported path expression")
    }

    def toHExpr(t: Tree): Expr[Any] = c.Expr(t)(typeOfAny.asInstanceOf[c.WeakTypeTag[Any]]).asInstanceOf[Expr[Any]]

    def toHType(tpe: c.universe.Type): ?? = {
      val tag: Type[Any] = c.WeakTypeTag(tpe).asInstanceOf[Type[Any]]
      tag.as_??
    }

    def extract(tree: Tree, acc: List[ConfigEntry]): Either[String, Settings] = tree match {
      case TypeApply(Select(_, name), _) if name.decodedName.toString == "apply" =>
        Right(new Settings(acc))
      case TypeApply(Select(Select(_, cfgName), name), _)
          if cfgName.decodedName.toString == "Config" && name.decodedName.toString == "apply" =>
        Right(new Settings(acc))
      case TypeApply(Select(Select(_, cfgName), name), _)
          if cfgName.decodedName.toString == "Config" && name.decodedName.toString == "empty" =>
        Right(new Settings(acc))
      case Select(expr, name) if name.decodedName.toString == "enableDiagnostics" =>
        extract(expr, ConfigEntry.EnableDiagnostics :: acc)
      case Apply(TypeApply(Select(expr, name), _), List(outputField, pipe))
          if name.decodedName.toString == "addField" =>
        extractPath(outputField) match {
          case Right(fieldName) =>
            extract(expr, ConfigEntry.AddField(fieldName, toHExpr(pipe)) :: acc)
          case Left(err) => Left(err)
        }
      case Apply(TypeApply(Select(expr, name), _), List(inputField, outputField))
          if name.decodedName.toString == "renameField" =>
        (extractPath(inputField), extractPath(outputField)) match {
          case (Right(inName), Right(outName)) =>
            extract(expr, ConfigEntry.RenameField(inName, outName) :: acc)
          case _ => Left("Unsupported renameField arguments")
        }
      case Apply(TypeApply(Select(expr, name), _), List(inputField, outputField, pipe))
          if name.decodedName.toString == "plugInField" =>
        (extractPath(inputField), extractPath(outputField)) match {
          case (Right(inName), Right(outName)) =>
            extract(expr, ConfigEntry.PlugInField(inName, outName, toHExpr(pipe)) :: acc)
          case _ => Left("Unsupported plugInField arguments")
        }
      case Select(expr, name) if name.decodedName.toString == "fieldMatchingCaseInsensitive" =>
        extract(expr, ConfigEntry.FieldCaseInsensitive :: acc)
      case Apply(TypeApply(Select(expr, name), List(fallbackTypeTree)), List(fallbackValue))
          if name.decodedName.toString == "addFallbackToValue" =>
        extract(
          expr,
          ConfigEntry.AddFallbackValue(toHType(fallbackTypeTree.tpe), toHExpr(fallbackValue)) :: acc
        )
      case Select(expr, name) if name.decodedName.toString == "enableFallbackToDefaults" =>
        extract(expr, ConfigEntry.EnableFallbackToDefaults :: acc)
      case Apply(TypeApply(Select(expr, name), List(inputSubtype)), List(pipe))
          if name.decodedName.toString == "removeSubtype" =>
        extract(
          expr,
          ConfigEntry.RemoveSubtype(toHType(inputSubtype.tpe), toHExpr(pipe)) :: acc
        )
      case TypeApply(Select(expr, name), List(inputSubtype, outputSubtype))
          if name.decodedName.toString == "renameSubtype" =>
        extract(
          expr,
          ConfigEntry.RenameSubtype(
            toHType(inputSubtype.tpe),
            toHType(outputSubtype.tpe)
          ) :: acc
        )
      case Apply(TypeApply(Select(expr, name), List(inputSubtype, outputSubtype)), List(pipe))
          if name.decodedName.toString == "plugInSubtype" =>
        extract(
          expr,
          ConfigEntry.PlugInSubtype(
            toHType(inputSubtype.tpe),
            toHType(outputSubtype.tpe),
            toHExpr(pipe)
          ) :: acc
        )
      case Select(expr, name) if name.decodedName.toString == "enumMatchingCaseInsensitive" =>
        extract(expr, ConfigEntry.EnumCaseInsensitive :: acc)
      case Select(expr, name) if name.decodedName.toString == "recursiveDerivation" =>
        extract(expr, ConfigEntry.EnableRecursiveDerivation :: acc)
      case _ =>
        Left(s"Unsupported PipeDerivationConfig expression")
    }

    extract(tree, Nil) match {
      case Right(settings) => settings
      case Left(err)       => throw new RuntimeException(s"Invalid configuration: $err")
    }
  }
}
