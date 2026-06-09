package pipez.internal.compiletime

import hearth.*
import hearth.std.*
import pipez.PipeDerivationConfig

trait PipezConfigParserShared { this: PipezMacrosImpl & MacroCommons & StdExtensions =>

  def readConfig[In: Type, Out: Type](code: Expr[PipeDerivationConfig[Pipe, In, Out]]): Settings = {
    val parsed = DestructuredExpr.parseUntyped(code.asUntyped)
    extract(parsed, Nil) match {
      case Right(settings) => settings
      case Left(err)       => throw new RuntimeException(s"Invalid configuration: $err")
    }
  }

  private def extractFieldName(expr: DestructuredExpr): Either[String, String] = expr match {
    case lam: DestructuredExpr.Lambda =>
      lam.params match {
        case List(param) =>
          extractFieldStep(lam.body, param)
        case _ => Left(s"Expected single-param lambda, got ${lam.params.size} params")
      }
    case _ => Left(s"Expected lambda, got ${expr.plainPrint}")
  }

  private def extractFieldStep(expr: DestructuredExpr, root: DestructuredExpr.Lambda.Param): Either[String, String] =
    expr match {
      case mc: DestructuredExpr.MethodCall =>
        val instanceOpt = mc.applied.collectFirst { case ai: DestructuredExpr.MethodCall.AppliedInstance => ai.value }
        instanceOpt match {
          case Some(instance) =>
            instance match {
              case ref: DestructuredExpr.Lambda.ParamRef if ref.param eq root =>
                Right(mc.method.name)
              case nested =>
                extractFieldStep(nested, root).map(_ => mc.method.name)
            }
          case None => Left(s"Expected field access, got ${expr.plainPrint}")
        }
      case ref: DestructuredExpr.Lambda.ParamRef if ref.param eq root =>
        Left("Empty field path")
      case _ => Left(s"Expected field access on lambda param, got ${expr.plainPrint}")
    }

  private def extract(expr: DestructuredExpr, acc: List[ConfigEntry]): Either[String, Settings] = expr match {
    case mc: DestructuredExpr.MethodCall =>
      val methodName = mc.method.name

      val instanceOpt = mc.applied.collectFirst { case ai: DestructuredExpr.MethodCall.AppliedInstance => ai.value }
      val typeArgsOpt = mc.applied.collectFirst { case at: DestructuredExpr.MethodCall.AppliedTypes => at.typeArgs }
      val valueArgsOpt = mc.applied.collectFirst { case av: DestructuredExpr.MethodCall.AppliedValues => av.args }

      methodName match {
        case "apply" =>
          Right(new Settings(acc))

        case "enableDiagnostics" =>
          instanceOpt match {
            case Some(inner) => extract(inner, ConfigEntry.EnableDiagnostics :: acc)
            case None        => Left("enableDiagnostics: missing instance")
          }

        case "addField" =>
          (instanceOpt, valueArgsOpt) match {
            case (Some(inner), Some(List(outputField, pipe))) =>
              extractFieldName(outputField).flatMap { fieldName =>
                extract(inner, ConfigEntry.AddField(fieldName, pipe.toUntypedExpr.asTyped[Any]) :: acc)
              }
            case _ => Left(s"addField: unexpected args")
          }

        case "renameField" =>
          (instanceOpt, valueArgsOpt) match {
            case (Some(inner), Some(List(inputField, outputField))) =>
              for {
                inName <- extractFieldName(inputField)
                outName <- extractFieldName(outputField)
                result <- extract(inner, ConfigEntry.RenameField(inName, outName) :: acc)
              } yield result
            case _ => Left(s"renameField: unexpected args")
          }

        case "plugInField" =>
          (instanceOpt, valueArgsOpt) match {
            case (Some(inner), Some(List(inputField, outputField, pipe))) =>
              for {
                inName <- extractFieldName(inputField)
                outName <- extractFieldName(outputField)
                result <- extract(
                  inner,
                  ConfigEntry.PlugInField(inName, outName, pipe.toUntypedExpr.asTyped[Any]) :: acc
                )
              } yield result
            case _ => Left(s"plugInField: unexpected args")
          }

        case "fieldMatchingCaseInsensitive" =>
          instanceOpt match {
            case Some(inner) => extract(inner, ConfigEntry.FieldCaseInsensitive :: acc)
            case None        => Left("fieldMatchingCaseInsensitive: missing instance")
          }

        case "addFallbackToValue" =>
          (instanceOpt, typeArgsOpt, valueArgsOpt) match {
            case (Some(inner), Some(List(fvType)), Some(List(fallbackValue))) =>
              extract(
                inner,
                ConfigEntry.AddFallbackValue(fvType, fallbackValue.toUntypedExpr.asTyped[Any]) :: acc
              )
            case _ => Left(s"addFallbackToValue: unexpected args")
          }

        case "enableFallbackToDefaults" =>
          instanceOpt match {
            case Some(inner) => extract(inner, ConfigEntry.EnableFallbackToDefaults :: acc)
            case None        => Left("enableFallbackToDefaults: missing instance")
          }

        case "removeSubtype" =>
          (instanceOpt, typeArgsOpt, valueArgsOpt) match {
            case (Some(inner), Some(List(inSubType)), Some(List(pipe))) =>
              extract(
                inner,
                ConfigEntry.RemoveSubtype(inSubType, pipe.toUntypedExpr.asTyped[Any]) :: acc
              )
            case _ => Left(s"removeSubtype: unexpected args")
          }

        case "renameSubtype" =>
          (instanceOpt, typeArgsOpt) match {
            case (Some(inner), Some(List(inSubType, outSubType))) =>
              extract(inner, ConfigEntry.RenameSubtype(inSubType, outSubType) :: acc)
            case _ => Left(s"renameSubtype: unexpected args")
          }

        case "plugInSubtype" =>
          (instanceOpt, typeArgsOpt, valueArgsOpt) match {
            case (Some(inner), Some(List(inSubType, outSubType)), Some(List(pipe))) =>
              extract(
                inner,
                ConfigEntry.PlugInSubtype(inSubType, outSubType, pipe.toUntypedExpr.asTyped[Any]) :: acc
              )
            case _ => Left(s"plugInSubtype: unexpected args")
          }

        case "enumMatchingCaseInsensitive" =>
          instanceOpt match {
            case Some(inner) => extract(inner, ConfigEntry.EnumCaseInsensitive :: acc)
            case None        => Left("enumMatchingCaseInsensitive: missing instance")
          }

        case "recursiveDerivation" =>
          instanceOpt match {
            case Some(inner) => extract(inner, ConfigEntry.EnableRecursiveDerivation :: acc)
            case None        => Left("recursiveDerivation: missing instance")
          }

        case other =>
          Left(s"Unsupported config method: $other")
      }

    case _ =>
      Left(s"Unsupported config expression: ${expr.plainPrint}")
  }
}
