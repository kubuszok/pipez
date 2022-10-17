package pipez.internal

import pipez.{ PipeDerivation, PipeDerivationConfig }
import pipez.internal.Definitions.{ Context, Result }

import scala.annotation.{ nowarn, unused }
import scala.util.chaining.*
import scala.quoted.{ Type as _, * }

private[internal] trait PlatformDefinitions[Pipe[_, _], In, Out](using val quotes: Quotes)
    extends Definitions[Pipe, In, Out] {

  import quotes.*
  import quotes.reflect.*

  override type Type[A] = scala.quoted.Type[A]
  override type Expr[A] = scala.quoted.Expr[A]

  // def PipeOf[I: Type, O: Type]: Type[Pipe[I, O]] is defined in MacrosImpl

  final def previewType[A: Type]: String =
    val repr = TypeRepr.of[A]
    scala.util.Try(repr.dealias.show(using Printer.TypeReprAnsiCode)).getOrElse(repr.toString)

  final def previewCode[A](code: Expr[A]): String = code.asTerm.show(using Printer.TreeAnsiCode)

  final def pathCode(path: Path): Expr[pipez.Path] = path match
    case Path.Root                => '{ pipez.Path.root }
    case Path.Field(from, name)   => '{ ${ pathCode(from) }.field(${ Expr(name) }) }
    case Path.Subtype(from, name) => '{ ${ pathCode(from) }.subtype(${ Expr(name) }) }

  final def summonPipe[Input: Type, Output: Type]: DerivationResult[Expr[Pipe[Input, Output]]] =
    DerivationResult
      .fromOption(scala.quoted.Expr.summon[Pipe[Input, Output]])(
        DerivationError.RequiredImplicitNotFound(typeOf[Input], typeOf[Output])
      )
      .logSuccess(i => s"Summoned implicit value: ${previewCode(i)}")

  // def derivePipe[Input: Type, Output: Type](Settings): DerivationResult[Expr[Pipe[Input, Output]]] is defined in MacrosImpl

  final def singleAbstractMethodExpansion[SAM: Type](code: Expr[SAM]): Expr[SAM] =
    val SAM = typeOf[SAM]
    '{ scala.Predef.identity[SAM.Underlying](${ code }) }

  final def readConfig(code: Expr[PipeDerivationConfig[Pipe, In, Out]]): DerivationResult[Settings] = {
    def extractPath(in: Tree): Either[String, Path] = in match {
      case Block(List(DefDef(_, _, _, Some(term))), _) => extractPath(term)
      case Select(term, field)                         => extractPath(term).map(Path.Field(_, field)) // extract .field
      case Apply(Select(term, get), List())            => extractPath(term).map(Path.Field(_, get)) // extract .getField
      case Ident(_)                                    => Right(Path.Root) // drop argName from before .field
      case Singleton(Select(_, subtypeName))           => Right(Path.Subtype(Path.Root, subtypeName)) // A.CaseObject
      case TypeSelect(_, subtypeName)                  => Right(Path.Subtype(Path.Root, subtypeName)) // A.CaseClass
      case _ => Left(s"Path ${previewCode(in.asExpr)} is not in format _.field1.field2 ($in)")
    }

    def extract(tree: Term, acc: List[ConfigEntry]): Either[String, Settings] = tree match {
      // drop wrapper
      case Inlined(_, List(), expr) => extract(expr, acc)
      // matches PipeDerivationConfig[Pipe, In, Out]
      case TypeApply(Select(_, "apply"), List(_, _, _)) =>
        Right(new Settings(acc))
      // matches PipeCompanion.Config[In, Out]
      case TypeApply(Select(Select(_, "Config"), "apply"), List(_, _)) =>
        Right(new Settings(acc))
      // matches {cfg}.enableDiagnostics
      case Select(expr, "enableDiagnostics") =>
        extract(expr, ConfigEntry.EnableDiagnostics :: acc)
      // matches {cfg}.addField(_.in, pipe)
      case Apply(TypeApply(Select(expr, "addField"), List(outputType)), List(outputField, pipe)) =>
        for {
          outFieldPath <- extractPath(outputField)
          outFieldType = returnType[Any](outputType.tpe)
          result <- extract(
            expr,
            ConfigEntry.AddField(
              outFieldPath,
              outFieldType,
              singleAbstractMethodExpansion(pipe.asExpr.asInstanceOf[Expr[Pipe[In, Any]]])(
                PipeOf[In, Any](In, outFieldType)
              )
            ) :: acc
          )
        } yield result
      // matches {cfg}.renameField(_.in, _.out)
      case Apply(TypeApply(Select(expr, "renameField"), List(inputType, outputType)), List(inputField, outputField)) =>
        for {
          inPath <- extractPath(inputField)
          inputFieldType = returnType[Any](inputType.tpe)
          outPath <- extractPath(outputField)
          outputFieldType = returnType[Any](outputType.tpe)
          result <- extract(
            expr,
            ConfigEntry.RenameField(inPath, inputFieldType, outPath, outputFieldType) :: acc
          )
        } yield result
      // matches {cfg}.plugIn(_.in, _.out, pipe)
      case Apply(TypeApply(Select(expr, "plugInField"), List(inputType, outputType)),
                 List(inputField, outputField, pipe)
          ) =>
        for {
          inPath <- extractPath(inputField)
          inputFieldType = returnType[Any](inputType.tpe)
          outPath <- extractPath(outputField)
          outputFieldType = returnType[Any](outputType.tpe)
          result <- extract(
            expr,
            ConfigEntry.PlugInField(
              inPath,
              inputFieldType,
              outPath,
              outputFieldType,
              singleAbstractMethodExpansion(pipe.asExpr.asInstanceOf[Expr[Pipe[Any, Any]]])(
                PipeOf[Any, Any](inputFieldType, outputFieldType)
              )
            ) :: acc
          )
        } yield result
      // matches {cfg}.fieldMatchingCaseInsensitive
      case Select(expr, "fieldMatchingCaseInsensitive") =>
        extract(expr, ConfigEntry.FieldCaseInsensitive :: acc)
      // matches {cfg}.addFallbackToValue(fallbackValue)
      case Apply(TypeApply(Select(expr, "addFallbackToValue"), List(fallbackValueType)), List(fallbackValue)) =>
        extract(
          expr,
          ConfigEntry.AddFallbackValue(returnType[Any](fallbackValueType.tpe),
                                       fallbackValue.asExpr.asInstanceOf[Expr[Any]]
          ) :: acc
        )
      // matches {cfg}.enableFallbackToDefaults
      case Select(expr, "enableFallbackToDefaults") =>
        extract(expr, ConfigEntry.EnableFallbackToDefaults :: acc)
      // matches {cfg}.removeSubtype[InSubtype](pipe)
      case Apply(TypeApply(Select(expr, "removeSubtype"), List(inputSubtype)), List(pipe)) =>
        for {
          inputSubtypePath <- extractPath(inputSubtype)
          inputSubtypeType = returnType[In](inputSubtype.tpe)
          result <- extract(
            expr,
            ConfigEntry.RemoveSubtype(
              inputSubtypePath,
              inputSubtypeType,
              singleAbstractMethodExpansion(pipe.asExpr.asInstanceOf[Expr[Pipe[In, Out]]])(
                PipeOf[In, Out](inputSubtypeType, Out)
              )
            ) :: acc
          )
        } yield result
      // matches {cfg}.renameSubtype[InSubtype, OutSubtype]
      case TypeApply(Select(expr, "renameSubtype"), List(inputSubtype, outputSubtype)) =>
        for {
          inputSubtypePath <- extractPath(inputSubtype)
          inputSubtypeType = returnType[In](inputSubtype.tpe)
          outputSubtypePath <- extractPath(outputSubtype)
          outputSubtypeType = returnType[Out](outputSubtype.tpe)
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
      // matches {cfg}.plugInSubtype[InSubtype, OutSubtype](pipe)
      case Apply(TypeApply(Select(expr, "plugInSubtype"), List(inputSubtype, outputSubtype)), List(pipe)) =>
        for {
          inputSubtypePath <- extractPath(inputSubtype)
          inputSubtypeType = returnType[In](inputSubtype.tpe)
          outputSubtypePath <- extractPath(outputSubtype)
          outputSubtypeType = returnType[Out](outputSubtype.tpe)
          result <- extract(
            expr,
            ConfigEntry.PlugInSubtype(
              inputSubtypePath,
              inputSubtypeType,
              outputSubtypePath,
              outputSubtypeType,
              singleAbstractMethodExpansion(pipe.asExpr.asInstanceOf[Expr[Pipe[In, Out]]])(
                PipeOf[In, Out](inputSubtypeType, outputSubtypeType)
              )
            ) :: acc
          )
        } yield result
      // matches {cfg}.enumMatchingCaseInsensitive
      case Select(expr, "enumMatchingCaseInsensitive") =>
        extract(expr, ConfigEntry.EnumCaseInsensitive :: acc)
      // matches {cfg}.enumMatchingCaseInsensitive
      case Select(expr, "recursiveDerivation") =>
        extract(expr, ConfigEntry.EnableRecursiveDerivation :: acc)
      case els =>
        Left(s"${previewCode(code)} is not a right PipeDerivationConfig")
    }

    DerivationResult.fromEither(extract(code.asTerm, Nil))(DerivationError.InvalidConfiguration(_))
  }

  // Scala 3-macro specific instances, required because code-generation needs these types

  implicit val Pipe:    scala.quoted.Type[Pipe]
  implicit val Context: scala.quoted.Type[Context]
  implicit val Result:  scala.quoted.Type[Result]

  def returnType[A](typeRepr: TypeRepr): Type[A] = typeRepr.widenByName match
    case MethodType(_, _, out) => returnType[A](out)
    case out                   => out.asType.asInstanceOf[Type[A]]

  val resolveTypeArgsForMethodArguments = (tpe: TypeRepr, method: Symbol) =>
    tpe.memberType(method) match
      // monomorphic
      case MethodType(names, types, _) =>
        val typeArgs:           List[TypeRepr]        = Nil
        val typeArgumentByName: Map[String, TypeRepr] = names.zip(types).toMap
        DerivationResult.pure(typeArgumentByName -> typeArgs)
      // polymorphic
      case PolyType(_, _, MethodType(names, types, AppliedType(_, typeRefs))) =>
        // TODO: check if types of constructor match types passed to Out
        val typeArgs: List[TypeRepr] = TypeRepr.of[Out].typeArgs
        val typeArgumentByAlias = typeRefs.zip(typeArgs).toMap
        val typeArgumentByName: Map[String, TypeRepr] = names
          .zip(types)
          .toMap
          .view
          .mapValues { tpe =>
            typeArgumentByAlias.getOrElse(tpe, tpe)
          }
          .toMap
        DerivationResult.pure(typeArgumentByName -> typeArgs)
      // unknown
      case tpe =>
        DerivationResult.fail(
          DerivationError.NotYetImplemented(
            s"Constructor of ${previewType[Out]} has unrecognized/unsupported format of type: ${tpe}"
          )
        )
}
