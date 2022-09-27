package pipez.internal

import pipez.PipeDerivationConfig
import pipez.internal.Definitions.{ Context, Result }

import scala.annotation.nowarn
import scala.reflect.macros.blackbox

@nowarn("msg=The outer reference in this type test cannot be checked at run time.")
private[internal] trait PlatformDefinitions[Pipe[_, _], In, Out] extends Definitions[Pipe, In, Out] {

  val c: blackbox.Context

  import c.universe.*

  type Tagged[U] = { type Tag = U }
  type @@[T, U]  = T & Tagged[U]

  override type Type[A] = c.Type @@ A
  override type Expr[A] = c.Expr[A]

  // def PipeOf[I: Type, O: Type]: Type[Pipe[I, O]] is defined in MacrosImpl

  final def previewType[A: Type]: String = typeOf[A].toString

  final def previewCode[A](code: Expr[A]): String = showCode(code.tree)

  final def pathCode(path: Path): Expr[pipez.Path] = path match {
    case Path.Root                => c.Expr[pipez.Path](q"_root_.pipez.Path.root")
    case Path.Field(from, name)   => c.Expr[pipez.Path](q"${pathCode(from)}.field(${Constant(name)})")
    case Path.Subtype(from, name) => c.Expr[pipez.Path](q"${pathCode(from)}.subtype(${Constant(name)})")
  }

  final def summonPipe[Input: Type, Output: Type]: DerivationResult[Expr[Pipe[Input, Output]]] =
    DerivationResult
      .unsafe(c.Expr[Pipe[Input, Output]](c.inferImplicitValue(PipeOf[Input, Output], silent = false)))(_ =>
        DerivationError.RequiredImplicitNotFound(typeOf[Input], typeOf[Output])
      )
      .logSuccess(i => s"Summoned implicit value: ${previewCode(i)}")

  // def derivePipe[Input: Type, Output: Type](Settings): DerivationResult[Expr[Pipe[Input, Output]]] is defined in MacrosImpl

  final def singleAbstractMethodExpansion[SAM: Type](code: Expr[SAM]): Expr[SAM] =
    c.Expr(q"_root_.scala.Predef.identity[${typeOf[SAM]}]($code)")

  final def readConfig(code: Expr[PipeDerivationConfig[Pipe, In, Out]]): DerivationResult[Settings] = {
    @nowarn("cat=unused")
    def extractPath(in: Tree): Either[String, Path] = in match {
      case Function(_, expr)                          => extractPath(expr)
      case Select(expr, TermName(field))              => extractPath(expr).map(Path.Field(_, field)) // extract .field
      case Apply(Select(expr, TermName(get)), List()) => extractPath(expr).map(Path.Field(_, get)) // extract .getField
      case Ident(TermName(_))                         => Right(Path.Root) // drop argName from before .field
      case tt: TypeTree => Right(Path.Subtype(Path.Root, tt.tpe.toString)) // A.Subtype
      case _ => Left(s"Path ${previewCode(c.Expr(in))} is not in format _.field1.field2")
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
            ConfigEntry.AddField(
              outFieldPath,
              outFieldType.asInstanceOf[Type[Any]],
              singleAbstractMethodExpansion(c.Expr(pipe))(PipeOf[In, Any](In, outFieldType.asInstanceOf[Type[Any]]))
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
            ConfigEntry.RenameField(inPath,
                                    inputField.tpe.resultType.asInstanceOf[Type[Any]],
                                    outPath,
                                    outputField.tpe.resultType.asInstanceOf[Type[Any]]
            ) :: acc
          )
        } yield result
      // matches {cfg}.plugInField(_.in, _.out, pipe)
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
              inFieldType.asInstanceOf[Type[Any]],
              outFieldPath,
              outFieldType.asInstanceOf[Type[Any]],
              singleAbstractMethodExpansion(c.Expr(pipe))(
                PipeOf[Any, Any](inFieldType.asInstanceOf[Type[Any]], outFieldType.asInstanceOf[Type[Any]])
              )
            ) :: acc
          )
        } yield result
      // matches {cfg}.fieldMatchingCaseInsensitive
      case Select(expr, TermName("fieldMatchingCaseInsensitive")) =>
        extract(expr, ConfigEntry.FieldCaseInsensitive :: acc)
      // matches {cfg}.enableFallbackToDefaults
      case Select(expr, TermName("enableFallbackToDefaults")) =>
        extract(expr, ConfigEntry.EnableFallbackToDefaults :: acc)
      // matches {cfg}.removeSubtype[InSubtype](pipe)
      case Apply(TypeApply(Select(expr, TermName("removeSubtype")), List(inputSubtype)), List(pipe)) =>
        for {
          inputSubtypePath <- extractPath(inputSubtype)
          inputSubtypeType = inputSubtype.tpe.asInstanceOf[Type[In]]
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
          inputSubtypeType = inputSubtype.tpe.asInstanceOf[Type[In]]
          outputSubtypePath <- extractPath(outputSubtype)
          outputSubtypeType = outputSubtype.tpe.asInstanceOf[Type[Out]]
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
              inputSubtypeType.asInstanceOf[Type[In]],
              outputSubtypePath,
              outputSubtypeType.asInstanceOf[Type[Out]],
              singleAbstractMethodExpansion(c.Expr(pipe))(
                PipeOf[In, Out](inputSubtypeType.asInstanceOf[Type[In]], outputSubtypeType.asInstanceOf[Type[Out]])
              )
            ) :: acc
          )
        } yield result
      // matches {cfg}.enumMatchingCaseInsensitive
      case Select(expr, TermName("enumMatchingCaseInsensitive")) =>
        extract(expr, ConfigEntry.EnumCaseInsensitive :: acc)
      // matches {cfg}.enumMatchingCaseInsensitive
      case Select(expr, TermName("recursiveDerivation")) =>
        extract(expr, ConfigEntry.EnableRecursiveDerivation :: acc)
      case els =>
        Left(s"${previewCode(code)} is not a right PipeDerivationConfig")
    }

    DerivationResult.fromEither(extract(code.tree, Nil))(DerivationError.InvalidConfiguration(_))
  }

  // Scala 2-macro specific instances

  implicit val AnyT:     Type[Any]        = c.weakTypeOf[Any].asInstanceOf[Type[Any]]
  implicit val ArrayAny: Type[Array[Any]] = c.weakTypeOf[Array[Any]].asInstanceOf[Type[Array[Any]]]
  def Context:           Type[Context]    = c.weakTypeOf[Context].asInstanceOf[Type[Context]]
  def Result[A: Type]: Type[Result[A]] =
    c.universe.appliedType(c.weakTypeOf[Result[Unit]].typeConstructor, typeOf[A]).asInstanceOf[Type[Result[A]]]
}
