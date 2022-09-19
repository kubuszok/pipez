package pipez

/** Represents path to the value from the object that was passed into `Pipe` to the parsed field.
  *
  * You can use it in `pipeDerivation.updateContext(context, path)` definition, so that before parsing of value
  * `Context` would be enriched with an information how this value was obtained.
  *
  * It allows generating error messages with information where parsing failed.
  */
sealed abstract class Path(override val toString: String) extends Product with Serializable {

  final def field(name: String):   Path = Path.Field(this, name)
  final def subtype(name: String): Path = Path.Subtype(this, name)
}
object Path {

  def root: Path = Root

  case object Root extends Path("root")
  final case class Field(from: Path, name: String) extends Path(s"$from.$name")
  final case class Subtype(from: Path, name: String) extends Path(s"($from: $name)")
}
