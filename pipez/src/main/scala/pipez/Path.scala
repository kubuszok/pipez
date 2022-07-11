package pipez

sealed abstract class Path(show: String) extends Product with Serializable {

  final def field(name: String):   Path = Path.Field(this, name)
  final def subtype(name: String): Path = Path.Subtype(this, name)
  final def index(index: Int):     Path = Path.AtIndex(this, index)
  final def key(key: String):      Path = Path.AtKey(this, key)

  override lazy val toString = show
}
object Path {
  case object Root extends Path("root")
  final case class Field(from: Path, name: String) extends Path(s"$from.$name")
  final case class Subtype(from: Path, name: String) extends Path(s"($from: $name)")
  final case class AtIndex(from: Path, index: Int) extends Path(s"$from($index)")
  final case class AtKey(from: Path, key: String) extends Path(s"$from($key)")
}
