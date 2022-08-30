package pipez

sealed abstract class Path(show: String) extends Product with Serializable {

  final def field(name: String):   Path = Path.Field(this, name)
  final def subtype(name: String): Path = Path.Subtype(this, name)

  override lazy val toString = show
}
object Path {

  def root: Path = Root

  case object Root extends Path("root")
  final case class Field(from: Path, name: String) extends Path(s"$from.$name")
  final case class Subtype(from: Path, name: String) extends Path(s"($from: $name)")
}
