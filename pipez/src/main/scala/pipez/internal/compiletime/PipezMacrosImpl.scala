package pipez.internal.compiletime

import hearth.*
import hearth.fp.effect.*
import hearth.std.*
import pipez.{Path, PipeDerivation, PipeDerivationConfig}

import scala.annotation.nowarn
import scala.collection.immutable.ListMap

@nowarn("msg=The outer reference in this type test cannot be checked at run time.")
trait PipezMacrosImpl
    extends PipezConfigParserShared
    with rules.PipezUseImplicitRuleImpl
    with rules.PipezHandleAsValueTypeRuleImpl
    with rules.PipezHandleAsCaseClassRuleImpl
    with rules.PipezHandleAsEnumRuleImpl { this: MacroCommons & StdExtensions =>

  type Pipe[_, _]

  // Provide Type[Any] for rule impls that need it in implicit scope.
  // Implemented in platform-specific bridges to avoid cross-quotes plugin
  // recursive implicit self-reference on Scala 3.
  implicit def typeOfAny: Type[Any]

  implicit def PipeCtor: Type.Ctor2[Pipe]

  def pdExpr: Expr[Any]

  def pipeType[I: Type, O: Type]: Type[Pipe[I, O]] = {
    debugLog(s"  pipeType called for [${Type[I].prettyPrint}, ${Type[O].prettyPrint}]")
    val r = PipeCtor.apply[I, O]
    debugLog(s"  pipeType done")
    r
  }

  // ---- Code generation ----
  // All internal expression types use Expr[Any] with asInstanceOf casts.
  // This is safe because all generated code goes through pipeDerivation method calls.

  def generateLift[In: Type, Out: Type](body: (Expr[Any], Expr[Any]) => Expr[Any]): Expr[Pipe[In, Out]]
  def generateUnlift(pipe: Expr[Any], in: Expr[Any], ctx: Expr[Any]): Expr[Any]
  def generatePureResult(a: Expr[Any]): Expr[Any]
  def generateMergeResults(ctx: Expr[Any], ra: Expr[Any], rb: Expr[Any], f: Expr[Any]): Expr[Any]
  def generateUpdateContext(ctx: Expr[Any], path: Expr[Path]): Expr[Any]

  /** Build a block expression: { stmt0; stmt1; ...; result } */
  def generateBlock(statements: List[Expr[Any]], result: Expr[Any]): Expr[Any]

  /** Build a lambda (Array[Any], Any) => Any that stores the value at the given index, then reads all fields from the
    * array and calls the case class primary constructor. The parameter types and constructor are resolved from the case
    * class info.
    */
  def generateArrayToConstructorFn[Out: Type](
      outClass: CaseClass[Out],
      lastIndex: Int,
      totalFields: Int
  ): Expr[Any]

  /** Build a lambda (Array[Any], Any) => Any that stores the value at the given index, then reads all fields from the
    * array, calls each bean setter, and returns the bean.
    */
  def generateArrayToBeanFn[Out: Type](
      beanFields: List[(??, Method)],
      defaultConstructor: Method,
      lastIndex: Int,
      totalFields: Int
  ): Expr[Any]

  // ---- Settings ----

  final class Settings(val entries: List[ConfigEntry]) {
    lazy val isDiagnosticsEnabled: Boolean = entries.contains(ConfigEntry.EnableDiagnostics)
    lazy val isFieldCaseInsensitive: Boolean = entries.contains(ConfigEntry.FieldCaseInsensitive)
    lazy val isEnumCaseInsensitive: Boolean = entries.contains(ConfigEntry.EnumCaseInsensitive)
    lazy val isRecursiveDerivationEnabled: Boolean = entries.contains(ConfigEntry.EnableRecursiveDerivation)
    lazy val isFallbackToDefaultEnabled: Boolean = entries.contains(ConfigEntry.EnableFallbackToDefaults)

    lazy val addedFields: List[ConfigEntry.AddField] = entries.collect { case e: ConfigEntry.AddField => e }
    lazy val renamedFields: List[ConfigEntry.RenameField] = entries.collect { case e: ConfigEntry.RenameField => e }
    lazy val pluggedFields: List[ConfigEntry.PlugInField] = entries.collect { case e: ConfigEntry.PlugInField => e }
    lazy val fallbackValues: List[ConfigEntry.AddFallbackValue] = entries.collect {
      case e: ConfigEntry.AddFallbackValue => e
    }
    lazy val removedSubtypes: List[ConfigEntry.RemoveSubtype] = entries.collect { case e: ConfigEntry.RemoveSubtype =>
      e
    }
    lazy val renamedSubtypes: List[ConfigEntry.RenameSubtype] = entries.collect { case e: ConfigEntry.RenameSubtype =>
      e
    }
    lazy val pluggedSubtypes: List[ConfigEntry.PlugInSubtype] = entries.collect { case e: ConfigEntry.PlugInSubtype =>
      e
    }

    def stripForRecursion: Settings = new Settings(
      entries.collect {
        case ConfigEntry.EnableDiagnostics        => ConfigEntry.EnableDiagnostics
        case ConfigEntry.FieldCaseInsensitive     => ConfigEntry.FieldCaseInsensitive
        case ConfigEntry.EnableFallbackToDefaults => ConfigEntry.EnableFallbackToDefaults
        case ConfigEntry.EnumCaseInsensitive      => ConfigEntry.EnumCaseInsensitive
      }
    )
  }

  sealed trait ConfigEntry
  object ConfigEntry {
    case object EnableDiagnostics extends ConfigEntry
    final case class AddField(outFieldName: String, pipe: Expr[Any]) extends ConfigEntry
    final case class RenameField(inFieldName: String, outFieldName: String) extends ConfigEntry
    final case class PlugInField(inFieldName: String, outFieldName: String, pipe: Expr[Any]) extends ConfigEntry
    case object FieldCaseInsensitive extends ConfigEntry
    final case class AddFallbackValue(fallbackType: ??, fallbackValue: Expr[Any]) extends ConfigEntry
    case object EnableFallbackToDefaults extends ConfigEntry
    final case class RemoveSubtype(inSubtypeType: ??, pipe: Expr[Any]) extends ConfigEntry
    final case class RenameSubtype(inSubtypeType: ??, outSubtypeType: ??) extends ConfigEntry
    final case class PlugInSubtype(inSubtypeType: ??, outSubtypeType: ??, pipe: Expr[Any]) extends ConfigEntry
    case object EnumCaseInsensitive extends ConfigEntry
    case object EnableRecursiveDerivation extends ConfigEntry
  }

  // ---- DerivationCtx ----

  final case class DerivationCtx[In, Out](
      inType: Type[In],
      outType: Type[Out],
      settings: Settings,
      derivedPipeType: Option[??]
  )

  // ---- Rule infrastructure ----

  abstract class PipezRule(val name: String) extends Rule {
    def apply[In: Type, Out: Type](implicit ctx: DerivationCtx[In, Out]): MIO[Rule.Applicability[Expr[Pipe[In, Out]]]]
  }

  def deriveResultRecursively[In: Type, Out: Type](implicit
      ctx: DerivationCtx[In, Out]
  ): MIO[Expr[Pipe[In, Out]]] = {
    debugLog(s"  deriveResultRecursively: ${Type[In].prettyPrint} => ${Type[Out].prettyPrint}")
    Log.namedScope(s"Deriving Pipe[${Type[In].prettyPrint}, ${Type[Out].prettyPrint}]") {
      debugLog(s"    about to run Rules")
      Rules(
        PipezUseImplicitRule,
        PipezHandleAsValueTypeRule,
        PipezHandleAsCaseClassRule,
        PipezHandleAsEnumRule
      ) { rule => debugLog(s"    trying rule: ${rule.name}"); rule.apply[In, Out] }.flatMap {
        case Right(result) =>
          debugLog(s"    rule matched!")
          MIO.pure(result)
        case Left(failures) =>
          val reasons = failures.toList.map(_._2.mkString(", ")).mkString("; ")
          debugLog(s"    all rules failed: $reasons")
          MIO.fail(
            new RuntimeException(
              s"Pipe[${Type[In].prettyPrint}, ${Type[Out].prettyPrint}] couldn't be generated: $reasons"
            )
          )
      }
    }
  }

  // ---- Summoning helpers ----

  def summonPipe[In: Type, Out: Type]: Option[Expr[Pipe[In, Out]]] = {
    implicit val PipeIO: Type[Pipe[In, Out]] = pipeType[In, Out]
    Expr.summonImplicit[Pipe[In, Out]].toOption
  }

  def summonOrDerive[In: Type, Out: Type](implicit
      ctx: DerivationCtx[?, ?]
  ): MIO[Expr[Pipe[In, Out]]] =
    summonPipe[In, Out] match {
      case Some(pipe)                                        => MIO.pure(pipe)
      case None if ctx.settings.isRecursiveDerivationEnabled =>
        val newCtx = DerivationCtx[In, Out](
          inType = Type[In],
          outType = Type[Out],
          settings = ctx.settings.stripForRecursion,
          derivedPipeType = ctx.derivedPipeType
        )
        deriveResultRecursively[In, Out](Type[In], Type[Out], newCtx)
      case None =>
        MIO.fail(
          new RuntimeException(
            s"Couldn't find implicit Pipe[${Type[In].prettyPrint}, ${Type[Out].prettyPrint}] and recursive derivation was not enabled"
          )
        )
    }

  // ---- Path helpers ----

  def pathFieldCode(name: String): Expr[Path] =
    Expr.quote(Path.root.field(Expr.splice(Expr(name))))

  def pathSubtypeCode(name: String): Expr[Path] =
    Expr.quote(Path.root.subtype(Expr.splice(Expr(name))))

  // ---- Name matching ----

  private val getAccessor = raw"(?i)get(.)(.*)".r
  private val isAccessor = raw"(?i)is(.)(.*)".r

  def dropGetIs(name: String): String = name match {
    case getAccessor(head, tail) => head.toLowerCase + tail
    case isAccessor(head, tail)  => head.toLowerCase + tail
    case _                       => name
  }

  def inputNameMatchesOutputName(inName: String, outName: String, caseInsensitive: Boolean): Boolean = {
    val in = Set(inName, dropGetIs(inName))
    val out = Set(outName, dropGetIs(outName))
    if (caseInsensitive) in.exists(a => out.exists(b => a.equalsIgnoreCase(b)))
    else in.intersect(out).nonEmpty
  }

  // ---- Entry points ----

  def deriveDefault[In: Type, Out: Type]: Expr[Pipe[In, Out]] =
    deriveWithSettings[In, Out](new Settings(Nil))

  def deriveConfigured[In: Type, Out: Type](config: Expr[PipeDerivationConfig[Pipe, In, Out]]): Expr[Pipe[In, Out]] =
    deriveWithSettings[In, Out](readConfig(config))

  private def debugLog(msg: String): Unit = {
    val f = new java.io.FileWriter("/tmp/pipez-debug.log", true)
    f.write(s"[${java.time.Instant.now}] $msg\n")
    f.close()
  }

  /** Top-level derivation skips PipezUseImplicitRule to avoid self-referential implicit cycles (e.g. implicit lazy val
    * codec = derive(config) would summon itself via the implicit rule).
    */
  private def deriveTopLevel[In: Type, Out: Type](implicit
      ctx: DerivationCtx[In, Out]
  ): MIO[Expr[Pipe[In, Out]]] = {
    debugLog(s"  deriveTopLevel: ${Type[In].prettyPrint} => ${Type[Out].prettyPrint}")
    Log.namedScope(s"Deriving Pipe[${Type[In].prettyPrint}, ${Type[Out].prettyPrint}]") {
      debugLog(s"    about to run Rules (top-level, no implicit rule)")
      Rules(
        PipezHandleAsValueTypeRule,
        PipezHandleAsCaseClassRule,
        PipezHandleAsEnumRule
      ) { rule => debugLog(s"    trying rule: ${rule.name}"); rule.apply[In, Out] }.flatMap {
        case Right(result) =>
          debugLog(s"    rule matched!")
          MIO.pure(result)
        case Left(failures) =>
          val reasons = failures.toList.map(_._2.mkString(", ")).mkString("; ")
          debugLog(s"    all rules failed: $reasons")
          MIO.fail(
            new RuntimeException(
              s"Pipe[${Type[In].prettyPrint}, ${Type[Out].prettyPrint}] couldn't be generated: $reasons"
            )
          )
      }
    }
  }

  def deriveWithSettings[In: Type, Out: Type](settings: Settings): Expr[Pipe[In, Out]] = {
    debugLog(s"deriveWithSettings start: ${Type[In].prettyPrint} => ${Type[Out].prettyPrint}")
    @nowarn("msg=unused") implicit val PipeIO: Type[Pipe[In, Out]] = pipeType[In, Out]
    debugLog(s"  pipeType constructed")
    val ctx = DerivationCtx[In, Out](Type[In], Type[Out], settings, derivedPipeType = None)
    debugLog(s"  ctx created, entering MIO")

    val result = Log.namedScope(s"PipeDerivation[${Type[In].prettyPrint} => ${Type[Out].prettyPrint}]") {
      debugLog(s"  inside Log.namedScope")
      for {
        _ <- Environment.loadStandardExtensions().toMIO(allowFailures = true)
        result <- deriveTopLevel[In, Out](Type[In], Type[Out], ctx)
      } yield result
    }
    debugLog(s"  MIO created, calling runToExprOrFail")
    result.runToExprOrFail(
      "PipeDerivation.derive",
      infoRendering = DontRender,
      errorRendering = DontRender,
      timeout = scala.concurrent.duration.FiniteDuration(5, java.util.concurrent.TimeUnit.SECONDS)
    ) { (_, errors) =>
      val msg = s"Pipe[${Type[In].prettyPrint}, ${Type[Out].prettyPrint}] couldn't be generated:\n" +
        errors.map(e => s" - ${e.getMessage}").mkString("\n")
      debugLog(s"  ERROR: $msg")
      msg
    }
  }

}
