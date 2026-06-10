package pipez.internal.compiletime

import hearth.*
import hearth.fp.effect.*
import hearth.std.*
import pipez.{Path, PipeDerivation, PipeDerivationConfig}

import scala.annotation.nowarn
import scala.collection.immutable.ListMap

@nowarn("msg=The outer reference in this type test cannot be checked at run time.")
trait PipezMacrosImpl
    extends rules.PipezHandleAsValueTypeRuleImpl
    with rules.PipezHandleAsCaseClassRuleImpl
    with rules.PipezHandleAsEnumRuleImpl { this: MacroCommons & StdExtensions =>

  type Pipe[_, _]

  implicit def typeOfAny: Type[Any]
  implicit def PipeCtor: Type.Ctor2[Pipe]

  def pdExpr: Expr[Any]

  def pipeType[I: Type, O: Type]: Type[Pipe[I, O]] = PipeCtor.apply[I, O]

  // ---- Code generation (abstract, implemented by platform-specific bridges) ----
  // Context and Result are abstract type members of PipeDerivation, so code generation
  // involving them uses Expr[Any]. The bridges cast to the concrete types internally.

  def generateLift[In: Type, Out: Type](body: (Expr[In], Expr[Any]) => Expr[Any]): Expr[Pipe[In, Out]]
  def generateUnlift(pipe: Expr[Any], in: Expr[Any], ctx: Expr[Any]): Expr[Any]
  def generatePureResult(a: Expr[Any]): Expr[Any]
  def generateMergeResults(ctx: Expr[Any], ra: Expr[Any], rb: Expr[Any], f: Expr[Any]): Expr[Any]
  def generateUpdateContext(ctx: Expr[Any], path: Expr[Path]): Expr[Any]

  def generateArrayToConstructorFn[Out: Type](
      outClass: CaseClass[Out],
      lastIndex: Int,
      totalFields: Int
  ): Expr[Any]

  def generateArrayToBeanFn[Out: Type](
      beanFields: List[(??, Method)],
      defaultConstructor: Method,
      lastIndex: Int,
      totalFields: Int
  ): Expr[Any]

  def postProcessResult[A: Type](expr: Expr[A]): Expr[A] = expr

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
      cache: MLocal[ValDefsCache],
      isTopLevel: Boolean
  )

  // ---- Rule infrastructure ----

  abstract class PipezRule(val name: String) extends Rule {

    /** Given typed input and context expressions (from inside a cached def body), produce the body expression. The body
      * has runtime type Result[Out] but is typed as Any in the shared code because Result is abstract.
      */
    def apply[In: Type, Out: Type](in: Expr[In], ctx: Expr[Any])(implicit
        dctx: DerivationCtx[In, Out]
    ): MIO[Rule.Applicability[Expr[Any]]]
  }

  // ---- ValDefsCache helpers ----

  private def helperKey[In: Type, Out: Type]: String =
    s"pipe-helper:${Type[In].prettyPrint}:${Type[Out].prettyPrint}"

  private def getHelper[In: Type, Out: Type](
      cache: MLocal[ValDefsCache]
  ): MIO[Option[(Expr[In], Expr[Any]) => Expr[Any]]] =
    cache.get2Ary[In, Any, Any](helperKey[In, Out])(implicitly[Type[In]], typeOfAny, typeOfAny)

  private def setHelper[In: Type, Out: Type](
      cache: MLocal[ValDefsCache]
  )(
      body: (Expr[In], Expr[Any]) => MIO[Expr[Any]]
  ): MIO[Unit] = {
    val key = helperKey[In, Out]
    val defName = s"transform_${Type[In].shortName}_${Type[Out].shortName}"
    val defBuilder = ValDefBuilder.ofDef2[In, Any, Any](defName)(implicitly[Type[In]], typeOfAny, typeOfAny)
    for {
      _ <- Log.info(s"Forward-declaring helper for Pipe[${Type[In].prettyPrint}, ${Type[Out].prettyPrint}]")
      _ <- cache.forwardDeclare(key, defBuilder)
      _ <- MIO.scoped { runSafe =>
        runSafe(cache.buildCachedWith(key, defBuilder) { case (_, (inExpr, ctxExpr)) =>
          runSafe(body(inExpr, ctxExpr))
        })
      }
      _ <- Log.info(s"Built helper for Pipe[${Type[In].prettyPrint}, ${Type[Out].prettyPrint}]")
    } yield ()
  }

  private def pipeKey[In: Type, Out: Type]: String =
    s"pipe-instance:${Type[In].prettyPrint}:${Type[Out].prettyPrint}"

  private def getPipe[In: Type, Out: Type](
      cache: MLocal[ValDefsCache]
  ): MIO[Option[Expr[Pipe[In, Out]]]] = {
    @nowarn("msg=unused") implicit val PipeIO: Type[Pipe[In, Out]] = pipeType[In, Out]
    cache.get0Ary[Pipe[In, Out]](pipeKey[In, Out])
  }

  private def setPipe[In: Type, Out: Type](
      cache: MLocal[ValDefsCache]
  )(pipe: Expr[Pipe[In, Out]]): MIO[Unit] = {
    @nowarn("msg=unused") implicit val PipeIO: Type[Pipe[In, Out]] = pipeType[In, Out]
    cache.buildCachedWith(
      pipeKey[In, Out],
      ValDefBuilder.ofLazy[Pipe[In, Out]](s"pipe_${Type[In].shortName}_${Type[Out].shortName}")
    )(_ => pipe)
  }

  // ---- Derivation ----

  def deriveResultRecursively[In: Type, Out: Type](implicit
      ctx: DerivationCtx[In, Out]
  ): MIO[Expr[Pipe[In, Out]]] =
    Log.namedScope(s"Deriving Pipe[${Type[In].prettyPrint}, ${Type[Out].prettyPrint}]") {
      getHelper[In, Out](ctx.cache).flatMap {
        case Some(helperCall) =>
          Log.info(s"Found cached helper for Pipe[${Type[In].prettyPrint}, ${Type[Out].prettyPrint}]") >>
            MIO.pure(generateLift[In, Out]((in, ctxE) => helperCall(in, ctxE)))
        case None =>
          getPipe[In, Out](ctx.cache).flatMap {
            case Some(pipe) =>
              Log.info(s"Found cached pipe for Pipe[${Type[In].prettyPrint}, ${Type[Out].prettyPrint}]") >>
                MIO.pure(pipe)
            case None =>
              val summoned = if (!ctx.isTopLevel) summonPipe[In, Out] else None
              summoned match {
                case Some(pipe) =>
                  Log.info(s"Summoned implicit Pipe[${Type[In].prettyPrint}, ${Type[Out].prettyPrint}]") >>
                    setPipe[In, Out](ctx.cache)(pipe) >> MIO.pure(pipe)
                case None =>
                  val ruleCtx = ctx.copy(isTopLevel = false)
                  setHelper[In, Out](ctx.cache) { (inExpr, ctxExpr) =>
                    deriveBodyViaRules[In, Out](inExpr, ctxExpr, ruleCtx)
                  } >> getHelper[In, Out](ctx.cache).flatMap {
                    case Some(helperCall) =>
                      MIO.pure(generateLift[In, Out]((in, ctxE) => helperCall(in, ctxE)))
                    case None =>
                      MIO.fail(
                        new RuntimeException(
                          s"Failed to build helper for Pipe[${Type[In].prettyPrint}, ${Type[Out].prettyPrint}]"
                        )
                      )
                  }
              }
          }
      }
    }

  private def deriveBodyViaRules[In: Type, Out: Type](
      in: Expr[In],
      ctx: Expr[Any],
      dctx: DerivationCtx[In, Out]
  ): MIO[Expr[Any]] = {
    implicit val implicitDctx: DerivationCtx[In, Out] = dctx
    Rules(
      PipezHandleAsValueTypeRule,
      PipezHandleAsCaseClassRule,
      PipezHandleAsEnumRule
    ) { rule =>
      Log.info(s"Trying rule: ${rule.name}") >> rule.apply[In, Out](in, ctx)
    }.flatMap {
      case Right(result) =>
        Log.info(s"Rule matched for Pipe[${Type[In].prettyPrint}, ${Type[Out].prettyPrint}]") >>
          MIO.pure(result)
      case Left(failures) =>
        val reasons = failures.toList.map(_._2.mkString(", ")).mkString("; ")
        MIO.fail(
          new RuntimeException(
            s"Pipe[${Type[In].prettyPrint}, ${Type[Out].prettyPrint}] couldn't be generated: $reasons"
          )
        )
    }
  }

  // ---- Summoning helpers ----

  def summonPipe[In: Type, Out: Type]: Option[Expr[Pipe[In, Out]]] = {
    @nowarn("msg=unused") implicit val PipeIO: Type[Pipe[In, Out]] = pipeType[In, Out]
    Expr.summonImplicit[Pipe[In, Out]].toOption
  }

  def summonOrDerive[In: Type, Out: Type](implicit
      ctx: DerivationCtx[?, ?]
  ): MIO[Expr[Pipe[In, Out]]] =
    if (ctx.settings.isRecursiveDerivationEnabled) {
      implicit val nestedCtx: DerivationCtx[In, Out] =
        DerivationCtx(Type[In], Type[Out], ctx.settings.stripForRecursion, ctx.cache, isTopLevel = false)
      deriveResultRecursively[In, Out]
    } else
      summonPipe[In, Out] match {
        case Some(pipe) => MIO.pure(pipe)
        case None       =>
          MIO.fail(
            new RuntimeException(
              s"Couldn't find implicit Pipe[${Type[In].prettyPrint}, ${Type[Out].prettyPrint}] " +
                "and recursive derivation was not enabled"
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

  private def deriveWithSettings[In: Type, Out: Type](settings: Settings): Expr[Pipe[In, Out]] = {
    @nowarn("msg=unused") implicit val PipeIO: Type[Pipe[In, Out]] = pipeType[In, Out]
    val cache = ValDefsCache.mlocal
    implicit val ctx: DerivationCtx[In, Out] =
      DerivationCtx[In, Out](Type[In], Type[Out], settings, cache, isTopLevel = true)

    Log
      .namedScope(s"PipeDerivation[${Type[In].prettyPrint} => ${Type[Out].prettyPrint}]") {
        for {
          _ <- Environment.loadStandardExtensions().toMIO(allowFailures = true)
          result <- deriveResultRecursively[In, Out]
          cacheState <- cache.get
        } yield postProcessResult(cacheState.toValDefs.use(_ => result))
      }
      .runToExprOrFail(
        "PipeDerivation.derive",
        infoRendering = DontRender,
        errorRendering = DontRender,
        timeout = scala.concurrent.duration.FiniteDuration(5, java.util.concurrent.TimeUnit.SECONDS)
      ) { (_, errors) =>
        s"Pipe[${Type[In].prettyPrint}, ${Type[Out].prettyPrint}] couldn't be generated:\n" +
          errors.map(e => s" - ${e.getMessage}").mkString("\n")
      }
  }

  // ---- Config parser (implemented by platform-specific bridges) ----

  def readConfig[In: Type, Out: Type](code: Expr[PipeDerivationConfig[Pipe, In, Out]]): Settings
}
