package pipez.internal.compiletime

import hearth.*
import hearth.fp.DirectStyle
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

  // ---- The type class's abstract Context / Result, threaded as real Hearth types ----
  // `PipeDerivation#Context` and `#Result[_]` are abstract type members, known concretely only from the user's
  // instance at the call site. Both platforms extract them at the entry point and expose them here as `Ctx`/`Res` plus
  // their `Type` evidence, so the whole derivation is typed (`Expr[Ctx]`, `Expr[Res[X]]`, `Expr[Pipe[In, Out]]`) with
  // no `Expr[Any]`/`asInstanceOf` erasure. `Ctx`/`Res` stay abstract phantoms; the evidence values carry the real
  // types into the generated trees.

  type Ctx
  type Res[_]

  implicit def ctxType: Type[Ctx]
  implicit def resultCtor: Type.Ctor1[Res]
  implicit final def resType[A: Type]: Type[Res[A]] = resultCtor.apply[A]

  /** The user's `PipeDerivation` instance, typed (via the adapter's one sanctioned cast) against the `Aux` refinement
    * that equates its abstract `Context`/`Result` members with the carried `Ctx`/`Res`.
    */
  def pdAux: Expr[PipeDerivation.Aux[Pipe, Ctx, Res]]

  /** Everything the shared, generic codegen needs. `Pipe`/`Ctx`/`Res` are inferred from this value at each call site,
    * so inside the `gen*` helpers they are *method* type parameters — which the cross-quotes plugin resolves from
    * block-level `implicit` `Type.Ctor2`/`Type.Ctor1`/`Type` evidence (a context bound is NOT enough for the `Aux`).
    */
  final def codegenCtx: CodegenCtx[Pipe, Ctx, Res] = CodegenCtx(pdAux, PipeCtor, resultCtor, ctxType)

  // ---- Code generation: thin typed facade over the shared, generic `gen*` cross-quote helpers ----

  final def generateLift[In: Type, Out: Type](body: (Expr[In], Expr[Ctx]) => Expr[Res[Out]]): Expr[Pipe[In, Out]] =
    genLift[Pipe, Ctx, Res, In, Out](codegenCtx, body)
  final def generateUnlift[In: Type, Out: Type](
      pipe: Expr[Pipe[In, Out]],
      in: Expr[In],
      ctx: Expr[Ctx]
  ): Expr[Res[Out]] = genUnlift[Pipe, Ctx, Res, In, Out](codegenCtx, pipe, in, ctx)
  final def generatePureResult[A: Type](a: Expr[A]): Expr[Res[A]] = genPure[Pipe, Ctx, Res, A](codegenCtx, a)
  final def generateMergeResults[A: Type, B: Type, C: Type](
      ctx: Expr[Ctx],
      ra: Expr[Res[A]],
      rb: Expr[Res[B]],
      f: Expr[(A, B) => C]
  ): Expr[Res[C]] = genMerge[Pipe, Ctx, Res, A, B, C](codegenCtx, ctx, ra, rb, f)
  final def generateUpdateContext(ctx: Expr[Ctx], path: Expr[Path]): Expr[Ctx] =
    genUpdate[Pipe, Ctx, Res](codegenCtx, ctx, path)

  // ---- Shared, generic codegen (`Pipe`/`Ctx`/`Res` are method type params here, resolved in the cross-quotes via the
  //      block-level `implicit` Ctor/Type evidence — this is what lets the codegen be platform-agnostic) ----

  final case class CodegenCtx[P2[_, _], C2, R2[_]](
      pd: Expr[PipeDerivation.Aux[P2, C2, R2]],
      pipeCtor: Type.Ctor2[P2],
      resCtor: Type.Ctor1[R2],
      ctxTpe: Type[C2]
  )

  private def genPure[P2[_, _], C2, R2[_], A: Type](cg: CodegenCtx[P2, C2, R2], a: Expr[A]): Expr[R2[A]] = {
    implicit val pc: Type.Ctor2[P2] = cg.pipeCtor
    implicit val rc: Type.Ctor1[R2] = cg.resCtor
    implicit val ct: Type[C2] = cg.ctxTpe
    implicit val auxT: Type[PipeDerivation.Aux[P2, C2, R2]] = Type.of[PipeDerivation.Aux[P2, C2, R2]]
    implicit val rA: Type[R2[A]] = cg.resCtor.apply[A]
    Expr.quote(Expr.splice(cg.pd).pureResult(Expr.splice(a)))
  }

  private def genUnlift[P2[_, _], C2, R2[_], In: Type, Out: Type](
      cg: CodegenCtx[P2, C2, R2],
      pipe: Expr[P2[In, Out]],
      in: Expr[In],
      ctx: Expr[C2]
  ): Expr[R2[Out]] = {
    implicit val pc: Type.Ctor2[P2] = cg.pipeCtor
    implicit val rc: Type.Ctor1[R2] = cg.resCtor
    implicit val ct: Type[C2] = cg.ctxTpe
    implicit val auxT: Type[PipeDerivation.Aux[P2, C2, R2]] = Type.of[PipeDerivation.Aux[P2, C2, R2]]
    implicit val pipeIO: Type[P2[In, Out]] = cg.pipeCtor.apply[In, Out]
    implicit val rOut: Type[R2[Out]] = cg.resCtor.apply[Out]
    Expr.quote(Expr.splice(cg.pd).unlift[In, Out](Expr.splice(pipe), Expr.splice(in), Expr.splice(ctx)))
  }

  private def genUpdate[P2[_, _], C2, R2[_]](cg: CodegenCtx[P2, C2, R2], ctx: Expr[C2], path: Expr[Path]): Expr[C2] = {
    implicit val pc: Type.Ctor2[P2] = cg.pipeCtor
    implicit val rc: Type.Ctor1[R2] = cg.resCtor
    implicit val ct: Type[C2] = cg.ctxTpe
    implicit val auxT: Type[PipeDerivation.Aux[P2, C2, R2]] = Type.of[PipeDerivation.Aux[P2, C2, R2]]
    Expr.quote(Expr.splice(cg.pd).updateContext(Expr.splice(ctx), Expr.splice(path)))
  }

  private def genMerge[P2[_, _], C2, R2[_], A: Type, B: Type, C: Type](
      cg: CodegenCtx[P2, C2, R2],
      ctx: Expr[C2],
      ra: Expr[R2[A]],
      rb: Expr[R2[B]],
      f: Expr[(A, B) => C]
  ): Expr[R2[C]] = {
    implicit val pc: Type.Ctor2[P2] = cg.pipeCtor
    implicit val rc: Type.Ctor1[R2] = cg.resCtor
    implicit val ct: Type[C2] = cg.ctxTpe
    implicit val auxT: Type[PipeDerivation.Aux[P2, C2, R2]] = Type.of[PipeDerivation.Aux[P2, C2, R2]]
    implicit val rA: Type[R2[A]] = cg.resCtor.apply[A]
    implicit val rB: Type[R2[B]] = cg.resCtor.apply[B]
    implicit val rC: Type[R2[C]] = cg.resCtor.apply[C]
    Expr.quote(
      Expr.splice(cg.pd).mergeResults[A, B, C](Expr.splice(ctx), Expr.splice(ra), Expr.splice(rb), Expr.splice(f))
    )
  }

  private def genLift[P2[_, _], C2, R2[_], In: Type, Out: Type](
      cg: CodegenCtx[P2, C2, R2],
      body: (Expr[In], Expr[C2]) => Expr[R2[Out]]
  ): Expr[P2[In, Out]] = {
    implicit val pc: Type.Ctor2[P2] = cg.pipeCtor
    implicit val rc: Type.Ctor1[R2] = cg.resCtor
    implicit val ct: Type[C2] = cg.ctxTpe
    implicit val auxT: Type[PipeDerivation.Aux[P2, C2, R2]] = Type.of[PipeDerivation.Aux[P2, C2, R2]]
    implicit val pipeIO: Type[P2[In, Out]] = cg.pipeCtor.apply[In, Out]
    implicit val rOut: Type[R2[Out]] = cg.resCtor.apply[Out]
    val lam: Expr[(In, C2) => R2[Out]] =
      LambdaBuilder.of2[In, C2]().buildWith[R2[Out]] { case (inE, ctxE) => body(inE, ctxE) }
    Expr.quote(Expr.splice(cg.pd).lift[In, Out](Expr.splice(lam)))
  }

  // ---- Array → Out builders (shared; no Pipe/Ctx/Res, just construction via Hearth's `CaseClass`/`JavaBean`) ----

  def generateArrayToConstructorFn[Out: Type](
      outClass: CaseClass[Out],
      lastIndex: Int,
      totalFields: Int
  ): Expr[(Array[Any], Unit) => Out] = {
    implicit val arrT: Type[Array[Any]] = Type.of[Array[Any]]
    implicit val unitT: Type[Unit] = Type.of[Unit]
    LambdaBuilder.of2[Array[Any], Unit]().buildWith[Out] { case (arrE, _) =>
      val args: Map[String, Expr_??] = outClass.primaryConstructor.totalParameters.flatten.zipWithIndex.map {
        case ((name, param), i) =>
          import param.tpe.Underlying as Pi
          name -> arrayGet[Pi](arrE, i).as_??
      }.toMap
      callConstructor[Out](outClass.primaryConstructor, args)
    }
  }

  def generateArrayToBeanFn[Out: Type](
      beanFields: List[(??, Method)],
      defaultConstructor: Method,
      lastIndex: Int,
      totalFields: Int
  ): Expr[(Array[Any], Unit) => Out] = {
    implicit val arrT: Type[Array[Any]] = Type.of[Array[Any]]
    implicit val unitT: Type[Unit] = Type.of[Unit]
    LambdaBuilder.of2[Array[Any], Unit]().buildWith[Out] { case (arrE, _) =>
      // Construct in `beanFields` order so each setter reads its OWN array slot — keying by the setter's parameter name
      // would collapse, since `var` setters often share a synthesized param name (e.g. `x$1`).
      DirectStyle[ValDefs].scoped { runSafe =>
        val beanRef: Expr[Out] = runSafe(ValDefs.createVal[Out](callConstructor[Out](defaultConstructor, Map.empty)))
        val setterCalls: List[Expr[Unit]] = beanFields.zipWithIndex.map { case ((fieldType, setter), i) =>
          import fieldType.Underlying as Fi
          callSetter[Out](beanRef, setter, arrayGet[Fi](arrE, i).as_??)
        }
        setterCalls.foldRight(beanRef)((call, acc) => Expr.quote { Expr.splice(call); Expr.splice(acc) })
      }.close
    }
  }

  private def callSetter[A: Type](bean: Expr[A], setter: Method, value: Expr_??): Expr[Unit] =
    setter.fold(
      onInstance = _ => bean.as_??,
      onTypes = _ => Map.empty,
      onValues = _ => Map(setter.totalParameters.flatten.head._1 -> value)
    ) match {
      case Right(e)  => e.value.asInstanceOf[Expr[Unit]]
      case Left(err) => throw new RuntimeException(s"Cannot call setter ${setter.name}: $err")
    }

  private def arrayGet[A: Type](arrE: Expr[Array[Any]], i: Int): Expr[A] = {
    implicit val arrT: Type[Array[Any]] = Type.of[Array[Any]]
    Expr.quote(Expr.splice(arrE).apply(Expr.splice(Expr(i))).asInstanceOf[A])
  }

  private def callConstructor[A: Type](ctor: Method, args: Map[String, Expr_??]): Expr[A] =
    ctor.fold(
      onInstance = _ => throw new RuntimeException("Constructor should not need an instance"),
      onTypes = _ => Map.empty,
      onValues = _ => args
    ) match {
      case Right(e)  => e.value.asInstanceOf[Expr[A]]
      case Left(err) => throw new RuntimeException(s"Cannot construct ${Type[A].prettyPrint}: $err")
    }

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

  // Config pipes/values come from the user's DSL (`addField(_, pipe)`, `addFallbackToValue(v)`, …); they carry their
  // real type from the call site as an `Expr_??` existential rather than being erased to `Expr[Any]`.
  sealed trait ConfigEntry
  object ConfigEntry {
    case object EnableDiagnostics extends ConfigEntry
    final case class AddField(outFieldName: String, pipe: Expr_??) extends ConfigEntry
    final case class RenameField(inFieldName: String, outFieldName: String) extends ConfigEntry
    final case class PlugInField(inFieldName: String, outFieldName: String, pipe: Expr_??) extends ConfigEntry
    case object FieldCaseInsensitive extends ConfigEntry
    final case class AddFallbackValue(fallbackType: ??, fallbackValue: Expr_??) extends ConfigEntry
    case object EnableFallbackToDefaults extends ConfigEntry
    final case class RemoveSubtype(inSubtypeType: ??, pipe: Expr_??) extends ConfigEntry
    final case class RenameSubtype(inSubtypeType: ??, outSubtypeType: ??) extends ConfigEntry
    final case class PlugInSubtype(inSubtypeType: ??, outSubtypeType: ??, pipe: Expr_??) extends ConfigEntry
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

    /** Given the typed input and context expressions (from inside a cached def body), produce the typed `Result[Out]`
      * body expression.
      */
    def apply[In: Type, Out: Type](in: Expr[In], ctx: Expr[Ctx])(implicit
        dctx: DerivationCtx[In, Out]
    ): MIO[Rule.Applicability[Expr[Res[Out]]]]
  }

  // ---- ValDefsCache helpers ----

  private def helperKey[In: Type, Out: Type]: String =
    s"pipe-helper:${Type[In].prettyPrint}:${Type[Out].prettyPrint}"

  private def getHelper[In: Type, Out: Type](
      cache: MLocal[ValDefsCache]
  ): MIO[Option[(Expr[In], Expr[Ctx]) => Expr[Res[Out]]]] =
    cache.get2Ary[In, Ctx, Res[Out]](helperKey[In, Out])(implicitly[Type[In]], ctxType, resType[Out])

  private def setHelper[In: Type, Out: Type](
      cache: MLocal[ValDefsCache]
  )(
      body: (Expr[In], Expr[Ctx]) => MIO[Expr[Res[Out]]]
  ): MIO[Unit] = {
    val key = helperKey[In, Out]
    val defName = s"transform_${Type[In].shortName}_${Type[Out].shortName}"
    val defBuilder = ValDefBuilder.ofDef2[In, Ctx, Res[Out]](defName)(implicitly[Type[In]], ctxType, resType[Out])
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
      ctx: Expr[Ctx],
      dctx: DerivationCtx[In, Out]
  ): MIO[Expr[Res[Out]]] = {
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

  protected def ignoredImplicitMethods: Seq[UntypedMethod] = Seq.empty

  def summonPipe[In: Type, Out: Type]: Option[Expr[Pipe[In, Out]]] = {
    @nowarn("msg=unused") implicit val PipeIO: Type[Pipe[In, Out]] = pipeType[In, Out]
    if (ignoredImplicitMethods.nonEmpty)
      PipeIO.summonExprIgnoring(ignoredImplicitMethods*).toOption
    else
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

  // ---- Config parser (shared across platforms via Hearth's DestructuredExpr) ----

  /** Parses the `PipeDerivationConfig` builder chain (`.addField(_.foo, pipe).renameField(_.a, _.b)...`) into a list of
    * [[ConfigEntry]] using Hearth's macro-agnostic [[DestructuredExpr]]. Replaces the two former platform-specific
    * raw-AST walkers (`PipezConfigParserScala2`/`Scala3`).
    */
  def readConfig[In: Type, Out: Type](code: Expr[PipeDerivationConfig[Pipe, In, Out]]): Settings = {
    import DestructuredExpr.MethodCall

    def instanceOf(mc: MethodCall): Option[DestructuredExpr] =
      mc.applied.collectFirst { case ai: MethodCall.AppliedInstance => ai.value }
    def valuesOf(mc: MethodCall): List[DestructuredExpr] =
      mc.applied.collect { case av: MethodCall.AppliedValues => av.args }.flatten
    def typesOf(mc: MethodCall): List[??] =
      mc.applied.collect { case at: MethodCall.AppliedTypes => at.typeArgs }.flatten

    def toHExpr(node: DestructuredExpr): Expr_?? = node.toUntypedExpr.as_??

    // Leaf field name of a path lambda `_.field` (or zero-arg accessor `_.getField`). Mirrors the old `extractPath`:
    // for a nested `_.a.b` the outermost selection (the leaf) is what the config records.
    def unwrapBlock(node: DestructuredExpr): DestructuredExpr = node match {
      case b: DestructuredExpr.Block => unwrapBlock(b.result)
      case other                     => other
    }
    def fieldName(node: DestructuredExpr): Either[String, String] = unwrapBlock(node) match {
      case lam: DestructuredExpr.Lambda =>
        unwrapBlock(lam.body) match {
          case mc: MethodCall => Right(mc.method.name)
          case other          => Left(s"Unsupported path expression: ${other.plainPrint}")
        }
      case mc: MethodCall => Right(mc.method.name)
      case other          => Left(s"Unsupported path expression: ${other.plainPrint}")
    }

    def extract(node: DestructuredExpr, acc: List[ConfigEntry]): Either[String, Settings] = unwrapBlock(node) match {
      case mc: MethodCall =>
        def receiver: Either[String, DestructuredExpr] =
          instanceOf(mc).toRight(s"Missing receiver for ${mc.method.name}")
        def into(entry: ConfigEntry): Either[String, Settings] =
          receiver.flatMap(r => extract(r, entry :: acc))
        def intoE(entry: Either[String, ConfigEntry]): Either[String, Settings] =
          entry.flatMap(into)

        mc.method.name match {
          case "apply" | "empty"              => Right(new Settings(acc))
          case "enableDiagnostics"            => into(ConfigEntry.EnableDiagnostics)
          case "fieldMatchingCaseInsensitive" => into(ConfigEntry.FieldCaseInsensitive)
          case "enableFallbackToDefaults"     => into(ConfigEntry.EnableFallbackToDefaults)
          case "enumMatchingCaseInsensitive"  => into(ConfigEntry.EnumCaseInsensitive)
          case "recursiveDerivation"          => into(ConfigEntry.EnableRecursiveDerivation)
          case "addField"                     =>
            valuesOf(mc) match {
              case List(outputField, pipe) =>
                intoE(fieldName(outputField).map(out => ConfigEntry.AddField(out, toHExpr(pipe))))
              case _ => Left("Unsupported addField arguments")
            }
          case "renameField" =>
            valuesOf(mc) match {
              case List(inputField, outputField) =>
                intoE(for {
                  in <- fieldName(inputField)
                  out <- fieldName(outputField)
                } yield ConfigEntry.RenameField(in, out))
              case _ => Left("Unsupported renameField arguments")
            }
          case "plugInField" =>
            valuesOf(mc) match {
              case List(inputField, outputField, pipe) =>
                intoE(for {
                  in <- fieldName(inputField)
                  out <- fieldName(outputField)
                } yield ConfigEntry.PlugInField(in, out, toHExpr(pipe)))
              case _ => Left("Unsupported plugInField arguments")
            }
          case "addFallbackToValue" =>
            valuesOf(mc) match {
              case List(fallbackValue) =>
                val fallbackType = typesOf(mc).headOption.getOrElse(fallbackValue.tpe)
                into(ConfigEntry.AddFallbackValue(fallbackType, toHExpr(fallbackValue)))
              case _ => Left("Unsupported addFallbackToValue arguments")
            }
          case "removeSubtype" =>
            (typesOf(mc), valuesOf(mc)) match {
              case (inSubtype :: _, List(pipe)) =>
                into(ConfigEntry.RemoveSubtype(inSubtype, toHExpr(pipe)))
              case _ => Left("Unsupported removeSubtype arguments")
            }
          case "renameSubtype" =>
            typesOf(mc) match {
              case inSubtype :: outSubtype :: _ =>
                into(ConfigEntry.RenameSubtype(inSubtype, outSubtype))
              case _ => Left("Unsupported renameSubtype arguments")
            }
          case "plugInSubtype" =>
            (typesOf(mc), valuesOf(mc)) match {
              case (inSubtype :: outSubtype :: _, List(pipe)) =>
                into(ConfigEntry.PlugInSubtype(inSubtype, outSubtype, toHExpr(pipe)))
              case _ => Left("Unsupported plugInSubtype arguments")
            }
          case other =>
            Left(s"Unsupported PipeDerivationConfig expression: $other")
        }
      case other =>
        Left(s"Unsupported PipeDerivationConfig expression: ${other.plainPrint}")
    }

    extract(DestructuredExpr.parseUntyped(UntypedExpr.fromTyped(code)), Nil) match {
      case Right(settings) => settings
      case Left(err)       => throw new RuntimeException(s"Invalid configuration: $err")
    }
  }
}
