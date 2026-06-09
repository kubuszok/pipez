package pipez.internal.compiletime

import hearth.MacroCommonsScala3
import pipez.{Path, PipeDerivation, PipeDerivationConfig}

import scala.annotation.nowarn

// Import scala.quoted types under aliases to avoid cross-quotes plugin interference
import scala.quoted.{Expr as SQExpr, Quotes, Type as SQType}

final private[pipez] class PipezMacros[P[_, _], In0, Out0](q: Quotes)(
    pipeTypeQ: SQType[P],
    inTypeQ: SQType[In0],
    outTypeQ: SQType[Out0],
    pdQ: SQExpr[PipeDerivation[P]]
) extends MacroCommonsScala3(using q),
      PipezMacrosImpl,
      PipezConfigParserScala3 {

  import quotes.*
  import quotes.reflect.*

  override type Pipe[A, B] = P[A, B]

  private def debugLog(msg: String): Unit = {
    val f = new java.io.FileWriter("/tmp/pipez-debug.log", true)
    f.write(s"[${java.time.Instant.now}] BRIDGE: $msg\n")
    f.close()
  }

  // Override to avoid cross-quotes plugin recursive implicit self-reference.
  // Use direct scala.quoted.Type.of instead of hearth's cross-quotes Type.of.
  @nowarn("msg=unused") implicit override lazy val typeOfAny: Type[Any] =
    SQType.of[Any](using quotes).asInstanceOf[Type[Any]]

  @nowarn("msg=Infinite loop|unused") implicit override lazy val PipeCtor: Type.Ctor2[Pipe] = {
    debugLog("PipeCtor init start")
    val pipeTypeRepr = TypeRepr.of(using pipeTypeQ)
    debugLog(s"PipeCtor typeRepr: $pipeTypeRepr")
    val result = Type.Ctor2.fromUntyped[P](pipeTypeRepr.asInstanceOf[UntypedType]).asInstanceOf[Type.Ctor2[Pipe]]
    debugLog("PipeCtor init done")
    result
  }

  // Extract Context and Result types from PipeDerivation
  private val (ctxQType, resCtorUntypedType) = {
    given SQType[P] = pipeTypeQ
    val ctxTpe = '{ $pdQ.updateContext(???, ???) }.asTerm.tpe.widen
    val AppliedType(resTpe, _) = '{ $pdQ.pureResult(1) }.asTerm.tpe.widen: @unchecked
    (ctxTpe, resTpe)
  }

  private type Ctx0
  private type Res0[_]

  private given ctxSQType: SQType[Ctx0] =
    ctxQType.asType.asInstanceOf[SQType[Ctx0]]

  @nowarn("msg=unused") private given resSQType: SQType[Res0[Any]] =
    resCtorUntypedType.appliedTo(TypeRepr.of[Any]).asType.asInstanceOf[SQType[Res0[Any]]]

  private given resCtorSQType: SQType[Res0] =
    resCtorUntypedType.asType.asInstanceOf[SQType[Res0]]

  override lazy val pdExpr: Expr[Any] = {
    given SQType[P] = pipeTypeQ
    '{ $pdQ.asInstanceOf[PipeDerivation.Aux[P, Ctx0, Res0]] }.asInstanceOf[Expr[Any]]
  }

  private def pdTyped: Expr[PipeDerivation.Aux[P, Ctx0, Res0]] =
    pdExpr.asInstanceOf[Expr[PipeDerivation.Aux[P, Ctx0, Res0]]]

  // ---- Code generation implementations ----

  override def generateLift[In: Type, Out: Type](
      body: (Expr[Any], Expr[Any]) => Expr[Any]
  ): Expr[Pipe[In, Out]] =
    // Extract implicit types in a separate block to avoid recursive given self-reference (SOE)
    // and forward reference errors between the val and given definitions
    generateLiftImpl[In, Out](
      summon[Type[In]].asInstanceOf[SQType[In]],
      summon[Type[Out]].asInstanceOf[SQType[Out]],
      body
    )

  private def generateLiftImpl[In, Out](
      inSQType: SQType[In],
      outSQType: SQType[Out],
      body: (Expr[Any], Expr[Any]) => Expr[Any]
  ): Expr[Pipe[In, Out]] = {
    given SQType[P] = pipeTypeQ
    given SQType[In] = inSQType
    given SQType[Out] = outSQType
    val pd = pdTyped

    '{
      $pd.lift[In, Out] { (in: In, ctx: Ctx0) =>
        val _in = in
        val _ctx = ctx
        ${
          val bodyResult = body(
            '{ _in }.asInstanceOf[Expr[Any]],
            '{ _ctx }.asInstanceOf[Expr[Any]]
          )
          // Insert tree-level asInstanceOf to cast Result[Any] to Result[Out]
          val bodyTerm = bodyResult.asInstanceOf[SQExpr[Any]].asTerm
          val expectedType = resCtorUntypedType.appliedTo(TypeRepr.of[Out])
          val casted = TypeApply(Select.unique(bodyTerm, "asInstanceOf"), List(Inferred(expectedType)))
          casted.asExpr.asInstanceOf[Expr[Res0[Out]]]
        }
      }
    }.asInstanceOf[Expr[Pipe[In, Out]]]
  }

  // Use tree construction with proper type handling to avoid ScopeException.
  // The key insight: expressions from one splice (like '{ _ctx }) can be used in
  // tree construction (Select/Apply) without triggering ScopeException, as long as
  // we don't create new quote blocks that would check splice ownership.

  private def pdTerm: Term = pdExpr.asInstanceOf[SQExpr[Any]].asTerm

  private def toTerm(e: Expr[Any]): Term = e.asInstanceOf[SQExpr[Any]].asTerm

  override def generateUnlift(
      pipe: Expr[Any],
      in: Expr[Any],
      ctx: Expr[Any]
  ): Expr[Any] = {
    val anyTR = TypeRepr.of[Any]
    val pipeTR = PipeCtor.apply[Any, Any](using typeOfAny, typeOfAny).asInstanceOf[SQType[?]] match {
      case '[t] => TypeRepr.of[t]
    }
    // pd.unlift[Any, Any](pipe.asInstanceOf[Pipe[Any, Any]], in, ctx)
    val unliftSel = pdTerm.select(pdTerm.tpe.widen.typeSymbol.methodMember("unlift").head)
    val pipeCast = TypeApply(Select.unique(toTerm(pipe), "asInstanceOf"), List(Inferred(pipeTR)))
    val ctxCast = TypeApply(Select.unique(toTerm(ctx), "asInstanceOf"), List(TypeTree.of(using ctxSQType)))
    val applied = unliftSel
      .appliedToTypes(List(anyTR, anyTR))
      .appliedToArgs(List(pipeCast, toTerm(in), ctxCast))
    applied.asExpr.asInstanceOf[Expr[Any]]
  }

  override def generatePureResult(a: Expr[Any]): Expr[Any] = {
    val anyTR = TypeRepr.of[Any]
    // pd.pureResult[Any](a) using tree construction to stay in same splice context
    val pureResultSel = pdTerm.select(pdTerm.tpe.widen.typeSymbol.methodMember("pureResult").head)
    val applied = pureResultSel
      .appliedToType(anyTR)
      .appliedTo(toTerm(a))
    applied.asExpr.asInstanceOf[Expr[Any]]
  }

  override def generateMergeResults(
      ctx: Expr[Any],
      ra: Expr[Any],
      rb: Expr[Any],
      f: Expr[Any]
  ): Expr[Any] = {
    val anyTR = TypeRepr.of[Any]
    val resTR = resCtorUntypedType.appliedTo(anyTR)
    val fn2TR = TypeRepr.of[(Any, Any) => Any]
    // pd.mergeResults[Any, Any, Any](ctx, ra, rb, f)
    val mergeResultsSel = pdTerm.select(pdTerm.tpe.widen.typeSymbol.methodMember("mergeResults").head)
    // Cast arguments to expected types
    val ctxCast = TypeApply(Select.unique(toTerm(ctx), "asInstanceOf"), List(TypeTree.of(using ctxSQType)))
    val raCast = TypeApply(Select.unique(toTerm(ra), "asInstanceOf"), List(Inferred(resTR)))
    val rbCast = TypeApply(Select.unique(toTerm(rb), "asInstanceOf"), List(Inferred(resTR)))
    val fCast = TypeApply(Select.unique(toTerm(f), "asInstanceOf"), List(Inferred(fn2TR)))
    val applied = mergeResultsSel
      .appliedToTypes(List(anyTR, anyTR, anyTR))
      .appliedToArgs(List(ctxCast, raCast, rbCast, fCast))
    applied.asExpr.asInstanceOf[Expr[Any]]
  }

  override def generateUpdateContext(ctx: Expr[Any], path: Expr[Path]): Expr[Any] = {
    val ctxCast = TypeApply(Select.unique(toTerm(ctx), "asInstanceOf"), List(TypeTree.of(using ctxSQType)))
    val updateContextSel = pdTerm.select(pdTerm.tpe.widen.typeSymbol.methodMember("updateContext").head)
    val applied = updateContextSel.appliedToArgs(List(ctxCast, toTerm(path)))
    applied.asExpr.asInstanceOf[Expr[Any]]
  }

  // ---- Forwarding entry points ----

  def doDeriveDef: Expr[Pipe[In0, Out0]] = {
    implicit val inT: Type[In0] = inTypeQ.asInstanceOf[Type[In0]]
    implicit val outT: Type[Out0] = outTypeQ.asInstanceOf[Type[Out0]]
    deriveDefault[In0, Out0]
  }

  def doDeriveConf(config: Expr[PipeDerivationConfig[P, In0, Out0]]): Expr[Pipe[In0, Out0]] = {
    implicit val inT: Type[In0] = inTypeQ.asInstanceOf[Type[In0]]
    implicit val outT: Type[Out0] = outTypeQ.asInstanceOf[Type[Out0]]
    deriveConfigured[In0, Out0](config.asInstanceOf[Expr[PipeDerivationConfig[Pipe, In0, Out0]]])
  }
}

private[pipez] object PipezMacros {

  def deriveDefault[P[_, _], In, Out](
      pd: SQExpr[PipeDerivation[P]]
  )(using q: Quotes, pType: SQType[P], inType: SQType[In], outType: SQType[Out]): SQExpr[P[In, Out]] =
    new PipezMacros[P, In, Out](q)(pType, inType, outType, pd).doDeriveDef.asInstanceOf[SQExpr[P[In, Out]]]

  def deriveConfigured[P[_, _], In, Out](
      config: SQExpr[PipeDerivationConfig[P, In, Out]]
  )(
      pd: SQExpr[PipeDerivation[P]]
  )(using q: Quotes, pType: SQType[P], inType: SQType[In], outType: SQType[Out]): SQExpr[P[In, Out]] =
    new PipezMacros[P, In, Out](q)(pType, inType, outType, pd).doDeriveConf(config).asInstanceOf[SQExpr[P[In, Out]]]
}
