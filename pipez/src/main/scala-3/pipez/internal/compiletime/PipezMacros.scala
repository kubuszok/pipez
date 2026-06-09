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
  // All methods use Select/Apply tree construction to avoid needing SQType[Res0]
  // in quote blocks, since Res0 is a HKT that cannot be represented as a quoted Type.

  override def generateLift[In: Type, Out: Type](
      body: (Expr[Any], Expr[Any]) => Expr[Any]
  ): Expr[Pipe[In, Out]] = {
    given SQType[P] = pipeTypeQ
    @nowarn("msg=Infinite loop") given SQType[In] = summon[Type[In]].asInstanceOf[SQType[In]]
    @nowarn("msg=Infinite loop") given SQType[Out] = summon[Type[Out]].asInstanceOf[SQType[Out]]
    val pd = pdTyped

    '{
      $pd.lift[In, Out] { (in: In, ctx: Ctx0) =>
        val _in = in
        val _ctx = ctx
        ${
          body(
            '{ _in }.asInstanceOf[Expr[Any]],
            '{ _ctx }.asInstanceOf[Expr[Any]]
          ).asInstanceOf[Expr[Res0[Out]]]
        }
      }
    }.asInstanceOf[Expr[Pipe[In, Out]]]
  }

  override def generateUnlift(
      pipe: Expr[Any],
      in: Expr[Any],
      ctx: Expr[Any]
  ): Expr[Any] = {
    given SQType[P] = pipeTypeQ
    val pd = pdTyped
    val pipeExpr = pipe.asInstanceOf[Expr[P[Any, Any]]]
    val inExpr = in.asInstanceOf[Expr[Any]]
    val ctxExpr = ctx.asInstanceOf[Expr[Ctx0]]

    '{ $pd.unlift[Any, Any]($pipeExpr, $inExpr, $ctxExpr) }.asInstanceOf[Expr[Any]]
  }

  override def generatePureResult(a: Expr[Any]): Expr[Any] = {
    given SQType[P] = pipeTypeQ
    val pd = pdTyped
    val aExpr = a.asInstanceOf[Expr[Any]]

    '{ $pd.pureResult[Any]($aExpr) }.asInstanceOf[Expr[Any]]
  }

  override def generateMergeResults(
      ctx: Expr[Any],
      ra: Expr[Any],
      rb: Expr[Any],
      f: Expr[Any]
  ): Expr[Any] = {
    given SQType[P] = pipeTypeQ
    val pd = pdTyped
    val ctxExpr = ctx.asInstanceOf[Expr[Ctx0]]
    val raExpr = ra.asInstanceOf[Expr[Res0[Any]]]
    val rbExpr = rb.asInstanceOf[Expr[Res0[Any]]]
    val fExpr = f.asInstanceOf[Expr[(Any, Any) => Any]]

    '{ $pd.mergeResults[Any, Any, Any]($ctxExpr, $raExpr, $rbExpr, $fExpr) }.asInstanceOf[Expr[Any]]
  }

  override def generateUpdateContext(ctx: Expr[Any], path: Expr[Path]): Expr[Any] = {
    given SQType[P] = pipeTypeQ
    val pd = pdTyped
    val ctxExpr = ctx.asInstanceOf[Expr[Ctx0]]
    val pExpr = path.asInstanceOf[Expr[Path]]

    '{ $pd.updateContext($ctxExpr, $pExpr) }.asInstanceOf[Expr[Any]]
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
