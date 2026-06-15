package pipez.internal.compiletime

import hearth.MacroCommonsScala3
import pipez.{Path, PipeDerivation, PipeDerivationConfig}

import scala.annotation.nowarn

import scala.quoted.{Expr as SQExpr, Quotes, Type as SQType}

final private[pipez] class PipezMacros[P[_, _], In0, Out0](q: Quotes)(
    pipeTypeQ: SQType[P],
    inTypeQ: SQType[In0],
    outTypeQ: SQType[Out0],
    pdQ: SQExpr[PipeDerivation[P]]
) extends MacroCommonsScala3(using q),
      PipezMacrosImpl {

  import quotes.*
  import quotes.reflect.*

  override type Pipe[A, B] = P[A, B]

  @nowarn("msg=unused") implicit override lazy val typeOfAny: Type[Any] =
    SQType.of[Any](using quotes).asInstanceOf[Type[Any]]

  @nowarn("msg=Infinite loop|unused") implicit override lazy val PipeCtor: Type.Ctor2[Pipe] = {
    val pipeTypeRepr = TypeRepr.of(using pipeTypeQ)
    Type.Ctor2.fromUntyped[P](pipeTypeRepr.asInstanceOf[UntypedType]).asInstanceOf[Type.Ctor2[Pipe]]
  }

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

  override protected lazy val ignoredImplicitMethods: Seq[UntypedMethod] = {
    val pipeTypeRepr = TypeRepr.of(using pipeTypeQ)
    val companionModule = pipeTypeRepr.typeSymbol.companionModule
    if companionModule != Symbol.noSymbol then {
      val companionType: Type[Any] =
        companionModule.moduleClass.typeRef.asType.asInstanceOf[scala.quoted.Type[Any]].asInstanceOf[Type[Any]]
      companionType.methods.collect {
        case m if m.name == "deriveAutomatic" || m.name == "derive" => m.asUntyped
      }.toSeq
    } else Seq.empty
  }

  // Scala 3 needs no stable val / `postProcessResult` dance: `scala.quoted` constructs the refined `Aux` type
  // (including the higher-kinded `Result`) natively inside the quote.
  private def pdTyped: Expr[PipeDerivation.Aux[P, Ctx0, Res0]] = {
    given SQType[P] = pipeTypeQ
    '{ $pdQ.asInstanceOf[PipeDerivation.Aux[P, Ctx0, Res0]] }.asInstanceOf[Expr[PipeDerivation.Aux[P, Ctx0, Res0]]]
  }

  // ---- Context / Result evidence + the typed `pd` (all `gen*` codegen is shared) ----
  // `Ctx`/`Res` stay abstract (inherited); the `Type`/`Ctor` evidence carries the real extracted types, which the
  // shared codegen turns into the proper trees via block-level implicits.

  implicit override def ctxType: Type[Ctx] = ctxSQType.asInstanceOf[Type[Ctx]]

  override def resultCtor: Type.Ctor1[Res] =
    Type.Ctor1.fromUntyped[Res](resCtorUntypedType.asInstanceOf[UntypedType])

  // The adapter's one sanctioned cast: the raw `pd` (with path-dependent Context/Result) typed against the stable
  // `Aux[Pipe, Ctx, Res]` carried by the Ctor/Type evidence. All `gen*` codegen is shared from here on.
  override def pdAux: Expr[PipeDerivation.Aux[Pipe, Ctx, Res]] =
    pdTyped.asInstanceOf[Expr[PipeDerivation.Aux[Pipe, Ctx, Res]]]

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
