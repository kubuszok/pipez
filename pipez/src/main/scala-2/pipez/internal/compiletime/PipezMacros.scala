package pipez.internal.compiletime

import hearth.MacroCommonsScala2
import pipez.{Path, PipeDerivation, PipeDerivationConfig}

import scala.annotation.nowarn
import scala.reflect.macros.blackbox

@nowarn("msg=The outer reference in this type test cannot be checked at run time.")
final private[pipez] class PipezMacrosImpl2[P[_, _], Ctx0, Res0[_], In0, Out0](val c: blackbox.Context)(
    pipeTpe: blackbox.Context#Type,
    ctxTpe0: blackbox.Context#Type,
    resTpe0: blackbox.Context#Type,
    resApply0: blackbox.Context#Type => blackbox.Context#Type,
    inTpe: blackbox.Context#Type,
    outTpe: blackbox.Context#Type,
    pd: blackbox.Context#Expr[PipeDerivation.Aux[P, Ctx0, Res0]]
) extends MacroCommonsScala2
    with PipezMacrosImpl
    with PipezConfigParserScala2 {

  import c.universe.*

  private val anyTag: c.WeakTypeTag[Any] = c.universe.weakTypeTag[Any]
  private def mkExpr(tree: Tree): c.Expr[Any] = c.Expr[Any](tree)(anyTag)

  override type Pipe[A, B] = P[A, B]

  implicit override lazy val typeOfAny: Type[Any] =
    c.WeakTypeTag(c.universe.definitions.AnyTpe).asInstanceOf[Type[Any]]

  implicit override lazy val PipeCtor: Type.Ctor2[Pipe] =
    Type.Ctor2.fromUntyped[P](pipeTpe.asInstanceOf[UntypedType]).asInstanceOf[Type.Ctor2[Pipe]]

  // Context and Result types are now concrete — provided by the Aux pattern at the entry point.
  private lazy val ctxTpe: c.Type = ctxTpe0.asInstanceOf[c.Type]
  private lazy val resTpe: c.Type = resTpe0.asInstanceOf[c.Type]

  // Stable val for pipeDerivation — all generated code uses this single reference.
  private val pdStableName: TermName = TermName(c.freshName("pd"))
  private val pdStableRef: Tree = Ident(pdStableName)

  private lazy val isIdentityResult: Boolean = resTpe =:= definitions.AnyTpe

  private lazy val pdInit: Tree = {
    val expr = pd.asInstanceOf[c.Expr[Any]]
    val PipeTc = pipeTpe.asInstanceOf[c.Type]
    if (isIdentityResult) {
      // Identity Result[A] = A — use base type, no Aux needed
      q"""val $pdStableName = $expr"""
    } else {
      val CtxT = ctxTpe
      val ResT = resTpe.typeConstructor
      q"""val $pdStableName: _root_.pipez.PipeDerivation.Aux[$PipeTc, $CtxT, $ResT] = $expr.asInstanceOf[_root_.pipez.PipeDerivation.Aux[$PipeTc, $CtxT, $ResT]]"""
    }
  }

  override lazy val pdExpr: Expr[Any] = mkExpr(pdStableRef).asInstanceOf[Expr[Any]]

  // ---- Code generation implementations ----

  private def tpeOf[A: Type]: c.Type = implicitly[Type[A]].asInstanceOf[c.WeakTypeTag[A]].tpe

  override def generateLift[In: Type, Out: Type](body: (Expr[Any], Expr[Any]) => Expr[Any]): Expr[Pipe[In, Out]] = {
    val inT = tpeOf[In]
    val outT = tpeOf[Out]
    val inName = TermName(c.freshName("in"))
    val ctxName = TermName(c.freshName("ctx"))
    val inRef = c.Expr[Any](Ident(inName))(anyTag)
    val ctxRef = c.Expr[Any](Ident(ctxName))(anyTag)
    val bodyExpr = body(inRef.asInstanceOf[Expr[Any]], ctxRef.asInstanceOf[Expr[Any]])
    val bodyTree = bodyExpr.asInstanceOf[c.Expr[Any]].tree
    val resOutTpe = resApply0(outT).asInstanceOf[c.Type]
    mkExpr(q"""{
      $pdInit
      $pdStableRef.lift[$inT, $outT](($inName: $inT, $ctxName: $ctxTpe) => ($bodyTree).asInstanceOf[$resOutTpe])
    }""").asInstanceOf[Expr[Pipe[In, Out]]]
  }

  override def generateUnlift(pipe: Expr[Any], in: Expr[Any], ctx: Expr[Any]): Expr[Any] = {
    val p = pipe.asInstanceOf[c.Expr[Any]]
    val i = in.asInstanceOf[c.Expr[Any]]
    val ct = ctx.asInstanceOf[c.Expr[Any]]
    mkExpr(q"""$pdStableRef.unlift($p, $i, $ct)""").asInstanceOf[Expr[Any]]
  }

  override def generatePureResult(a: Expr[Any]): Expr[Any] = {
    val ae = a.asInstanceOf[c.Expr[Any]]
    mkExpr(q"""$pdStableRef.pureResult($ae)""").asInstanceOf[Expr[Any]]
  }

  override def generateMergeResults(ctx: Expr[Any], ra: Expr[Any], rb: Expr[Any], f: Expr[Any]): Expr[Any] = {
    val ce = ctx.asInstanceOf[c.Expr[Any]]
    val rae = ra.asInstanceOf[c.Expr[Any]]
    val rbe = rb.asInstanceOf[c.Expr[Any]]
    val fe = f.asInstanceOf[c.Expr[Any]]
    mkExpr(q"""$pdStableRef.mergeResults($ce, $rae, $rbe, $fe)""").asInstanceOf[Expr[Any]]
  }

  override def generateUpdateContext(ctx: Expr[Any], path: Expr[Path]): Expr[Any] = {
    val ce = ctx.asInstanceOf[c.Expr[Any]]
    val pe = path.asInstanceOf[c.Expr[Path]]
    mkExpr(q"""$pdStableRef.updateContext($ce, $pe)""").asInstanceOf[Expr[Any]]
  }

  override def generateBlock(statements: List[Expr[Any]], result: Expr[Any]): Expr[Any] = {
    val stats = statements.map(_.asInstanceOf[c.Expr[Any]].tree)
    val res = result.asInstanceOf[c.Expr[Any]].tree
    mkExpr(q"..$stats; $res").asInstanceOf[Expr[Any]]
  }

  override def generateArrayToConstructorFn[Out: Type](
      outClass: CaseClass[Out],
      lastIndex: Int,
      totalFields: Int
  ): Expr[Any] = {
    val outT = tpeOf[Out]
    val arrName = TermName(c.freshName("arr"))
    val valName = TermName(c.freshName("value"))
    val caName = TermName(c.freshName("ca"))
    val outParams = outClass.primaryConstructor.totalParameters.flatten
    val ctorArgs = outParams.zipWithIndex.map { case ((_, param), idx) =>
      val paramT = param.tpe.Underlying.asInstanceOf[c.WeakTypeTag[Any]].tpe
      q"""$caName($idx).asInstanceOf[$paramT]"""
    }
    val bodyTree = q"""
      val $caName = $arrName.asInstanceOf[_root_.scala.Array[Any]]
      $caName($lastIndex) = $valName
      new $outT(..$ctorArgs)
    """
    val arrParam = ValDef(Modifiers(Flag.PARAM), arrName, tq"Any", EmptyTree)
    val valParam = ValDef(Modifiers(Flag.PARAM), valName, tq"Any", EmptyTree)
    mkExpr(Function(List(arrParam, valParam), bodyTree)).asInstanceOf[Expr[Any]]
  }

  override def generateArrayToBeanFn[Out: Type](
      beanFields: List[(??, Method)],
      defaultConstructor: Method,
      lastIndex: Int,
      totalFields: Int
  ): Expr[Any] = {
    val outT = tpeOf[Out]
    val arrName = TermName(c.freshName("arr"))
    val valName = TermName(c.freshName("value"))
    val beanName = TermName(c.freshName("bean"))
    val caName = TermName(c.freshName("ca"))
    val setterStmts = beanFields.zipWithIndex.map { case ((fieldType, setter), idx) =>
      val paramT = fieldType.Underlying.asInstanceOf[c.WeakTypeTag[Any]].tpe
      val setterName = TermName(setter.name)
      q"""$beanName.$setterName($caName($idx).asInstanceOf[$paramT])"""
    }
    val bodyTree = q"""
      val $caName = $arrName.asInstanceOf[_root_.scala.Array[Any]]
      $caName($lastIndex) = $valName
      val $beanName = new $outT()
      ..$setterStmts
      $beanName
    """
    val arrParam = ValDef(Modifiers(Flag.PARAM), arrName, tq"Any", EmptyTree)
    val valParam = ValDef(Modifiers(Flag.PARAM), valName, tq"Any", EmptyTree)
    mkExpr(Function(List(arrParam, valParam), bodyTree)).asInstanceOf[Expr[Any]]
  }

  // ---- Entry points ----

  private def wrapType[A](tpe: blackbox.Context#Type): Type[A] =
    c.WeakTypeTag(tpe.asInstanceOf[c.universe.Type]).asInstanceOf[Type[A]]

  def doDeriveDef: Expr[Pipe[In0, Out0]] = {
    implicit val inT: Type[In0] = wrapType[In0](inTpe)
    implicit val outT: Type[Out0] = wrapType[Out0](outTpe)
    deriveDefault[In0, Out0]
  }

  def doDeriveConf(config: c.Expr[PipeDerivationConfig[P, In0, Out0]]): Expr[Pipe[In0, Out0]] = {
    implicit val inT: Type[In0] = wrapType[In0](inTpe)
    implicit val outT: Type[Out0] = wrapType[Out0](outTpe)
    deriveConfigured[In0, Out0](config.asInstanceOf[Expr[PipeDerivationConfig[Pipe, In0, Out0]]])
  }
}

final class PipezMacro(val c: blackbox.Context) {

  import c.universe.*

  type ConstructorWeakTypeTag[F[_, _]] = WeakTypeTag[F[Any, Nothing]]

  private def extractConcreteTypes(pdExpr: c.Expr[?]): (c.Type, c.Type => c.Type) = {
    val pdTree = pdExpr.tree
    val declaredT = pdTree.tpe.widen
    // Re-summon the implicit to get its full refined type (including Aux type members)
    val implTree = c.inferImplicitValue(declaredT, silent = true)
    val pdTpe = if (implTree != EmptyTree) implTree.tpe.widen else declaredT

    val ctxMember = pdTpe.member(TypeName("Context"))
    val ctxTpe = ctxMember.typeSignatureIn(pdTpe).dealias

    val resMember = pdTpe.member(TypeName("Result"))
    val resSig = resMember.typeSignatureIn(pdTpe)
    // For `type Result[A] = Either[..., A]`, resSig is a PolyType with resultType = Either[..., A]
    // For `type Result[A] = A`, resSig is a PolyType with resultType = A (the type param itself)
    // Use dealias to resolve through type aliases
    // Build a function: given a c.Type for Out, produce the concrete Result[Out] type.
    // This handles all cases: Result[A] = A (identity), Result[A] = Either[E, A] (partial application), etc.
    val resApply: c.Type => c.Type = resSig.dealias match {
      case pt if pt.typeParams.nonEmpty =>
        val tp = pt.typeParams.head
        val rt = pt.resultType.dealias
        (outTpe: c.Type) => rt.substituteTypes(List(tp), List(outTpe))
      case _ =>
        // Abstract — no concrete resolution available, fall back to Any (erased at runtime)
        (_: c.Type) => definitions.AnyTpe
    }
    (ctxTpe, resApply)
  }

  private def macros[P[_, _]: ConstructorWeakTypeTag, In: WeakTypeTag, Out: WeakTypeTag](
      pipeDerivation: c.Expr[PipeDerivation[P]]
  ) = {
    val (ctxTpe, resApply) = extractConcreteTypes(pipeDerivation)
    val resIntTpe = resApply(definitions.IntTpe)
    // For identity Result[A] = A, resIntTpe = Int, typeConstructor gives Int (0 params)
    // Use definitions.AnyTpe as sentinel for identity case
    val resTpeForAux =
      if (resIntTpe =:= definitions.IntTpe) definitions.AnyTpe
      else resIntTpe.typeConstructor
    new PipezMacrosImpl2[P, Any, ({ type R[A] = Any })#R, In, Out](c)(
      pipeTpe = c.weakTypeOf[P[Any, Nothing]].typeConstructor,
      ctxTpe0 = ctxTpe,
      resTpe0 = resTpeForAux,
      resApply0 = resApply.asInstanceOf[blackbox.Context#Type => blackbox.Context#Type],
      inTpe = c.weakTypeOf[In],
      outTpe = c.weakTypeOf[Out],
      pd = pipeDerivation.asInstanceOf[c.Expr[PipeDerivation.Aux[P, Any, ({ type R[A] = Any })#R]]]
    )
  }

  def deriveDefault[P[_, _]: ConstructorWeakTypeTag, In: WeakTypeTag, Out: WeakTypeTag](
      pipeDerivation: c.Expr[PipeDerivation[P]]
  ): c.Expr[P[In, Out]] = {
    val m = macros[P, In, Out](pipeDerivation)
    m.doDeriveDef.asInstanceOf[c.Expr[P[In, Out]]]
  }

  def deriveConfigured[P[_, _]: ConstructorWeakTypeTag, In: WeakTypeTag, Out: WeakTypeTag](
      config: c.Expr[PipeDerivationConfig[P, In, Out]]
  )(
      pipeDerivation: c.Expr[PipeDerivation[P]]
  ): c.Expr[P[In, Out]] = {
    val m = macros[P, In, Out](pipeDerivation)
    m.doDeriveConf(config.asInstanceOf[m.c.Expr[PipeDerivationConfig[P, In, Out]]]).asInstanceOf[c.Expr[P[In, Out]]]
  }
}
