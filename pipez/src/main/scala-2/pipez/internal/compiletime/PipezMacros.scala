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
    pdRefinedType0: blackbox.Context#Type,
    inTpe: blackbox.Context#Type,
    outTpe: blackbox.Context#Type,
    pd: blackbox.Context#Expr[PipeDerivation.Aux[P, Ctx0, Res0]]
) extends MacroCommonsScala2
    with PipezMacrosImpl {

  import c.universe.*

  private val anyTag: c.WeakTypeTag[Any] = c.universe.weakTypeTag[Any]
  private def mkExpr(tree: Tree): c.Expr[Any] = c.Expr[Any](tree)(anyTag)

  override type Pipe[A, B] = P[A, B]

  implicit override lazy val typeOfAny: Type[Any] =
    c.WeakTypeTag(c.universe.definitions.AnyTpe).asInstanceOf[Type[Any]]

  implicit override lazy val PipeCtor: Type.Ctor2[Pipe] =
    Type.Ctor2.fromUntyped[P](pipeTpe.asInstanceOf[UntypedType]).asInstanceOf[Type.Ctor2[Pipe]]

  private lazy val ctxTpe: c.Type = ctxTpe0.asInstanceOf[c.Type]
  private lazy val pdDeclaredType: c.Type = pdRefinedType0.asInstanceOf[c.Type]

  // Stable val name for the PipeDerivation instance. The val definition is injected
  // by postProcessResult to ensure it's in the same block as cached defs from
  // toValDefs.use. All code gen methods reference this name, and c.typecheck in
  // postProcessResult resolves it.
  private val pdStableName: TermName = TermName(c.freshName("pd"))
  private val pdStableRef: Tree = Ident(pdStableName)

  override lazy val pdExpr: Expr[Any] = mkExpr(pdStableRef).asInstanceOf[Expr[Any]]

  override protected lazy val ignoredImplicitMethods: Seq[UntypedMethod] = {
    val pipeCtorType = PipezMacrosImpl2.this.pipeTpe.asInstanceOf[c.Type]
    val companionSym = pipeCtorType.typeSymbol.companion
    if (companionSym != c.universe.NoSymbol) {
      val companionTag: Type[Any] = c.WeakTypeTag(companionSym.info).asInstanceOf[Type[Any]]
      companionTag.methods.collect {
        case m if m.name == "deriveAutomatic" || m.name == "derive" => m.asUntyped
      }.toSeq
    } else Seq.empty
  }

  override def postProcessResult[A: Type](expr: Expr[A]): Expr[A] = {
    val tree = expr.asInstanceOf[c.Expr[Any]].tree
    val tpe = implicitly[Type[A]].asInstanceOf[c.WeakTypeTag[A]].tpe
    val pdTree = pd.asInstanceOf[c.Expr[Any]].tree
    val pdInit = q"""val $pdStableName: $pdDeclaredType = $pdTree"""
    val wrapped = tree match {
      case Block(stats, expr) => Block(pdInit :: stats, expr)
      case _                  => Block(List(pdInit), tree)
    }
    // Typecheck to resolve path-dependent types through the stable val, then
    // untypecheck so the outer compiler retypechecks with fresh context.
    // This avoids backend errors (key not found) from abstract type projections.
    val checked = c.typecheck(wrapped)
    val untyped = c.untypecheck(checked)
    c.Expr(untyped)(c.WeakTypeTag(tpe)).asInstanceOf[Expr[A]]
  }

  // ---- Code generation implementations ----

  private def tpeOf[A: Type]: c.Type = implicitly[Type[A]].asInstanceOf[c.WeakTypeTag[A]].tpe
  private def treeOf(e: Expr[?]): Tree = e.asInstanceOf[c.Expr[Any]].tree
  private def mkTyped[A: Type](tree: Tree): Expr[A] =
    c.Expr[A](tree)(implicitly[Type[A]].asInstanceOf[c.WeakTypeTag[A]]).asInstanceOf[Expr[A]]

  // `Ctx`/`Res` stay abstract (inherited); the evidence carries the real extracted Context/Result types. The generated
  // trees themselves are typed against the stable val's `pd.Context` / `pd.Result[A]` path-dependent members.
  implicit override def ctxType: Type[Ctx] =
    c.WeakTypeTag(ctxTpe).asInstanceOf[Type[Ctx]]
  override def resType[A: Type]: Type[Res[A]] =
    c.WeakTypeTag(resApply0(tpeOf[A]).asInstanceOf[c.Type]).asInstanceOf[Type[Res[A]]]

  override def generateLift[In: Type, Out: Type](
      body: (Expr[In], Expr[Ctx]) => Expr[Res[Out]]
  ): Expr[Pipe[In, Out]] = {
    val inT = tpeOf[In]
    val outT = tpeOf[Out]
    val inName = TermName(c.freshName("in"))
    val ctxName = TermName(c.freshName("ctx"))
    val inRef = mkTyped[In](Ident(inName))
    val ctxRef = mkTyped[Ctx](Ident(ctxName))
    val bodyTree = treeOf(body(inRef, ctxRef))
    val finalBody = q"""($bodyTree).asInstanceOf[$pdStableRef.Result[$outT]]"""
    val tree = q"""$pdStableRef.lift[$inT, $outT](($inName: $inT, $ctxName: $pdStableRef.Context) => $finalBody)"""
    mkTyped[Pipe[In, Out]](tree)(pipeType[In, Out])
  }

  private val ctxCastTpe = tq"$pdStableRef.Context"

  override def generateUnlift[In: Type, Out: Type](
      pipe: Expr[Pipe[In, Out]],
      in: Expr[In],
      ctx: Expr[Ctx]
  ): Expr[Res[Out]] =
    mkTyped[Res[Out]](
      q"""$pdStableRef.unlift(${treeOf(pipe)}, ${treeOf(in)}, ${treeOf(ctx)}.asInstanceOf[$ctxCastTpe])"""
    )

  override def generatePureResult[A: Type](a: Expr[A]): Expr[Res[A]] =
    mkTyped[Res[A]](q"""$pdStableRef.pureResult(${treeOf(a)})""")

  override def generateMergeResults[A: Type, B: Type, C: Type](
      ctx: Expr[Ctx],
      ra: Expr[Res[A]],
      rb: Expr[Res[B]],
      f: Expr[(A, B) => C]
  ): Expr[Res[C]] =
    mkTyped[Res[C]](
      q"""$pdStableRef.mergeResults(${treeOf(ctx)}.asInstanceOf[$ctxCastTpe], ${treeOf(ra)}, ${treeOf(rb)}, ${treeOf(
          f
        )})"""
    )

  override def generateUpdateContext(ctx: Expr[Ctx], path: Expr[Path]): Expr[Ctx] =
    mkTyped[Ctx](q"""$pdStableRef.updateContext(${treeOf(ctx)}.asInstanceOf[$ctxCastTpe], ${treeOf(path)})""")

  override def generateArrayToConstructorFn[Out: Type](
      outClass: CaseClass[Out],
      lastIndex: Int,
      totalFields: Int
  ): Expr[(Array[Any], Unit) => Out] = {
    val outT = tpeOf[Out]
    val arrName = TermName(c.freshName("arr"))
    val valName = TermName(c.freshName("value"))
    val outParams = outClass.primaryConstructor.totalParameters.flatten
    val ctorArgs = outParams.zipWithIndex.map { case ((_, param), idx) =>
      val paramT = param.tpe.Underlying.asInstanceOf[c.WeakTypeTag[Any]].tpe
      q"""$arrName($idx).asInstanceOf[$paramT]"""
    }
    val storeStmts = if (lastIndex >= 0) List(q"""$arrName($lastIndex) = $valName""") else Nil
    val bodyTree = q"""
      ..$storeStmts
      new $outT(..$ctorArgs)
    """
    val arrParam = ValDef(Modifiers(Flag.PARAM), arrName, tq"_root_.scala.Array[Any]", EmptyTree)
    val valParam = ValDef(Modifiers(Flag.PARAM), valName, tq"_root_.scala.Unit", EmptyTree)
    mkTyped[(Array[Any], Unit) => Out](Function(List(arrParam, valParam), bodyTree))(Type.of[(Array[Any], Unit) => Out])
  }

  override def generateArrayToBeanFn[Out: Type](
      beanFields: List[(??, Method)],
      defaultConstructor: Method,
      lastIndex: Int,
      totalFields: Int
  ): Expr[(Array[Any], Unit) => Out] = {
    val outT = tpeOf[Out]
    val arrName = TermName(c.freshName("arr"))
    val valName = TermName(c.freshName("value"))
    val beanName = TermName(c.freshName("bean"))
    val setterStmts = beanFields.zipWithIndex.map { case ((fieldType, setter), idx) =>
      val paramT = fieldType.Underlying.asInstanceOf[c.WeakTypeTag[Any]].tpe
      val setterName = TermName(setter.name)
      q"""$beanName.$setterName($arrName($idx).asInstanceOf[$paramT])"""
    }
    val storeStmts2 = if (lastIndex >= 0) List(q"""$arrName($lastIndex) = $valName""") else Nil
    val bodyTree = q"""
      ..$storeStmts2
      val $beanName = new $outT()
      ..$setterStmts
      $beanName
    """
    val arrParam = ValDef(Modifiers(Flag.PARAM), arrName, tq"_root_.scala.Array[Any]", EmptyTree)
    val valParam = ValDef(Modifiers(Flag.PARAM), valName, tq"_root_.scala.Unit", EmptyTree)
    mkTyped[(Array[Any], Unit) => Out](Function(List(arrParam, valParam), bodyTree))(Type.of[(Array[Any], Unit) => Out])
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
    val implTree = c.inferImplicitValue(declaredT, silent = true)
    val pdTpe = if (implTree != EmptyTree) implTree.tpe.widen else declaredT

    val ctxMember = pdTpe.member(TypeName("Context"))
    val ctxTpe = ctxMember.typeSignatureIn(pdTpe).dealias

    val resMember = pdTpe.member(TypeName("Result"))
    val resSig = resMember.typeSignatureIn(pdTpe)

    val resApply: c.Type => c.Type = resSig.dealias match {
      case pt if pt.typeParams.nonEmpty =>
        val tp = pt.typeParams.head
        val rt = pt.resultType.dealias
        if (rt.isInstanceOf[scala.reflect.internal.Types#TypeBounds] || rt.typeSymbol == tp)
          (outTpe: c.Type) => outTpe
        else (outTpe: c.Type) => rt.substituteTypes(List(tp), List(outTpe))
      case _ =>
        (_: c.Type) => definitions.AnyTpe
    }
    (ctxTpe, resApply)
  }

  private def macros[P[_, _]: ConstructorWeakTypeTag, In: WeakTypeTag, Out: WeakTypeTag](
      pipeDerivation: c.Expr[PipeDerivation[P]]
  ) = {
    val (ctxTpe, resApply) = extractConcreteTypes(pipeDerivation)
    val resIntTpe = resApply(definitions.IntTpe)
    val resTpeForAux =
      if (resIntTpe =:= definitions.IntTpe) definitions.AnyTpe
      else resIntTpe.typeConstructor

    val pdTree = pipeDerivation.tree
    val declaredT = pdTree.tpe.widen
    val implTree = c.inferImplicitValue(declaredT, silent = true)
    val pdRefinedType =
      if (implTree != EmptyTree && !(implTree.tpe.widen =:= declaredT))
        implTree.tpe.widen
      else declaredT

    new PipezMacrosImpl2[P, Any, ({ type R[A] = Any })#R, In, Out](c)(
      pipeTpe = c.weakTypeOf[P[Any, Nothing]].typeConstructor,
      ctxTpe0 = ctxTpe,
      resTpe0 = resTpeForAux,
      resApply0 = resApply.asInstanceOf[blackbox.Context#Type => blackbox.Context#Type],
      pdRefinedType0 = pdRefinedType,
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
