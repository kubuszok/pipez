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

  // ---- Context / Result evidence + the stable-val `pd` (all `gen*` codegen is shared) ----

  implicit override def ctxType: Type[Ctx] =
    c.WeakTypeTag(ctxTpe).asInstanceOf[Type[Ctx]]

  override def resultCtor: Type.Ctor1[Res] =
    Type.Ctor1.fromUntyped[Res](resTpe0.asInstanceOf[UntypedType])

  // The adapter's one sanctioned cast: the raw `pd` (here the stable val injected by postProcessResult, carrying the
  // refined `Aux` type) typed against `Aux[Pipe, Ctx, Res]` so the shared cross-quotes can call its methods.
  override def pdAux: Expr[PipeDerivation.Aux[Pipe, Ctx, Res]] =
    mkExpr(pdStableRef).asInstanceOf[Expr[PipeDerivation.Aux[Pipe, Ctx, Res]]]
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

  // Extracts the `Context` type and the `Result[_]` type CONSTRUCTOR (its member signature, a poly-type like
  // `[X] => Either[List[String], X]`) from the user's `PipeDerivation` instance — the only platform-specific reflection
  // the shared codegen needs (turned into `Type[Ctx]` / `Type.Ctor1[Res]` evidence inside `PipezMacrosImpl2`).
  private def extractConcreteTypes(pdExpr: c.Expr[?]): (c.Type, c.Type) = {
    val pdTree = pdExpr.tree
    val declaredT = pdTree.tpe.widen
    val implTree = c.inferImplicitValue(declaredT, silent = true)
    val pdTpe = if (implTree != EmptyTree) implTree.tpe.widen else declaredT

    val ctxTpe = pdTpe.member(TypeName("Context")).typeSignatureIn(pdTpe).dealias
    val resSig = pdTpe.member(TypeName("Result")).typeSignatureIn(pdTpe)
    (ctxTpe, resSig)
  }

  private def macros[P[_, _]: ConstructorWeakTypeTag, In: WeakTypeTag, Out: WeakTypeTag](
      pipeDerivation: c.Expr[PipeDerivation[P]]
  ) = {
    val (ctxTpe, resSig) = extractConcreteTypes(pipeDerivation)

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
      resTpe0 = resSig,
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
