package pipez.internal.compiletime

import hearth.MacroCommonsScala2
import pipez.{Path, PipeDerivation, PipeDerivationConfig}

import scala.annotation.nowarn
import scala.reflect.macros.blackbox

@nowarn("msg=The outer reference in this type test cannot be checked at run time.")
final private[pipez] class PipezMacrosImpl2[P[_, _], In0, Out0](val c: blackbox.Context)(
    pipeTpe: blackbox.Context#Type,
    inTpe: blackbox.Context#Type,
    outTpe: blackbox.Context#Type,
    pd: blackbox.Context#Expr[PipeDerivation[P]]
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

  // ---- Context / Result extracted IN the macro (mirrors what Scala 3 does natively inside quotes) ----
  // `PipeDerivation#Context`/`#Result[_]` are abstract members, concrete only in the user's instance. Scala 2 cannot
  // read them through a cross-quote (`pd.pureResult(1)` would leak a path-dependent `pd.Result` stabilizer into the
  // emitted tree), so we re-resolve the implicit to recover its refined `Aux` type and read the members off it with
  // plain `c.universe` reflection. No pre-extracted types are threaded through the constructor.

  private lazy val pdDeclaredType: c.Type = {
    val declaredT = pd.asInstanceOf[c.Expr[Any]].tree.tpe.widen
    val implTree = c.inferImplicitValue(declaredT, silent = true)
    if (implTree != EmptyTree) implTree.tpe.widen else declaredT
  }
  private lazy val ctxTpe: c.Type =
    pdDeclaredType.member(TypeName("Context")).typeSignatureIn(pdDeclaredType).dealias
  private lazy val resSig: c.Type =
    pdDeclaredType.member(TypeName("Result")).typeSignatureIn(pdDeclaredType)

  implicit override lazy val ctxType: Type[Ctx] =
    c.WeakTypeTag(ctxTpe).asInstanceOf[Type[Ctx]]
  override lazy val resultCtor: Type.Ctor1[Res] =
    Type.Ctor1.fromUntyped[Res](resSig.asInstanceOf[UntypedType])

  // The instance needs a STABLE reference for the codegen cross-quotes to stabilize the path-dependent
  // Context/Result; the val is injected by `postProcessResult` so it sits in the same block as the cached defs from
  // `toValDefs.use`, ascribed with the refined `Aux` type so `pd.Result`/`pd.Context` resolve.
  private val pdStableName: TermName = TermName(c.freshName("pd"))
  private val pdStableRef: Tree = Ident(pdStableName)

  // The one sanctioned cast: the stable val (carrying the refined `Aux` type) typed against `Aux[Pipe, Ctx, Res]`, so
  // all the shared `gen*` codegen can call its methods without further casting.
  override def pdAux: Expr[PipeDerivation.Aux[Pipe, Ctx, Res]] =
    mkExpr(pdStableRef).asInstanceOf[Expr[PipeDerivation.Aux[Pipe, Ctx, Res]]]

  override protected lazy val ignoredImplicitMethods: Seq[UntypedMethod] = {
    val companionSym = pipeTpe.asInstanceOf[c.Type].typeSymbol.companion
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
      case Block(stats, e) => Block(pdInit :: stats, e)
      case _               => Block(List(pdInit), tree)
    }
    // Typecheck to resolve the path-dependent projections through the stable val, then untypecheck so the outer
    // compiler retypechecks cleanly (avoids backend "key not found" from abstract type projections).
    c.Expr(c.untypecheck(c.typecheck(wrapped)))(c.WeakTypeTag(tpe)).asInstanceOf[Expr[A]]
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

  private def macros[P[_, _]: ConstructorWeakTypeTag, In: WeakTypeTag, Out: WeakTypeTag](
      pipeDerivation: c.Expr[PipeDerivation[P]]
  ) =
    new PipezMacrosImpl2[P, In, Out](c)(
      pipeTpe = c.weakTypeOf[P[Any, Nothing]].typeConstructor,
      inTpe = c.weakTypeOf[In],
      outTpe = c.weakTypeOf[Out],
      pd = pipeDerivation
    )

  def deriveDefault[P[_, _]: ConstructorWeakTypeTag, In: WeakTypeTag, Out: WeakTypeTag](
      pipeDerivation: c.Expr[PipeDerivation[P]]
  ): c.Expr[P[In, Out]] =
    macros[P, In, Out](pipeDerivation).doDeriveDef.asInstanceOf[c.Expr[P[In, Out]]]

  def deriveConfigured[P[_, _]: ConstructorWeakTypeTag, In: WeakTypeTag, Out: WeakTypeTag](
      config: c.Expr[PipeDerivationConfig[P, In, Out]]
  )(
      pipeDerivation: c.Expr[PipeDerivation[P]]
  ): c.Expr[P[In, Out]] = {
    val m = macros[P, In, Out](pipeDerivation)
    m.doDeriveConf(config.asInstanceOf[m.c.Expr[PipeDerivationConfig[P, In, Out]]]).asInstanceOf[c.Expr[P[In, Out]]]
  }
}
