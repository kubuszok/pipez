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

  private lazy val ctxTpe: c.Type = {
    val pdTpe = pd.actualType.asInstanceOf[c.Type]
    pdTpe.member(TypeName("Context")).asType.toType.asSeenFrom(pdTpe, pdTpe.typeSymbol)
  }

  private lazy val resTpe: c.Type = {
    val pdTpe = pd.actualType.asInstanceOf[c.Type]
    pdTpe.member(TypeName("Result")).asType.toType.asSeenFrom(pdTpe, pdTpe.typeSymbol)
  }

  private lazy val pdTyped: c.Expr[Any] = {
    val expr = pd.asInstanceOf[c.Expr[PipeDerivation[P]]]
    val PipeTc = pipeTpe.asInstanceOf[c.Type]
    val CtxT = ctxTpe
    val ResT = resTpe.typeConstructor
    c.Expr[Any](q"""$expr.asInstanceOf[_root_.pipez.PipeDerivation.Aux[$PipeTc, $CtxT, $ResT]]""")(anyTag)
  }

  override lazy val pdExpr: Expr[Any] = pdTyped.asInstanceOf[Expr[Any]]

  // ---- Code generation implementations ----

  private def tpeOf[A: Type]: c.Type = implicitly[Type[A]].asInstanceOf[c.WeakTypeTag[A]].tpe

  override def generateLift[In: Type, Out: Type](body: (Expr[Any], Expr[Any]) => Expr[Any]): Expr[Pipe[In, Out]] = {
    val inT = tpeOf[In]
    val outT = tpeOf[Out]
    val pdE = pdTyped
    val inName = TermName(c.freshName("in"))
    val ctxName = TermName(c.freshName("ctx"))
    val inRef = c.Expr[Any](Ident(inName))(anyTag)
    val ctxRef = c.Expr[Any](Ident(ctxName))(anyTag)
    val bodyExpr = body(inRef.asInstanceOf[Expr[Any]], ctxRef.asInstanceOf[Expr[Any]])
    val bodyTree = bodyExpr.asInstanceOf[c.Expr[Any]].tree
    val liftTree = q"""$pdE.lift[$inT, $outT]((($inName: $inT, $ctxName: $ctxTpe) => $bodyTree))"""
    mkExpr(liftTree).asInstanceOf[Expr[Pipe[In, Out]]]
  }

  override def generateUnlift(pipe: Expr[Any], in: Expr[Any], ctx: Expr[Any]): Expr[Any] = {
    val pdE = pdTyped
    val p = pipe.asInstanceOf[c.Expr[Any]]
    val i = in.asInstanceOf[c.Expr[Any]]
    val ct = ctx.asInstanceOf[c.Expr[Any]]
    mkExpr(q"""$pdE.unlift($p, $i, $ct)""").asInstanceOf[Expr[Any]]
  }

  override def generatePureResult(a: Expr[Any]): Expr[Any] = {
    val pdE = pdTyped
    val ae = a.asInstanceOf[c.Expr[Any]]
    mkExpr(q"""$pdE.pureResult($ae)""").asInstanceOf[Expr[Any]]
  }

  override def generateMergeResults(ctx: Expr[Any], ra: Expr[Any], rb: Expr[Any], f: Expr[Any]): Expr[Any] = {
    val pdE = pdTyped
    val ce = ctx.asInstanceOf[c.Expr[Any]]
    val rae = ra.asInstanceOf[c.Expr[Any]]
    val rbe = rb.asInstanceOf[c.Expr[Any]]
    val fe = f.asInstanceOf[c.Expr[Any]]
    mkExpr(q"""$pdE.mergeResults($ce, $rae, $rbe, $fe)""").asInstanceOf[Expr[Any]]
  }

  override def generateUpdateContext(ctx: Expr[Any], path: Expr[Path]): Expr[Any] = {
    val pdE = pdTyped
    val ce = ctx.asInstanceOf[c.Expr[Any]]
    val pe = path.asInstanceOf[c.Expr[Path]]
    mkExpr(q"""$pdE.updateContext($ce, $pe)""").asInstanceOf[Expr[Any]]
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
    mkExpr(Function(List(arrParam, valParam), bodyTree))
      .asInstanceOf[Expr[Any]]
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
    mkExpr(Function(List(arrParam, valParam), bodyTree))
      .asInstanceOf[Expr[Any]]
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
  ) = new PipezMacrosImpl2[P, In, Out](c)(
    pipeTpe = c.weakTypeOf[P[Any, Nothing]].typeConstructor,
    inTpe = c.weakTypeOf[In],
    outTpe = c.weakTypeOf[Out],
    pd = pipeDerivation
  )

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
