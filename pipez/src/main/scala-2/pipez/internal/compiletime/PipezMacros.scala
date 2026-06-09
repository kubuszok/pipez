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
    with PipezMacrosImpl
    with PipezConfigParserScala2 {

  import c.universe.*

  override type Pipe[A, B] = P[A, B]

  override implicit lazy val typeOfAny: Type[Any] = Type.of[Any]

  override implicit lazy val PipeCtor: Type.Ctor2[Pipe] =
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
    val expr   = pd.asInstanceOf[c.Expr[PipeDerivation[P]]]
    val PipeTc = pipeTpe.asInstanceOf[c.Type]
    val CtxT   = ctxTpe
    val ResT   = resTpe.typeConstructor
    c.Expr(q"""$expr.asInstanceOf[_root_.pipez.PipeDerivation.Aux[$PipeTc, $CtxT, $ResT]]""")
  }

  override lazy val pdExpr: Expr[Any] = pdTyped.asInstanceOf[Expr[Any]]

  // ---- Code generation implementations ----

  override def generateLift[In: Type, Out: Type](body: (Expr[Any], Expr[Any]) => Expr[Any]): Expr[Pipe[In, Out]] = {
    val inT    = typeOf[In]
    val outT   = typeOf[Out]
    val pdE    = pdTyped
    val inName  = TermName(c.freshName("in"))
    val ctxName = TermName(c.freshName("ctx"))
    val inRef   = c.Expr[Any](Ident(inName))
    val ctxRef  = c.Expr[Any](Ident(ctxName))
    val bodyExpr = body(inRef.asInstanceOf[Expr[Any]], ctxRef.asInstanceOf[Expr[Any]])
    val bodyTree = bodyExpr.asInstanceOf[c.Expr[Any]].tree
    val liftTree = q"""$pdE.lift[$inT, $outT]((($inName: $inT, $ctxName: $ctxTpe) => $bodyTree))"""
    c.Expr(liftTree).asInstanceOf[Expr[Pipe[In, Out]]]
  }

  override def generateUnlift(pipe: Expr[Any], in: Expr[Any], ctx: Expr[Any]): Expr[Any] = {
    val pdE = pdTyped
    val p   = pipe.asInstanceOf[c.Expr[Any]]
    val i   = in.asInstanceOf[c.Expr[Any]]
    val ct  = ctx.asInstanceOf[c.Expr[Any]]
    c.Expr(q"""$pdE.unlift($p, $i, $ct)""").asInstanceOf[Expr[Any]]
  }

  override def generatePureResult(a: Expr[Any]): Expr[Any] = {
    val pdE = pdTyped
    val ae  = a.asInstanceOf[c.Expr[Any]]
    c.Expr(q"""$pdE.pureResult($ae)""").asInstanceOf[Expr[Any]]
  }

  override def generateMergeResults(ctx: Expr[Any], ra: Expr[Any], rb: Expr[Any], f: Expr[Any]): Expr[Any] = {
    val pdE = pdTyped
    val ce  = ctx.asInstanceOf[c.Expr[Any]]
    val rae = ra.asInstanceOf[c.Expr[Any]]
    val rbe = rb.asInstanceOf[c.Expr[Any]]
    val fe  = f.asInstanceOf[c.Expr[Any]]
    c.Expr(q"""$pdE.mergeResults($ce, $rae, $rbe, $fe)""").asInstanceOf[Expr[Any]]
  }

  override def generateUpdateContext(ctx: Expr[Any], path: Expr[Path]): Expr[Any] = {
    val pdE = pdTyped
    val ce  = ctx.asInstanceOf[c.Expr[Any]]
    val pe  = path.asInstanceOf[c.Expr[Path]]
    c.Expr(q"""$pdE.updateContext($ce, $pe)""").asInstanceOf[Expr[Any]]
  }

  override def generateBlock(statements: List[Expr[Any]], result: Expr[Any]): Expr[Any] = {
    val stats = statements.map(_.asInstanceOf[c.Expr[Any]].tree)
    val res   = result.asInstanceOf[c.Expr[Any]].tree
    c.Expr(q"..$stats; $res").asInstanceOf[Expr[Any]]
  }

  override def generateArrayToConstructorFn[Out: Type](
      outClass: CaseClass[Out],
      lastIndex: Int,
      totalFields: Int
  ): Expr[Any] = {
    val outT = typeOf[Out]
    val arrName  = TermName(c.freshName("arr"))
    val valName  = TermName(c.freshName("value"))

    // arr(lastIndex) = value
    val storeStmt = q"""$arrName($lastIndex) = $valName"""

    // Build constructor args: arr(i).asInstanceOf[ParamType]
    val outParams = outClass.primaryConstructor.totalParameters.flatten
    val ctorArgs = outParams.zipWithIndex.map { case ((_, param), idx) =>
      val paramT = param.tpe.asInstanceOf[c.Type]
      q"""$arrName($idx).asInstanceOf[$paramT]"""
    }

    val newExpr = q"""new $outT(..$ctorArgs)"""
    c.Expr(q"""((($arrName: _root_.scala.Array[Any], $valName: Any) => { $storeStmt; $newExpr }): (Any, Any) => Any)""").asInstanceOf[Expr[Any]]
  }

  override def generateArrayToBeanFn[Out: Type](
      beanFields: List[(??, Method)],
      defaultConstructor: Method,
      lastIndex: Int,
      totalFields: Int
  ): Expr[Any] = {
    val outT = typeOf[Out]
    val arrName  = TermName(c.freshName("arr"))
    val valName  = TermName(c.freshName("value"))
    val beanName = TermName(c.freshName("bean"))

    // arr(lastIndex) = value
    val storeStmt = q"""$arrName($lastIndex) = $valName"""

    // Construct the bean
    val beanValDef = q"""val $beanName = new $outT()"""

    // Call each setter
    val setterStmts = beanFields.zipWithIndex.map { case ((fieldType, setter), idx) =>
      val paramT = fieldType.asInstanceOf[c.Type]
      val setterName = TermName(setter.name)
      q"""$beanName.$setterName($arrName($idx).asInstanceOf[$paramT])"""
    }

    val body = q"""
      $storeStmt
      $beanValDef
      ..$setterStmts
      $beanName
    """
    c.Expr(q"""((($arrName: _root_.scala.Array[Any], $valName: Any) => { $body }): (Any, Any) => Any)""").asInstanceOf[Expr[Any]]
  }

  // ---- Entry points ----

  def doDeriveDef: Expr[Pipe[In0, Out0]] = {
    implicit val inT: Type[In0]  = inTpe.asInstanceOf[Type[In0]]
    implicit val outT: Type[Out0] = outTpe.asInstanceOf[Type[Out0]]
    deriveDefault[In0, Out0]
  }

  def doDeriveConf(config: c.Expr[PipeDerivationConfig[P, In0, Out0]]): Expr[Pipe[In0, Out0]] = {
    implicit val inT: Type[In0]  = inTpe.asInstanceOf[Type[In0]]
    implicit val outT: Type[Out0] = outTpe.asInstanceOf[Type[Out0]]
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
    m.doDeriveConf(config).asInstanceOf[c.Expr[P[In, Out]]]
  }
}
