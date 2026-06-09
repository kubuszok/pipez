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

  // Use the implicit's actual type for the stable val annotation.
  // This ensures path-dependent types (Context, Result) are resolved concretely.
  private lazy val pdDeclaredType: c.Type = {
    val pdTree = pd.asInstanceOf[c.Expr[Any]].tree
    val declaredT = pdTree.tpe.widen
    val implTree = c.inferImplicitValue(declaredT, silent = true)
    if (implTree != EmptyTree) implTree.tpe.widen else declaredT
  }

  private lazy val pdInit: Tree = {
    val expr = pd.asInstanceOf[c.Expr[Any]]
    q"""val $pdStableName: $pdDeclaredType = $expr"""
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
    val isIdentity = resOutTpe =:= outT
    val finalBody = if (isIdentity) bodyTree else q"""($bodyTree).asInstanceOf[$resOutTpe]"""
    mkExpr(q"""{
      $pdInit
      $pdStableRef.lift[$inT, $outT](($inName: $inT, $ctxName: $pdStableRef.Context) => $finalBody)
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
        if (rt.isInstanceOf[scala.reflect.internal.Types#TypeBounds] || rt.typeSymbol == tp) {
          // Identity: type Result[Out] = Out
          (outTpe: c.Type) => outTpe
        } else { (outTpe: c.Type) =>
          rt.substituteTypes(List(tp), List(outTpe))
        }
      case _ =>
        (_: c.Type) => definitions.AnyTpe
    }
    (ctxTpe, resApply)
  }

  private def macros[P[_, _]: ConstructorWeakTypeTag, In: WeakTypeTag, Out: WeakTypeTag](
      pipeDerivation: c.Expr[PipeDerivation[P]]
  ) = {
    // Diagnostic: dump all type info for every macro invocation
    // (placed BEFORE extractConcreteTypes to catch crashes)
    {
      val pdTree = pipeDerivation.tree
      val f = new java.io.FileWriter("/tmp/pipez-types.log", true)
      f.write(s"\n=== MACRO INVOCATION ===\n")
      f.write(s"In=${c.weakTypeOf[In]}, Out=${c.weakTypeOf[Out]}\n")
      f.write(s"Pipe=${c.weakTypeOf[P[Any, Nothing]].typeConstructor}\n")
      f.write(s"pd.tree = $pdTree\n")
      f.write(s"pd.tree.tpe = ${pdTree.tpe}\n")
      f.write(s"pd.tree.tpe.widen = ${pdTree.tpe.widen}\n")
      f.write(s"pd.actualType = ${pipeDerivation.actualType}\n")
      if (pdTree.symbol != null) {
        f.write(s"pd.tree.symbol = ${pdTree.symbol}\n")
        f.write(s"pd.tree.symbol.typeSignature = ${pdTree.symbol.typeSignature}\n")
        f.write(s"pd.tree.symbol.info = ${pdTree.symbol.info}\n")
      }
      val declaredT = pdTree.tpe.widen
      val implTree = c.inferImplicitValue(declaredT, silent = true)
      f.write(s"inferImplicitValue isEmpty = ${implTree == EmptyTree}\n")
      if (implTree != EmptyTree) {
        f.write(s"inferImplicitValue.tpe = ${implTree.tpe}\n")
        f.write(s"inferImplicitValue.tpe.widen = ${implTree.tpe.widen}\n")
        val implTpe = implTree.tpe.widen
        val rm = implTpe.member(TypeName("Result"))
        f.write(s"Result member = $rm\n")
        f.write(s"Result.typeSignatureIn = ${rm.typeSignatureIn(implTpe)}\n")
        val rs = rm.typeSignatureIn(implTpe)
        f.write(s"Result sig.dealias = ${rs.dealias}\n")
        if (rs.typeParams.nonEmpty) {
          f.write(s"Result sig.typeParams = ${rs.typeParams}\n")
          f.write(s"Result sig.resultType = ${rs.resultType}\n")
          f.write(s"Result sig.resultType.dealias = ${rs.resultType.dealias}\n")
        }
        val cm = implTpe.member(TypeName("Context"))
        f.write(s"Context member = $cm\n")
        f.write(s"Context.typeSignatureIn = ${cm.typeSignatureIn(implTpe)}\n")
      }
      f.close()
    }

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

  private def fixTypes[Out: WeakTypeTag](expr: c.Expr[Out]): c.Expr[Out] =
    try c.Expr[Out](c.typecheck(tree = c.untypecheck(expr.tree)))
    catch {
      case _: scala.reflect.macros.TypecheckException =>
        // If re-typechecking fails (e.g., enum invariance), wrap in asInstanceOf and try again
        val outTpe = c.weakTypeOf[Out]
        try c.Expr[Out](c.typecheck(tree = c.untypecheck(q"""${expr.tree}.asInstanceOf[$outTpe]""")))
        catch { case e2: scala.reflect.macros.TypecheckException => c.abort(c.enclosingPosition, e2.msg) }
    }

  def deriveDefault[P[_, _]: ConstructorWeakTypeTag, In: WeakTypeTag, Out: WeakTypeTag](
      pipeDerivation: c.Expr[PipeDerivation[P]]
  ): c.Expr[P[In, Out]] = {
    val m = macros[P, In, Out](pipeDerivation)
    fixTypes(m.doDeriveDef.asInstanceOf[c.Expr[P[In, Out]]])
  }

  def deriveConfigured[P[_, _]: ConstructorWeakTypeTag, In: WeakTypeTag, Out: WeakTypeTag](
      config: c.Expr[PipeDerivationConfig[P, In, Out]]
  )(
      pipeDerivation: c.Expr[PipeDerivation[P]]
  ): c.Expr[P[In, Out]] = {
    val m = macros[P, In, Out](pipeDerivation)
    fixTypes(
      m.doDeriveConf(config.asInstanceOf[m.c.Expr[PipeDerivationConfig[P, In, Out]]]).asInstanceOf[c.Expr[P[In, Out]]]
    )
  }
}
