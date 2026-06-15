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

  override lazy val pdExpr: Expr[Any] = {
    given SQType[P] = pipeTypeQ
    '{ $pdQ.asInstanceOf[PipeDerivation.Aux[P, Ctx0, Res0]] }.asInstanceOf[Expr[Any]]
  }

  private def pdTyped: Expr[PipeDerivation.Aux[P, Ctx0, Res0]] =
    pdExpr.asInstanceOf[Expr[PipeDerivation.Aux[P, Ctx0, Res0]]]

  // ---- Context / Result threaded as Hearth types ----
  // `Ctx`/`Res` stay abstract (inherited); the evidence carries the real types, and the generate* bodies cast between
  // the abstract `Res[A]`/`Ctx` and the concrete `Res0[A]`/`Ctx0` internally.

  implicit override def ctxType: Type[Ctx] = ctxSQType.asInstanceOf[Type[Ctx]]

  override def resType[A: Type]: Type[Res[A]] = {
    val xRepr = TypeRepr.of(using summon[Type[A]].asInstanceOf[SQType[A]])
    resCtorUntypedType.appliedTo(xRepr).asType.asInstanceOf[Type[Res[A]]]
  }

  // ---- Code generation implementations (typed `'{ }` quotes against `PipeDerivation.Aux[P, Ctx0, Res0]`) ----

  private def sqExpr[A](e: Expr[A]): SQExpr[A] = e.asInstanceOf[SQExpr[A]]
  // The abstract `Ctx`/`Res[A]` and the concrete `Ctx0`/`Res0[A]` are the same runtime type; relabel when splicing
  // into the typed `pd.*` calls (which are declared in terms of `Ctx0`/`Res0`).
  private def ctx0(e: Expr[Ctx]): SQExpr[Ctx0] = e.asInstanceOf[SQExpr[Ctx0]]
  private def res0[A](e: Expr[Res[A]]): SQExpr[Res0[A]] = e.asInstanceOf[SQExpr[Res0[A]]]
  private def pdSQ: SQExpr[PipeDerivation.Aux[P, Ctx0, Res0]] =
    pdTyped.asInstanceOf[SQExpr[PipeDerivation.Aux[P, Ctx0, Res0]]]

  // NOTE: on Scala 3, Hearth's `Type[X]` IS `scala.quoted.Type[X]`, so a `given SQType[X] = summon[Type[X]]` would
  // resolve to itself and stack-overflow. Always build the `SQType` givens from the NAMED `Type` evidence, never via
  // `summon`.

  override def generateLift[In, Out](
      body: (Expr[In], Expr[Ctx]) => Expr[Res[Out]]
  )(using inT: Type[In], outT: Type[Out]): Expr[Pipe[In, Out]] = {
    given SQType[P] = pipeTypeQ
    given SQType[In] = inT.asInstanceOf[SQType[In]]
    given SQType[Out] = outT.asInstanceOf[SQType[Out]]
    val pd = pdSQ
    '{
      $pd.lift[In, Out] { (in: In, ctx: Ctx0) =>
        ${
          sqExpr(body('in.asInstanceOf[Expr[In]], 'ctx.asInstanceOf[Expr[Ctx]]))
            .asInstanceOf[SQExpr[Res0[Out]]]
        }
      }
    }.asInstanceOf[Expr[Pipe[In, Out]]]
  }

  override def generateUnlift[In, Out](
      pipe: Expr[Pipe[In, Out]],
      in: Expr[In],
      ctx: Expr[Ctx]
  )(using inT: Type[In], outT: Type[Out]): Expr[Res[Out]] = {
    given SQType[P] = pipeTypeQ
    given SQType[In] = inT.asInstanceOf[SQType[In]]
    given SQType[Out] = outT.asInstanceOf[SQType[Out]]
    val pd = pdSQ
    '{ $pd.unlift[In, Out](${ sqExpr(pipe) }, ${ sqExpr(in) }, ${ ctx0(ctx) }) }.asInstanceOf[Expr[Res[Out]]]
  }

  override def generatePureResult[A](a: Expr[A])(using aT: Type[A]): Expr[Res[A]] = {
    given SQType[P] = pipeTypeQ
    given SQType[A] = aT.asInstanceOf[SQType[A]]
    val pd = pdSQ
    '{ $pd.pureResult[A](${ sqExpr(a) }) }.asInstanceOf[Expr[Res[A]]]
  }

  override def generateMergeResults[A, B, C](
      ctx: Expr[Ctx],
      ra: Expr[Res[A]],
      rb: Expr[Res[B]],
      f: Expr[(A, B) => C]
  )(using aT: Type[A], bT: Type[B], cT: Type[C]): Expr[Res[C]] = {
    given SQType[P] = pipeTypeQ
    given SQType[A] = aT.asInstanceOf[SQType[A]]
    given SQType[B] = bT.asInstanceOf[SQType[B]]
    given SQType[C] = cT.asInstanceOf[SQType[C]]
    val pd = pdSQ
    '{
      $pd.mergeResults[A, B, C](${ ctx0(ctx) }, ${ res0(ra) }, ${ res0(rb) }, ${ sqExpr(f) })
    }.asInstanceOf[Expr[Res[C]]]
  }

  override def generateUpdateContext(ctx: Expr[Ctx], path: Expr[Path]): Expr[Ctx] = {
    given SQType[P] = pipeTypeQ
    val pd = pdSQ
    '{ $pd.updateContext(${ ctx0(ctx) }, ${ sqExpr(path) }) }.asInstanceOf[Expr[Ctx]]
  }

  override def generateArrayToConstructorFn[Out](
      outClass: CaseClass[Out],
      lastIndex: Int,
      totalFields: Int
  )(using outT: Type[Out]): Expr[(Array[Any], Unit) => Out] = {
    given SQType[Out] = outT.asInstanceOf[SQType[Out]]
    val outTR = UntypedType.fromTyped[Out].asInstanceOf[TypeRepr]
    val outParams = outClass.primaryConstructor.totalParameters.flatten
    val ctorSym = outTR.typeSymbol.primaryConstructor
    val typeParams = outTR match {
      case AppliedType(_, args) => args
      case _                    => Nil
    }
    val paramTypeReprs: List[TypeRepr] = outParams.map { case (_, param) =>
      val existential = param.tpe
      import existential.Underlying as ParamT
      UntypedType.fromTyped[ParamT].asInstanceOf[TypeRepr]
    }

    '{ (arr: Array[Any], _dummy: Unit) =>
      ${
        val arrTerm = 'arr.asTerm
        val ctorArgs = paramTypeReprs.zipWithIndex.map { case (paramTR, idx) =>
          val arrGet = Apply(Select.unique(arrTerm, "apply"), List(Literal(IntConstant(idx))))
          TypeApply(Select.unique(arrGet, "asInstanceOf"), List(Inferred(paramTR)))
        }
        val outTypeTree = Inferred(outTR)
        val newExpr =
          if typeParams.nonEmpty then New(outTypeTree)
            .select(ctorSym)
            .appliedToTypes(typeParams)
            .appliedToArgs(ctorArgs)
          else New(outTypeTree).select(ctorSym).appliedToArgs(ctorArgs)
        newExpr.asExpr.asInstanceOf[SQExpr[Out]]
      }
    }.asInstanceOf[Expr[(Array[Any], Unit) => Out]]
  }

  override def generateArrayToBeanFn[Out](
      beanFields: List[(??, Method)],
      defaultConstructor: Method,
      lastIndex: Int,
      totalFields: Int
  )(using outT: Type[Out]): Expr[(Array[Any], Unit) => Out] = {
    given SQType[Out] = outT.asInstanceOf[SQType[Out]]
    val outTR = UntypedType.fromTyped[Out].asInstanceOf[TypeRepr]

    val beanConstructorTerm = defaultConstructor.fold(
      onInstance = _ => throw new RuntimeException("Default constructor should not need instance"),
      onTypes = _ => Map.empty,
      onValues = _ => Map.empty
    ) match {
      case Right(result) => result.value.asInstanceOf[Expr[Any]].asInstanceOf[SQExpr[Any]].asTerm
      case Left(err)     => throw new RuntimeException(s"Cannot call default constructor: $err")
    }

    val setterInfo: List[(TypeRepr, Symbol)] = beanFields.map { case (fieldType, setter) =>
      val existential = fieldType
      import existential.Underlying as FT
      val paramTR = UntypedType.fromTyped[FT].asInstanceOf[TypeRepr]
      val setterSym = outTR.typeSymbol
        .methodMember(setter.name)
        .headOption
        .getOrElse(throw new RuntimeException(s"Setter ${setter.name} not found on ${outTR.show}"))
      (paramTR, setterSym)
    }

    '{ (arr: Array[Any], _dummy: Unit) =>
      ${
        val arrTerm = 'arr.asTerm
        val beanSym = Symbol.newVal(Symbol.spliceOwner, "bean", outTR, Flags.EmptyFlags, Symbol.noSymbol)
        val beanValDef = ValDef(beanSym, Some(beanConstructorTerm))
        val beanRef = Ref(beanSym)
        val setterStmts = setterInfo.zipWithIndex.map { case ((paramTR, setterSym), idx) =>
          val arrGet = Apply(Select.unique(arrTerm, "apply"), List(Literal(IntConstant(idx))))
          val castedValue = TypeApply(Select.unique(arrGet, "asInstanceOf"), List(Inferred(paramTR)))
          Apply(beanRef.select(setterSym), List(castedValue))
        }
        Block(beanValDef :: setterStmts, beanRef).asExpr.asInstanceOf[SQExpr[Out]]
      }
    }.asInstanceOf[Expr[(Array[Any], Unit) => Out]]
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
