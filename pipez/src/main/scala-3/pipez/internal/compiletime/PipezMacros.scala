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
      PipezMacrosImpl,
      PipezConfigParserScala3 {

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

  // ---- Code generation implementations ----

  override def generateLift[In: Type, Out: Type](
      body: (Expr[In], Expr[Any]) => Expr[Any]
  ): Expr[Pipe[In, Out]] =
    generateLiftImpl[In, Out](
      summon[Type[In]].asInstanceOf[SQType[In]],
      summon[Type[Out]].asInstanceOf[SQType[Out]],
      body
    )

  private def generateLiftImpl[In, Out](
      inSQType: SQType[In],
      outSQType: SQType[Out],
      body: (Expr[In], Expr[Any]) => Expr[Any]
  ): Expr[Pipe[In, Out]] = {
    given SQType[P] = pipeTypeQ
    given SQType[In] = inSQType
    given SQType[Out] = outSQType
    val pd = pdTyped

    '{
      $pd.lift[In, Out] { (in: In, ctx: Ctx0) =>
        val _in = in
        val _ctx = ctx
        ${
          val bodyResult = body(
            '{ _in }.asInstanceOf[Expr[In]],
            '{ _ctx }.asInstanceOf[Expr[Any]]
          )
          val bodyTerm = bodyResult.asInstanceOf[SQExpr[Any]].asTerm
          val expectedType = resCtorUntypedType.appliedTo(TypeRepr.of[Out])
          val casted = TypeApply(Select.unique(bodyTerm, "asInstanceOf"), List(Inferred(expectedType)))
          casted.asExpr.asInstanceOf[Expr[Res0[Out]]]
        }
      }
    }.asInstanceOf[Expr[Pipe[In, Out]]]
  }

  private def pdTerm: Term = pdExpr.asInstanceOf[SQExpr[Any]].asTerm
  private def toTerm(e: Expr[Any]): Term = e.asInstanceOf[SQExpr[Any]].asTerm

  override def generateUnlift(
      pipe: Expr[Any],
      in: Expr[Any],
      ctx: Expr[Any]
  ): Expr[Any] = {
    val anyTR = TypeRepr.of[Any]
    val pipeTR = PipeCtor.apply[Any, Any](using typeOfAny, typeOfAny).asInstanceOf[SQType[?]] match {
      case '[t] => TypeRepr.of[t]
    }
    val unliftSel = pdTerm.select(pdTerm.tpe.widen.typeSymbol.methodMember("unlift").head)
    val pipeCast = TypeApply(Select.unique(toTerm(pipe), "asInstanceOf"), List(Inferred(pipeTR)))
    val ctxCast = TypeApply(Select.unique(toTerm(ctx), "asInstanceOf"), List(TypeTree.of(using ctxSQType)))
    val applied = unliftSel
      .appliedToTypes(List(anyTR, anyTR))
      .appliedToArgs(List(pipeCast, toTerm(in), ctxCast))
    applied.asExpr.asInstanceOf[Expr[Any]]
  }

  override def generatePureResult(a: Expr[Any]): Expr[Any] = {
    val anyTR = TypeRepr.of[Any]
    val pureResultSel = pdTerm.select(pdTerm.tpe.widen.typeSymbol.methodMember("pureResult").head)
    val applied = pureResultSel.appliedToType(anyTR).appliedTo(toTerm(a))
    applied.asExpr.asInstanceOf[Expr[Any]]
  }

  override def generateMergeResults(
      ctx: Expr[Any],
      ra: Expr[Any],
      rb: Expr[Any],
      f: Expr[Any]
  ): Expr[Any] = {
    val anyTR = TypeRepr.of[Any]
    val resTR = resCtorUntypedType.appliedTo(anyTR)
    val fn2TR = TypeRepr.of[(Any, Any) => Any]
    val mergeResultsSel = pdTerm.select(pdTerm.tpe.widen.typeSymbol.methodMember("mergeResults").head)
    val ctxCast = TypeApply(Select.unique(toTerm(ctx), "asInstanceOf"), List(TypeTree.of(using ctxSQType)))
    val raCast = TypeApply(Select.unique(toTerm(ra), "asInstanceOf"), List(Inferred(resTR)))
    val rbCast = TypeApply(Select.unique(toTerm(rb), "asInstanceOf"), List(Inferred(resTR)))
    val fCast = TypeApply(Select.unique(toTerm(f), "asInstanceOf"), List(Inferred(fn2TR)))
    val applied = mergeResultsSel
      .appliedToTypes(List(anyTR, anyTR, anyTR))
      .appliedToArgs(List(ctxCast, raCast, rbCast, fCast))
    applied.asExpr.asInstanceOf[Expr[Any]]
  }

  override def generateUpdateContext(ctx: Expr[Any], path: Expr[Path]): Expr[Any] = {
    val ctxCast = TypeApply(Select.unique(toTerm(ctx), "asInstanceOf"), List(TypeTree.of(using ctxSQType)))
    val updateContextSel = pdTerm.select(pdTerm.tpe.widen.typeSymbol.methodMember("updateContext").head)
    val applied = updateContextSel.appliedToArgs(List(ctxCast, toTerm(path)))
    applied.asExpr.asInstanceOf[Expr[Any]]
  }

  override def generateArrayToConstructorFn[Out: Type](
      outClass: CaseClass[Out],
      lastIndex: Int,
      totalFields: Int
  ): Expr[Any] = {
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

    '{ (arr: Array[Any], _dummy: Any) =>
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
        newExpr.asExpr.asInstanceOf[SQExpr[Any]]
      }
    }.asInstanceOf[Expr[Any]]
  }

  override def generateArrayToBeanFn[Out: Type](
      beanFields: List[(??, Method)],
      defaultConstructor: Method,
      lastIndex: Int,
      totalFields: Int
  ): Expr[Any] = {
    val outTR = UntypedType.fromTyped[Out].asInstanceOf[TypeRepr]

    val beanConstructorTerm = defaultConstructor.fold(
      onInstance = _ => throw new RuntimeException("Default constructor should not need instance"),
      onTypes = _ => Map.empty,
      onValues = _ => Map.empty
    ) match {
      case Right(result) => toTerm(result.value.asInstanceOf[Expr[Any]])
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

    '{ (arr: Array[Any], _dummy: Any) =>
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

        Block(beanValDef :: setterStmts, beanRef).asExpr.asInstanceOf[SQExpr[Any]]
      }
    }.asInstanceOf[Expr[Any]]
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
