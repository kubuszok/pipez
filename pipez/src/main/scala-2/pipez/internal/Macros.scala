package pipez.internal

import pipez.{ PipeDerivation, PipeDerivationConfig }
import pipez.internal.Definitions.{ Context, Result }

import scala.annotation.nowarn
import scala.reflect.macros.{ TypecheckException, blackbox }

// The way we are dispatching things here is one giant workaround for https://github.com/scala/bug/issues/5712

@nowarn("msg=The outer reference in this type test cannot be checked at run time.")
final class MacrosImpl[Pipe[_, _], In, Out](val c: blackbox.Context)(
  pipeTpe:                                         blackbox.Context#Type,
  inTpe:                                           blackbox.Context#Type,
  outTpe:                                          blackbox.Context#Type,
  pd:                                              blackbox.Context#Expr[PipeDerivation[Pipe]]
) extends PlatformDefinitions[Pipe, In, Out]
    with PlatformGenerators[Pipe, In, Out] {

  override val In:  Type[In]  = inTpe.asInstanceOf[Type[In]]
  override val Out: Type[Out] = outTpe.asInstanceOf[Type[Out]]

  def PipeOf[I: Type, O: Type]: Type[Pipe[I, O]] =
    c.universe
      .appliedType(pipeTpe.typeSymbol.asInstanceOf[c.Symbol], typeOf[I], typeOf[O])
      .asInstanceOf[Type[Pipe[I, O]]]

  def derivePipe[Input: Type, Output: Type](settings: Settings): DerivationResult[Expr[Pipe[Input, Output]]] = {
    val m = new MacrosImpl[Pipe, Input, Output](c)(
      pipeTpe = pipeTpe,
      inTpe = typeOf[Input],
      outTpe = typeOf[Output],
      pd = pd
    )
    val filteredSettings = settings.stripSpecificsToCurrentDerivation.asInstanceOf[m.Settings]
    val result           = m.derive(filteredSettings)
    val mError           = m.DerivationError
    import DerivationError._
    // pattern matching is done through companion objects - which differ when objects exists in different MacroImpls
    def fixErrors(errors: List[m.DerivationError]): List[DerivationError] = errors.map {
      case mError.MissingPublicConstructor            => MissingPublicConstructor
      case mError.MissingPublicSource(outFieldName)   => MissingPublicSource(outFieldName)
      case mError.MissingMatchingSubType(subtypeName) => MissingMatchingSubType(subtypeName)
      case mError.MissingMatchingValue(valueName)     => MissingMatchingValue(valueName)
      case mError.RequiredImplicitNotFound(inFieldType, outFieldType) =>
        RequiredImplicitNotFound(inFieldType.asInstanceOf[Type[Any]], outFieldType.asInstanceOf[Type[Any]])
      case mError.RecursiveDerivationFailed(inType, outType, errors) =>
        RecursiveDerivationFailed(inType.asInstanceOf[Type[Any]], outType.asInstanceOf[Type[Any]], fixErrors(errors))
      case mError.NotSupportedFieldConversion(inField, inFieldType, outField, outFieldType) =>
        NotSupportedFieldConversion(inField,
                                    inFieldType.asInstanceOf[Type[Any]],
                                    outField,
                                    outFieldType.asInstanceOf[Type[Any]]
        )
      case mError.NotSupportedEnumConversion(isInSumType, isOutSumType) =>
        NotSupportedEnumConversion(isInSumType, isOutSumType)
      case mError.InvalidConfiguration(msg) => InvalidConfiguration(msg)
      case mError.InvalidInput(msg)         => InvalidInput(msg)
      case mError.NotYetSupported           => NotYetSupported
      case mError.NotYetImplemented(msg)    => NotYetImplemented(msg)
    }
    result.fold(expr =>
      DerivationResult
        .pure(expr.asInstanceOf[Expr[Pipe[Input, Output]]])
        .log("Nested derivation's diagnostics:\n" + result.diagnostic.map("   - " + _).mkString("\n"))
    )(errors =>
      DerivationResult.fail(DerivationError.RecursiveDerivationFailed(typeOf[Input], typeOf[Output], fixErrors(errors)))
    )
  }

  import c.universe.*

  val pipeDerivation: Expr[PipeDerivation.Aux[Pipe, Context, Result]] = {
    val expr = pd.asInstanceOf[Expr[PipeDerivation[Pipe]]]
    val Pipe = pipeTpe.asInstanceOf[c.Type]
    c.Expr(
      q"""$expr.asInstanceOf[_root_.pipez.PipeDerivation.Aux[$Pipe, $Context, ${Result[Any].typeConstructor}]]"""
    )
  }

  val previewPipeDerivation: String = previewCode(pd.asInstanceOf[Expr[PipeDerivation[Pipe]]])
}

final class Macro(val c: blackbox.Context) {

  import c.universe.*

  type ConstructorWeakTypeTag[F[_, _]] = WeakTypeTag[F[Any, Nothing]]

  private def macros[Pipe[_, _]: ConstructorWeakTypeTag, In: WeakTypeTag, Out: WeakTypeTag](
    pipeDerivation: c.Expr[PipeDerivation[Pipe]]
  ) = new MacrosImpl[Pipe, In, Out](
    c
  )(
    pipeTpe = c.weakTypeOf[Pipe[Any, Nothing]].typeConstructor,
    inTpe = c.weakTypeOf[In],
    outTpe = c.weakTypeOf[Out],
    pd = pipeDerivation
  )

  // I messed up something with the types of generated Trees - shape is almost identical to the last working version
  // before I started working on Scala 3 and making adjustments, but without this, it produces "type mismatch error" in
  // one test case:
  //
  //    def test[In, Out](out: Out)(implicit
  //      contextCodec:        ContextCodec[In, Out]
  //    ): ContextCodec[CaseParamIn[In], CaseParamOutExt[Out]] = ContextCodec.derive(
  //      ContextCodec.Config[CaseParamIn[In], CaseParamOutExt[Out]].addField(_.x, (_, _, _) => Right(out))
  //    )
  //
  // If we figured it out, we could remove this fix, as it only fails this case as far as I can tell.
  private def fixTypes[Pipe[_, _]: ConstructorWeakTypeTag, Out](
    expr:           blackbox.Context#Expr[Out],
    pipeDerivation: c.Expr[PipeDerivation[Pipe]]
  ): c.Expr[Out] = try
    c.Expr[Out](c.typecheck(tree = c.untypecheck(expr.tree.asInstanceOf[c.Tree])))
  catch {
    case TypecheckException(_, msg) => c.abort(c.enclosingPosition, msg)
  }

  /** Called with `macro pipez.internal.Macro.deriveDefault[Pipe, In, Out]` */
  def deriveDefault[Pipe[_, _]: ConstructorWeakTypeTag, In: WeakTypeTag, Out: WeakTypeTag](
    pipeDerivation: c.Expr[PipeDerivation[Pipe]]
  ): c.Expr[Pipe[In, Out]] = {
    val m = macros[Pipe, In, Out](pipeDerivation)
    fixTypes(m.deriveDefault, pipeDerivation)
  }

  /** Called with `macro pipez.internal.Macro.deriveConfigured[Pipe, In, Out]` */
  def deriveConfigured[Pipe[_, _]: ConstructorWeakTypeTag, In: WeakTypeTag, Out: WeakTypeTag](
    config: c.Expr[PipeDerivationConfig[Pipe, In, Out]]
  )(
    pipeDerivation: c.Expr[PipeDerivation[Pipe]]
  ): c.Expr[Pipe[In, Out]] = {
    val m = macros[Pipe, In, Out](pipeDerivation)
    fixTypes(m.deriveConfigured(config.asInstanceOf[m.c.Expr[PipeDerivationConfig[Pipe, In, Out]]]), pipeDerivation)
  }
}
