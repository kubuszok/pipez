package pipez.dsl

/** After obtaining Patch apply it to value passes with addFallbackValue and return Patched value */
trait PatchApplier[Patch, Patched] {

  def apply(patch: Patch): Patched
}
object PatchApplier
    extends pipez.PipeSemiautoSupport[PatchApplier]
    with pipez.PipeSemiautoConfiguredSupport[PatchApplier]
    with PatchApplierInstances0 {

  implicit val pipeDerivation: pipez.PipeDerivation[PatchApplier] = PatchApplierDerivationDefinition
}
private[dsl] trait PatchApplierInstances0 { self: PatchApplier.type =>

  implicit def convertToSelf[A, B >: A]: PatchApplier[A, B] = a => a
}
private[dsl] object PatchApplierDerivationDefinition extends pipez.PipeDerivation.Simple[PatchApplier] {

  override def simpleLift[In, Out](f: In => Out):                          PatchApplier[In, Out] = f(_)
  override def simpleUnlift[In, Out](pipe: PatchApplier[In, Out], in: In): Out                   = pipe(in)
}
