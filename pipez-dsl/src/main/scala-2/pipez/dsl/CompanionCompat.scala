package pipez.dsl

trait ConverterCompanionCompat
    extends pipez.PipeAutoSupport[Converter]
    with pipez.PipeSemiautoConfiguredSupport[Converter]

trait ParserCompanionCompat
    extends pipez.PipeAutoSupport[Parser]
    with pipez.PipeSemiautoConfiguredSupport[Parser]

trait PatchApplierCompanionCompat
    extends pipez.PipeSemiautoSupport[PatchApplier]
    with pipez.PipeSemiautoConfiguredSupport[PatchApplier]
