package pipez.dsl

trait ConverterCompanionCompat extends pipez.PipeSemiautoConfiguredSupport[Converter] {
  implicit inline def deriveAutomatic[In, Out](using
      pipeDerivation: pipez.PipeDerivation[Converter]
  ): Converter[In, Out] = derive[In, Out]
}

trait ParserCompanionCompat extends pipez.PipeSemiautoConfiguredSupport[Parser] {
  implicit inline def deriveAutomatic[In, Out](using
      pipeDerivation: pipez.PipeDerivation[Parser]
  ): Parser[In, Out] = derive[In, Out]
}

trait PatchApplierCompanionCompat extends pipez.PipeSemiautoConfiguredSupport[PatchApplier]
