package pipez.dsl.internal

import pipez.dsl.{ Converter, Parser, PatchApplier }

import scala.quoted.*

object Macros:

  def deriveAndConvert[From: Type, To: Type](
    from:         Expr[From],
    config:       Expr[Converter.Config[From, To]]
  )(using quotes: Quotes): Expr[To] =
    '{ _root_.pipez.dsl.Converter.derive[From, To](${ config }).convert(${ from }) }

  def deriveAndParseFast[From: Type, To: Type](
    from:         Expr[From],
    config:       Expr[Parser.Config[From, To]]
  )(using quotes: Quotes): Expr[Parser.ParsingResult[To]] =
    '{ _root_.pipez.dsl.Parser.derive[From, To](${ config }).parseFast(${ from }) }

  def deriveAndParseFull[From: Type, To: Type](
    from:         Expr[From],
    config:       Expr[Parser.Config[From, To]]
  )(using quotes: Quotes): Expr[Parser.ParsingResult[To]] =
    '{ _root_.pipez.dsl.Parser.derive[From, To](${ config }).parseFull(${ from }) }

  def deriveAndPatch[From: Type, Patch: Type](
    from:         Expr[From],
    patch:        Expr[Patch]
  )(using quotes: Quotes): Expr[From] =
    '{
      _root_.pipez.dsl.PatchApplier
        .derive[Patch, From](_root_.pipez.dsl.PatchApplier.Config[Patch, From].addFallbackToValue(${ from }))
        .apply(${ patch })
    }

  def deriveAndPatchWithConfig[From: Type, Patch: Type](
    from:         Expr[From],
    patch:        Expr[Patch],
    config:       Expr[PatchApplier.Config[Patch, From]]
  )(using quotes: Quotes): Expr[From] =
    '{ _root_.pipez.dsl.PatchApplier.derive[Patch, From](${ config }.addFallbackToValue(${ from })).apply(${ patch }) }
