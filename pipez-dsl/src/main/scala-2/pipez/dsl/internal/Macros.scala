package pipez.dsl.internal

import pipez.dsl.{ Converter, Parser, PatchApplier }

import scala.reflect.macros.blackbox

class Macros(val c: blackbox.Context) {

  import c.universe.*

  def deriveAndConvert[From: WeakTypeTag, To: WeakTypeTag](
    config: Expr[Converter.Config[From, To]]
  ): c.Expr[To] = c.Expr[To](
    q"_root_.pipez.dsl.Converter.derive[${weakTypeOf[From]}, ${weakTypeOf[To]}]($config).convert(${c.prefix}.__from)"
  )

  def deriveAndParseFast[From: WeakTypeTag, To: WeakTypeTag](
    config: Expr[Parser.Config[From, To]]
  ): c.Expr[Parser.ParsingResult[To]] = c.Expr[Parser.ParsingResult[To]](
    q"_root_.pipez.dsl.Parser.derive[${weakTypeOf[From]}, ${weakTypeOf[To]}]($config).parseFast(${c.prefix}.__from)"
  )

  def deriveAndParseFull[From: WeakTypeTag, To: WeakTypeTag](
    config: Expr[Parser.Config[From, To]]
  ): c.Expr[Parser.ParsingResult[To]] = c.Expr[Parser.ParsingResult[To]](
    q"_root_.pipez.dsl.Parser.derive[${weakTypeOf[From]}, ${weakTypeOf[To]}]($config).parseFull(${c.prefix}.__from)"
  )

  def deriveAndPatch[From: WeakTypeTag, Patch: WeakTypeTag](patch: Expr[Patch]): c.Expr[From] = c.Expr[From](
    q"""_root_.pipez.dsl.PatchApplier.derive[${weakTypeOf[Patch]}, ${weakTypeOf[From]}](
      _root_.pipez.dsl.PatchApplier.Config[${weakTypeOf[Patch]}, ${weakTypeOf[From]}]
        .addFallbackToValue(${c.prefix}.__from)
    ).apply($patch)"""
  )

  def deriveAndPatchWithConfig[From: WeakTypeTag, Patch: WeakTypeTag](
    patch:  Expr[Patch],
    config: Expr[PatchApplier.Config[Patch, From]]
  ): c.Expr[From] = c.Expr[From](
    q"""_root_.pipez.dsl.PatchApplier.derive[${weakTypeOf[Patch]}, ${weakTypeOf[From]}](
      $config.addFallbackToValue(${c.prefix}.__from)
    ).apply($patch)"""
  )
}
