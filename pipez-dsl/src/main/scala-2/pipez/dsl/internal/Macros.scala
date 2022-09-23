package pipez.dsl.internal

import pipez.dsl.{ Converter, Parser }

import scala.reflect.macros.{ TypecheckException, blackbox }

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
}
