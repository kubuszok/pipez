package pipez.dsl.internal

import pipez.dsl.Converter

import scala.quoted.*

object Macros:

  def deriveAndConvert[From: Type, To: Type](
    from:         Expr[From],
    config:       Expr[Converter.Config[From, To]]
  )(using quotes: Quotes): Expr[To] =
    '{ _root_.pipez.dsl.Converter.derive[From, To](${ config }).convert(${ from }) }
