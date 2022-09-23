package pipez.dsl.internal

import pipez.dsl.{ Converter, Parser }

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
