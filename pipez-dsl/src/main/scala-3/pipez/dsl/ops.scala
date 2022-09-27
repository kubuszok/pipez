package pipez.dsl

extension [From](from: From)

  /** Converts values without possibility to fail using default configuration */
  inline def convertInto[To](implicit convert: Converter[From, To]): To = convert.convert(from)

  /** Converts values without possibility to fail using provided configuration */
  inline def convertInto[To](inline config: Converter.Config[From, To]): To =
    ${ internal.Macros.deriveAndConvert[From, To]('{ from }, '{ config }) }

  /** Converts values with possibility to fail-fast using default configuration */
  inline def parseFastInto[To](implicit parser: Parser[From, To]): Parser.ParsingResult[To] = parser.parseFast(from)

  /** Converts values with possibility to fail-fast using provided configuration */
  inline def parseFastInto[To](inline config: Parser.Config[From, To]): Parser.ParsingResult[To] =
    ${ internal.Macros.deriveAndParseFast[From, To]('{ from }, '{ config }) }

  /** Converts values with possibility to full-error-aggregation using default configuration */
  inline def parseFullInto[To](implicit parser: Parser[From, To]): Parser.ParsingResult[To] = parser.parseFull(from)

  /** Converts values with possibility to full-error-aggregation using provided configuration */
  inline def parseFullInto[To](inline config: Parser.Config[From, To]): Parser.ParsingResult[To] =
    ${ internal.Macros.deriveAndParseFull[From, To]('{ from }, '{ config }) }
end extension
