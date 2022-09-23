package pipez.dsl

extension [From](from: From)

  inline def convertInto[To](implicit convert: Converter[From, To]): To = convert.convert(from)

  inline def convertInto[To](inline config: Converter.Config[From, To]): To =
    ${ internal.Macros.deriveAndConvert[From, To]('{ from }, '{ config }) }

  inline def parseFastInto[To](implicit parser: Parser[From, To]): Parser.ParsingResult[To] = parser.parseFast(from)

  inline def parseFastInto[To](inline config: Parser.Config[From, To]): Parser.ParsingResult[To] =
    ${ internal.Macros.deriveAndParseFast[From, To]('{ from }, '{ config }) }

  inline def parseFullInto[To](implicit parser: Parser[From, To]): Parser.ParsingResult[To] = parser.parseFull(from)

  inline def parseFullInto[To](inline config: Parser.Config[From, To]): Parser.ParsingResult[To] =
    ${ internal.Macros.deriveAndParseFull[From, To]('{ from }, '{ config }) }
end extension
