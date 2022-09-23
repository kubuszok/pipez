package pipez

import scala.language.experimental.macros

package object dsl {

  implicit final class ConversionOps[From](private val from: From) extends AnyVal {

    @inline def convertInto[To](implicit convert: Converter[From, To]): To = convert.convert(from)

    def convertInto[To](config: Converter.Config[From, To]): To = macro dsl.internal.Macros.deriveAndConvert[From, To]

    @inline def parseFastInto[To](implicit parser: Parser[From, To]): Parser.ParsingResult[To] = parser.parseFast(from)

    def parseFastInto[To](
      config: Parser.Config[From, To]
    ): Parser.ParsingResult[To] = macro dsl.internal.Macros.deriveAndParseFast[From, To]

    @inline def parseFullInto[To](implicit parser: Parser[From, To]): Parser.ParsingResult[To] = parser.parseFull(from)

    def parseFullInto[To](
      config: Parser.Config[From, To]
    ): Parser.ParsingResult[To] = macro dsl.internal.Macros.deriveAndParseFull[From, To]

    @inline def __from: From = from
  }
}
