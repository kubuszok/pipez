package pipez

import scala.language.experimental.macros

package object dsl {

  implicit final class ConversionOps[From](private val from: From) extends AnyVal {

    /** Converts values without possibility to fail using default configuration */
    @inline def convertInto[To](implicit convert: Converter[From, To]): To = convert.convert(from)

    /** Converts values without possibility to fail using provided configuration */
    def convertInto[To](config: Converter.Config[From, To]): To = macro dsl.internal.Macros.deriveAndConvert[From, To]

    /** Converts values with possibility to fail-fast using default configuration */
    @inline def parseFastInto[To](implicit parser: Parser[From, To]): Parser.ParsingResult[To] = parser.parseFast(from)

    /** Converts values with possibility to fail-fast using provided configuration */
    def parseFastInto[To](
      config: Parser.Config[From, To]
    ): Parser.ParsingResult[To] = macro dsl.internal.Macros.deriveAndParseFast[From, To]

    /** Converts values with possibility to full-error-aggregation using default configuration */
    @inline def parseFullInto[To](implicit parser: Parser[From, To]): Parser.ParsingResult[To] = parser.parseFull(from)

    /** Converts values with possibility to full-error-aggregation using provided configuration */
    def parseFullInto[To](
      config: Parser.Config[From, To]
    ): Parser.ParsingResult[To] = macro dsl.internal.Macros.deriveAndParseFull[From, To]

    @inline def __from: From = from
  }
}
