package pipez.dsl

extension [From](from: From)

  inline def convertInto[To](implicit convert: Converter[From, To]): To = convert.convert(from)

  inline def convertInto[To](inline config: Converter.Config[From, To]): To =
    ${ internal.Macros.deriveAndConvert[From, To]('{ from }, '{ config }) }
end extension
