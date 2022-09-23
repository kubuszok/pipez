package pipez.dsl.internal

import pipez.dsl.Converter

import scala.reflect.macros.{ TypecheckException, blackbox }

class Macros(val c: blackbox.Context) {

  import c.universe.*

  def deriveAndConvert[From: WeakTypeTag, To: WeakTypeTag](
    config: Expr[Converter.Config[From, To]]
  ): c.Expr[To] = c.Expr[To](
    q"_root_.pipez.dsl.Converter.derive[${weakTypeOf[From]}, ${weakTypeOf[To]}]($config).convert(${c.prefix}.__from)"
  )
}
