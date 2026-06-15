package pipez.internal.compiletime.rules

import hearth.*
import hearth.fp.effect.*
import hearth.std.*
import pipez.internal.compiletime.PipezMacrosImpl

trait PipezHandleAsValueTypeRuleImpl { this: PipezMacrosImpl & MacroCommons & StdExtensions =>

  object PipezHandleAsValueTypeRule extends PipezRule("handle as value type (AnyVal)") {

    private def isPrimitive[A: Type]: Boolean =
      Type[A] <:< Type.of[Boolean] ||
        Type[A] <:< Type.of[Byte] ||
        Type[A] <:< Type.of[Short] ||
        Type[A] <:< Type.of[Int] ||
        Type[A] <:< Type.of[Long] ||
        Type[A] <:< Type.of[Float] ||
        Type[A] <:< Type.of[Double] ||
        Type[A] <:< Type.of[Char] ||
        Type[A] <:< Type.of[String]

    def apply[In: Type, Out: Type](in: Expr[In], ctx: Expr[Ctx])(implicit
        dctx: DerivationCtx[In, Out]
    ): MIO[Rule.Applicability[Expr[Res[Out]]]] =
      Log.info(s"Attempting value type conversion for ${Type[In].prettyPrint} => ${Type[Out].prettyPrint}") >> {
        val inIsValue = Type[In] match { case IsValueType(_) => true; case _ => false }
        val outIsValue = Type[Out] match { case IsValueType(_) => true; case _ => false }
        val inIsPrim = isPrimitive[In]
        val outIsPrim = isPrimitive[Out]

        if ((inIsValue || outIsValue) && (inIsValue || inIsPrim) && (outIsValue || outIsPrim))
          MIO(Rule.matched(convertValue[In, Out](in)))
        else
          MIO.pure(Rule.yielded(s"Not a value type conversion"))
      }

    /** `In` (a value type or primitive) -> its inner value -> wrapped into `Out`, all kept typed via `upcast`. */
    private def convertValue[In: Type, Out: Type](in: Expr[In]): Expr[Res[Out]] = {
      // Unwrap `In` to its inner value (the value type's payload, or `In` itself for a primitive).
      val innerValue: Expr_?? = Type[In] match {
        case IsValueType(isValue) =>
          import isValue.Underlying as Inner
          isValue.value.unwrap(in).as_??
        case _ =>
          in.as_??
      }
      import innerValue.Underlying as InInner

      // Wrap the inner value (upcast to `Out`'s inner type) back into `Out`.
      Type[Out] match {
        case IsValueType(isValue) =>
          import isValue.Underlying as OutInner
          if (!(Type[InInner] <:< Type[OutInner]))
            throw new RuntimeException(
              s"AnyVal conversion impossible: ${Type[InInner].prettyPrint} is not a subtype of ${Type[OutInner].prettyPrint}"
            )
          // `wrap.apply` yields the wrapped value as `Expr[wrap.Result[Out]]` with `wrap.Result[Out] =:= Out`; bridge
          // that Hearth value-type boundary to `Expr[Out]`.
          val wrapped: Expr[Out] = isValue.value.wrap.apply(innerValue.value.upcast[OutInner]).asInstanceOf[Expr[Out]]
          generatePureResult[Out](wrapped)
        case _ =>
          if (!(Type[InInner] <:< Type[Out]))
            throw new RuntimeException(
              s"AnyVal conversion impossible: ${Type[InInner].prettyPrint} is not a subtype of ${Type[Out].prettyPrint}"
            )
          generatePureResult[Out](innerValue.value.upcast[Out])
      }
    }
  }
}
