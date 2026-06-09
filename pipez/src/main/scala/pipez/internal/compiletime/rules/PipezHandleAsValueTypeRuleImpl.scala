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

    def apply[In: Type, Out: Type](implicit ctx: DerivationCtx[In, Out]): MIO[Rule.Applicability[Expr[Pipe[In, Out]]]] =
      Log.info(s"Attempting value type conversion") >> {
        val inIsValue = Type[In] match { case IsValueType(_) => true; case _ => false }
        val outIsValue = Type[Out] match { case IsValueType(_) => true; case _ => false }
        val inIsPrim = isPrimitive[In]
        val outIsPrim = isPrimitive[Out]

        if ((inIsValue || outIsValue) && (inIsValue || inIsPrim) && (outIsValue || outIsPrim))
          MIO {
            val (inInnerType, unwrapFn) = Type[In] match {
              case IsValueType(isValue) =>
                import isValue.{Underlying as Inner, value as proof}
                (Type[Inner].as_??, (in: Expr[Any]) => proof.unwrap(in.asInstanceOf[Expr[In]]).asInstanceOf[Expr[Any]])
              case _ =>
                (Type[In].as_??, (in: Expr[Any]) => in)
            }

            val (outInnerType, wrapFn) = Type[Out] match {
              case IsValueType(isValue) =>
                import isValue.{Underlying as Inner, value as proof}
                (
                  Type[Inner].as_??,
                  (v: Expr[Any]) => proof.wrap.apply(v.asInstanceOf[Expr[Inner]]).asInstanceOf[Expr[Any]]
                )
              case _ =>
                (Type[Out].as_??, (v: Expr[Any]) => v)
            }

            {
              import inInnerType.Underlying as InInner
              import outInnerType.Underlying as OutInner
              if (!(Type[InInner] <:< Type[OutInner]))
                throw new RuntimeException(
                  s"AnyVal conversion impossible: ${Type[InInner].prettyPrint} is not a subtype of ${Type[OutInner].prettyPrint}"
                )
            }

            Rule.matched(generateLift[In, Out] { (in, _) =>
              generatePureResult(wrapFn(unwrapFn(in)))
            })
          }
        else
          MIO.pure(Rule.yielded(s"Not a value type conversion"))
      }
  }
}
