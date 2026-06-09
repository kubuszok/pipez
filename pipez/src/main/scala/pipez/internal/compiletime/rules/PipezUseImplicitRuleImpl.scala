package pipez.internal.compiletime.rules

import hearth.*
import hearth.fp.effect.*
import hearth.std.*
import pipez.internal.compiletime.PipezMacrosImpl

import scala.annotation.nowarn

trait PipezUseImplicitRuleImpl { this: PipezMacrosImpl & MacroCommons & StdExtensions =>

  object PipezUseImplicitRule extends PipezRule("use implicit when available") {

    def apply[In: Type, Out: Type](using ctx: DerivationCtx[In, Out]): MIO[Rule.Applicability[Expr[Pipe[In, Out]]]] =
      Log.info(s"Attempting to use implicit Pipe[${Type[In].prettyPrint}, ${Type[Out].prettyPrint}]") >> {
        @nowarn("msg=unused") implicit val PipeIO: Type[Pipe[In, Out]] = pipeType[In, Out]
        summonPipe[In, Out] match {
          case Some(pipe) =>
            Log.info(s"Found implicit ${pipe.prettyPrint}") >>
              MIO.pure(Rule.matched(pipe))
          case None =>
            MIO.pure(Rule.yielded(s"No implicit Pipe[${Type[In].prettyPrint}, ${Type[Out].prettyPrint}] found"))
        }
      }
  }
}
