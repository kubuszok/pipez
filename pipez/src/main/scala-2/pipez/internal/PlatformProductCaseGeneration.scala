package pipez.internal

import pipez.PipeDerivation

import scala.collection.immutable.ListMap
import scala.util.chaining._

trait PlatformProductCaseGeneration extends ProductCaseGeneration { self: PlatformDefinitions with Dispatchers =>

  import c.universe._

  final def isUsableAsProductOutput[Out](tpe: Type[Out]): Boolean =
    (isCaseClass(tpe) || isJavaBean(tpe)) &&
      !tpe.typeSymbol.isAbstract &&
      tpe.members.exists(m => m.isPublic && m.isConstructor)
  final def isCaseClass[A](tpe: Type[A]): Boolean =
    tpe.typeSymbol.isClass &&
      tpe.typeSymbol.asClass.isCaseClass
  final def isJavaBean[A](tpe: Type[A]): Boolean =
    tpe.typeSymbol.isClass &&
      tpe.members.exists(m => m.isPublic && m.isMethod && m.asMethod.isSetter) &&
      tpe.members.exists(m => m.isPublic && m.isConstructor && m.asMethod.paramLists.flatten.isEmpty)

  object ProductTypeConversion extends ProductTypeConversion {
    // TODO: implement abstract members

    override def extractInData[In](inType: Type[In], settings: Settings): DerivationResult[InData] =
      inType.members // we fetch ALL members, even those that might have been inherited
        .to(List)
        .collect {
          case member if member.isMethod && member.asMethod.isGetter =>
            member.name.toString -> InData.Getter[Any](
              name = member.name.toString, // TODO
              tpe = member.asMethod.returnType,
              caller = (arg: Argument) => c.Expr[Any](q"$arg.${member.asMethod.name.toTermName}")
            )
        }
        .to(ListMap)
        .pipe(InData(_))
        .pipe(DerivationResult.pure)
        .tap(d => println(s"In getters: $d"))

    override def extractOutData[Out](outType: Type[Out], settings: Settings): DerivationResult[OutData] =
      if (isJavaBean(outType)) {
        // Java Bean case

        val defaultConstructor = outType.decls.collectFirst {
          case member if member.isPublic && member.isConstructor && member.asMethod.paramLists.flatten.isEmpty =>
            c.Expr[Out](q"new ${outType}()")
        } match {
          case Some(value) => DerivationResult.pure(value)
          case None        => DerivationResult.fail(DerivationError.MissingPublicConstructor(outType))
        }

        val setters = outType.decls
          .to(List)
          .collect {
            case member if member.isPublic && member.isMethod && member.asMethod.isSetter =>
              member.asMethod.paramLists.flatten.map { param =>
                param.name.toString -> OutData.Setter(
                  name = param.name.toString,
                  tpe = param.typeSignature,
                  caller = (_: Argument, _: CodeOf[Any]) => c.Expr[Unit](q"()")
                )
              }
          }
          .flatten
          .to(ListMap)
          .pipe(DerivationResult.pure(_))

        defaultConstructor.map2(setters)(OutData.JavaBean(_, _)).tap(d => println(s"Out setters: $d"))
      } else {
        // case class case

        outType.decls
          .to(List)
          .collectFirst {
            case member if member.isPublic && member.isConstructor =>
              member.asMethod.paramLists.map { params =>
                params
                  .map { param =>
                    param.name.toString -> OutData.ConstructorParam(
                      name = param.name.toString,
                      tpe = param.typeSignature
                    )
                  }
                  .to(ListMap)
              }
          }
          .get
          .pipe(OutData.CaseClass(_))
          .pipe(DerivationResult.pure(_))
          .tap(d => println(s"Out params: $d"))
      }

    override def generateCode[Pipe[_, _], In, Out](
      generatorData:  GeneratorData,
      pipeDerivation: CodeOf[PipeDerivation[Pipe]]
    ): DerivationResult[CodeOf[Pipe[In, Out]]] = {
      println(s"Derivation so far: data=$generatorData, pipe=$pipeDerivation")
      DerivationResult.fail(DerivationError.NotYetSupported)
    }
  }
}
