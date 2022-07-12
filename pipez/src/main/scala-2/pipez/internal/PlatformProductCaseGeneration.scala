package pipez.internal

import scala.collection.immutable.ListMap
import scala.util.chaining._

trait PlatformProductCaseGeneration extends ProductCaseGeneration { self: PlatformDefinitions with Dispatchers =>

  import c.universe._

  final def isUsableAsProductInput[In](tpe: Type[In]): Boolean =
    isCaseClass(tpe) || isJavaBean(tpe) // TODO: possibly reconsider
  final def isUsableAsProductOutput[Out](tpe: Type[Out]): Boolean =
    (isCaseClass(tpe) || isJavaBean(tpe)) && !tpe.typeSymbol.isAbstract
  final def isCaseClass[A](tpe: Type[A]): Boolean =
    tpe.typeSymbol.isClass && tpe.typeSymbol.asClass.isCaseClass
  final def isJavaBean[A](tpe: Type[A]): Boolean =
    tpe.typeSymbol.isClass && tpe.members.exists(m => m.isPublic && m.isMethod && m.asMethod.isSetter) // TODO

  object ProductTypeConversion extends ProductTypeConversion {
    // TODO: implement abstract members

    override def inputData[In](inType: Type[In], settings: Settings): DerivationResult[InData] =
      inType.members // we fetch ALL members, even those that might have been inherited
        .to(List)
        .collect {
          case member if member.isMethod && member.asMethod.isGetter =>
            member.name.toString -> InData.GetterData[Any](
              name = member.name.toString, // TODO
              tpe = member.asMethod.returnType,
              caller = (arg: Argument) => c.Expr[Any](q"$arg.${member.asMethod.name.toTermName}")
            )
        }
        .to(ListMap)
        .pipe(InData(_))
        .pipe(DerivationResult.pure)
        .tap(d => println(s"Out getters: $d"))

    override def outputData[Out](outType: Type[Out], settings: Settings): DerivationResult[OutData] =
      if (isJavaBean(outType)) {
        // Java Bean case

        val defaultConstructor: DerivationResult[CodeOf[Out]] = outType.decls.collectFirst {
          case member if member.isPublic && member.isConstructor && member.asMethod.paramLists.flatten.isEmpty =>
            c.Expr[Out](q"new ${outType}()")
        } match {
          case Some(value) => DerivationResult.pure(value)
          case None =>
            DerivationResult.fail(
              DerivationError.NotSupportedCase(
                s"Output type ${outType} was recognized as JavaBean but had no default constructor"
              )
            )
        }

        val setters = outType.decls
          .to(List)
          .collect {
            case member if member.isPublic && member.isMethod && member.asMethod.isSetter =>
              member.asMethod.paramLists.flatten.map { param =>
                param.name.toString -> OutData.SetterData(
                  name = param.name.toString,
                  tpe = param.typeSignature,
                  caller = (_: Argument, _: CodeOf[Any]) => c.Expr[Unit](q"()")
                )
              }
          }
          .flatten
          .to(ListMap)
          .pipe(DerivationResult.pure(_))

        defaultConstructor.map2(setters)(OutData.JavaBeanData(_, _))
      } else {
        // case class case

        outType.decls
          .to(List)
          .collectFirst {
            case member if member.isPublic && member.isConstructor =>
              member.asMethod.paramLists.map { params =>
                params
                  .map { param =>
                    param.name.toString -> OutData.ConstructorParamData(
                      name = param.name.toString,
                      tpe = param.typeSignature
                    )
                  }
                  .to(ListMap)
              }
          }
          .map(OutData.CaseClassData(_)) match {
          case Some(value) => DerivationResult.pure(value)
          case None =>
            DerivationResult.fail(
              DerivationError.NotSupportedCase(
                s"Output type ${outType} was recognized as a case class but no public constructor found"
              )
            )
        }
      }.tap(d => println(s"Out setters: $d"))
  }
}
