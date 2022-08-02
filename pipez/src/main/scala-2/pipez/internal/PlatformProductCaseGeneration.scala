package pipez.internal

import pipez.PipeDerivation

import scala.collection.immutable.ListMap
import scala.util.chaining._

trait PlatformProductCaseGeneration[Pipe[_, _], In, Out] extends ProductCaseGeneration[Pipe, In, Out] {
  self: PlatformDefinitions[Pipe, In, Out] with PlatformGenerators[Pipe, In, Out] =>

  import c.universe._

  final def isSubtype[A, B](lower: Type[A], higher: Type[B]): Boolean =
    lower <:< higher

  final def isCaseClass[A](tpe: Type[A]): Boolean =
    tpe.typeSymbol.isClass &&
      tpe.typeSymbol.asClass.isCaseClass
  final def isJavaBean[A](tpe: Type[A]): Boolean =
    tpe.typeSymbol.isClass &&
      tpe.members.exists(m => m.isPublic && m.isMethod && m.asMethod.isSetter) &&
      tpe.members.exists(m => m.isPublic && m.isConstructor && m.asMethod.paramLists.flatten.isEmpty)
  final def isInstantiable[A](tpe: Type[A]): Boolean =
    !tpe.typeSymbol.isAbstract && tpe.members.exists(m => m.isPublic && m.isConstructor)

  object ProductTypeConversion extends ProductTypeConversion {
    // TODO: implement abstract members

    override def extractInData(settings: Settings): DerivationResult[ProductInData] =
      inType.members // we fetch ALL members, even those that might have been inherited
        .to(List)
        .collect {
          case member if member.isMethod && member.asMethod.isGetter =>
            member.name.toString -> ProductInData.Getter[Any](
              name = member.name.toString, // TODO
              tpe = member.asMethod.returnType,
              caller = (arg: Argument[In]) => c.Expr[Any](q"$arg.${member.asMethod.name.toTermName}")
            )
        }
        .to(ListMap)
        .pipe(ProductInData(_))
        .pipe(DerivationResult.pure)
        .tap(d => println(s"In getters: $d"))

    override def extractOutData(settings: Settings): DerivationResult[ProductOutData] =
      if (isJavaBean(outType)) {
        // Java Bean case

        val defaultConstructor = outType.decls.collectFirst {
          case member if member.isPublic && member.isConstructor && member.asMethod.paramLists.flatten.isEmpty =>
            c.Expr[Out](q"new ${outType}()")
        } match {
          case Some(value) => DerivationResult.pure(value)
          case None        => DerivationResult.fail(DerivationError.MissingPublicConstructor)
        }

        val setters = outType.decls
          .to(List)
          .collect {
            case member if member.isPublic && member.isMethod && member.asMethod.isSetter =>
              member.asMethod.paramLists.flatten.map { param =>
                param.name.toString -> ProductOutData.Setter(
                  name = param.name.toString,
                  tpe = param.typeSignature,
                  caller = (_: Argument[In], _: CodeOf[Any]) => c.Expr[Unit](q"()")
                )
              }
          }
          .flatten
          .to(ListMap)
          .pipe(DerivationResult.pure(_))

        defaultConstructor.map2(setters)(ProductOutData.JavaBean(_, _)).tap(d => println(s"Out setters: $d"))
      } else {
        // case class case

        outType.decls
          .to(List)
          .collectFirst {
            case member if member.isPublic && member.isConstructor =>
              member.asMethod.paramLists.map { params =>
                params
                  .map { param =>
                    param.name.toString -> ProductOutData.ConstructorParam(
                      name = param.name.toString,
                      tpe = param.typeSignature
                    )
                  }
                  .to(ListMap)
              }
          }
          .get
          .pipe(ProductOutData.CaseClass(_))
          .pipe(DerivationResult.pure(_))
          .tap(d => println(s"Out params: $d"))
      }

    override def generateCode(generatorData: ProductGeneratorData): DerivationResult[CodeOf[Pipe[In, Out]]] = {
      println(s"Derivation so far: data=$generatorData, pipe=$pipeDerivation")
      DerivationResult.fail(DerivationError.NotYetSupported)
    }
  }
}
