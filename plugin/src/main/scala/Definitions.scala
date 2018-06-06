package scalaz
package plugin

import scala.tools.nsc.Global

abstract class Definitions {
  val global: Global
  import global._, analyzer._

  def init(): Unit = {
    ScalazPackage
    ScalazMetaPackage
    MinimalAttr
  }

  /* Create some classes that are otherwise defined in base, so that they're
   * available when compiling base.
   */

  lazy val ScalazPackage     = ensurePackage(rootMirror.RootClass, "scalaz")
  lazy val ScalazMetaPackage = ensurePackage(ScalazPackage.moduleClass, "meta")

  // class minimal(defns: Any*) extends StaticAnnotation
  lazy val MinimalAttr =
    rootMirror.getClassIfDefined(TypeName("scalaz.meta.minimal")).orElse {
      val scope = newScope
      val ann = ScalazMetaPackage.moduleClass
        .newClassWithInfo(TypeName("minimal"),
                          definitions.StaticAnnotationClass.typeOfThis :: Nil,
                          scope,
                          newFlags = Flag.SYNTHETIC)
      val ctor = ann.newConstructor(NoPosition)
      val param = ctor
        .newValueParameter(TermName("defns"))
        .setInfo(typeRef(NoPrefix, definitions.RepeatedParamClass, definitions.AnyTpe :: Nil))
      ann.info.decls.enter {
        ctor.setInfo(MethodType(param :: Nil, ann.typeOfThis))
      }

      ScalazMetaPackage.moduleClass.info.decls.enter(ann)
    }

  private def ensurePackage(owner: Symbol, name: String): Symbol = {
    val emptyTempl = (
      Template(Nil, noSelfType, Nil)
        setSymbol NoSymbol
        setType NoType
    )
    rootMirror.getPackageIfDefined(TermName(owner.name + name)).orElse {
      newNamer(NoContext.make(emptyTempl, owner, owner.info.decls))
        .createPackageSymbol(NoPosition, Ident(TermName(name)))
    }
  }
}
