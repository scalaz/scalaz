package scalaz.syntax.effect

trait EffectSyntaxes {

  object resource extends ToResourceOps
  
  object all extends ToAllEffectTypeClassOps
}

trait ToAllEffectTypeClassOps
    extends ToResourceOps

/**The members of this object are also offered in the package object [[scalaz.syntax.effect]] */
object EffectSyntax extends EffectSyntaxes
