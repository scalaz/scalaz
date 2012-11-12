package scalaz.syntax.effect

trait EffectSyntaxes {

  object resource extends ToResourceOps

  object monadCatchIO extends ToMonadCatchIOOps
  
  object all extends ToAllEffectTypeClassOps
}

trait ToAllEffectTypeClassOps
    extends ToResourceOps with ToMonadCatchIOOps

/**The members of this object are also offered in the package object [[scalaz.syntax.effect]] */
object EffectSyntax extends EffectSyntaxes
