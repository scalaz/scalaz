package scalaz.syntax.effect

trait EffectSyntaxes {

  object resource extends ToResourceV
  
  object all extends ToAllEffectTypeClassV
}

trait ToAllEffectTypeClassV
    extends ToResourceV

/**The members of this object are also offered in the package object [[scalaz.syntax.effect]] */
object EffectSyntax extends EffectSyntaxes
