package scalaz.syntax.effect

trait EffectSyntaxes {

  object liftIO extends ToLiftIOV
  
  object all extends ToAllEffectTypeClassV
}

trait ToAllEffectTypeClassV
    extends ToLiftIOV

/**The members of this object are also offered in the package object [[scalaz.syntax.effect]] */
object EffectSyntax extends EffectSyntaxes
