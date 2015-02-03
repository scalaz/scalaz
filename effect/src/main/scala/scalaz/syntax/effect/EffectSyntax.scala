package scalaz.syntax.effect

trait EffectSyntaxes {

  object id extends ToIdOps

  object resource extends ToResourceOps

  object monadCatchIO extends ToMonadCatchIOOps
  
  object capture extends ToCaptureOps

  object all extends ToAllEffectTypeClassOps

}

trait ToAllEffectTypeClassOps
    extends ToIdOps with ToResourceOps with ToMonadCatchIOOps with ToCaptureOps

/**The members of this object are also offered in the package object [[scalaz.syntax.effect]] */
object EffectSyntax extends EffectSyntaxes
