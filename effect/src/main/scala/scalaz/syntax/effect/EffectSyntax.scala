package scalaz.syntax.effect

object id extends ToIdOps

object resource extends ToResourceOps

object monadCatchIO extends ToMonadCatchIOOps

object all extends ToAllEffectTypeClassOps

trait ToAllEffectTypeClassOps
    extends ToIdOps with ToResourceOps with ToMonadCatchIOOps
