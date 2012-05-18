package scalaz
package effect

object Effect extends Effects

trait Effects
  extends syntax.effect.ToAllEffectTypeClassOps
  with std.effect.AllEffectInstances
