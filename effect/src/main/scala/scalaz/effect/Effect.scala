package scalaz
package effect

object Effect extends Effects

trait Effects
  extends syntax.effect.ToAllEffectTypeClassV
  with std.effect.AllEffectInstances
