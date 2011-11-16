package scalaz
package effect

object Effect extends Effects

trait Effects
  extends Dups
  with FinalizerHandles
  with IoExceptionOrs
  with IOFunctions
  with IORefs
  with RefCountedFinalizers
  with RegionTFunctions
  with STs
  with syntax.effect.ToAllEffectTypeClassV
  with std.effect.AllEffectInstances
