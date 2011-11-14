package scalaz
package effect

object Effect extends Effects

trait Effects
  extends Dups
  with FinalizerHandles
  with IoExceptionOrs
  with IOFunctions
  with IORefs
  with LiftControlIOs
  with MonadControlIOs
  with RefCountedFinalizers
  with RegionTFunctions
  with STs
