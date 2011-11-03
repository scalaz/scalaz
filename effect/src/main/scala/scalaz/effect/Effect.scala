package scalaz
package effect

object Effect extends Effects

trait Effects
  extends Dups
  with FinalizerHandles
  with IoExceptionOrs
  with IOs
  with IORefs
  with LiftControlIOs
  with MonadControlIOs
  with MonadIOs
  with RefCountedFinalizers
  with RegionTs
  with STs
