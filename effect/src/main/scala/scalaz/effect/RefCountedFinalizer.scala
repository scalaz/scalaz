package scalaz
package effect

sealed trait RefCountedFinalizer {
  val finalizer: IO[Unit]
  val refcount: IORef[Int]
}

object RefCountedFinalizer extends RefCountedFinalizers

trait RefCountedFinalizers {
  val refCountedFinalizer: IO[Unit] => IORef[Int] => RefCountedFinalizer =
    u => i => new RefCountedFinalizer {
      val finalizer = u
      val refcount = i
    }
}