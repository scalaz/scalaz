package scalaz
package effect

sealed trait RefCountedFinalizer {
  val finalizer: IO[Unit]
  val refcount: IORef[Int]
}

object RefCountedFinalizer extends RefCountedFinalizers {
  def apply(u: IO[Unit], i: IORef[Int]): RefCountedFinalizer = refCountedFinalizer(u, i)
}

trait RefCountedFinalizers {
  def refCountedFinalizer(u: IO[Unit], i: IORef[Int]): RefCountedFinalizer =
    new RefCountedFinalizer {
      val finalizer = u
      val refcount = i
    }
}