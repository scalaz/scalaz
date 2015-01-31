package scalaz
package effect

/** 
 * A typeclass for capturing effects. This provides a mechanism for abstracting over effectful 
 * monads such as [[scalaz.concurrent.Task]] and [[scalaz.effect.IO]].
 */
trait Capture[F[_]] {

  /** Capture an effect. This operation is referentially transparent for all A. */
  def capture[A](a: => A): F[A]

  /** Extract the value from the given `F[A]`, performing any captured effect. */
  def unsafeRun[A](fa: F[A]): A

}

object Capture {
  @inline def apply[F[_]](implicit F: Capture[F]): Capture[F] = F
}

