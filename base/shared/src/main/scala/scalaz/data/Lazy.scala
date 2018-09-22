package scalaz
package data

import tc._

class Lazy[A](val run: () => A) {
  lazy val value = run()
}

object Lazy {
  implicit def lazyDelay[A]: Delay[Lazy[A]] =
    instanceOf[DelayClass[Lazy[A]]](fa => new Lazy[A](() => fa().run()))

  implicit def lazyMonad: Monad[Lazy] =
    instanceOf(new MonadClass[Lazy] {
      def pure[A](a: A): Lazy[A] = new Lazy(() => a)

      def flatMap[A, B](ma: Lazy[A])(f: A => Lazy[B]): Lazy[B] =
        new Lazy[B](() => f(ma.run()).run())

      def flatten[A](ma: Lazy[Lazy[A]]): Lazy[A] =
        new Lazy[A](() => ma.run().run())

      def ap[A, B](fa: Lazy[A])(f: Lazy[A => B]): Lazy[B] =
        new Lazy[B](() => f.run()(fa.run()))

      def map[A, B](ma: Lazy[A])(f: A => B): Lazy[B] =
        new Lazy[B](() => f(ma.run()))
    })

}
