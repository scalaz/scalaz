package scalaz


trait =~~=[F[_], G[_]] {
  def ~~=>[A](a: F[A]): G[A]

  def <=~~[A](a: G[A]): F[A]
}

object =~~= extends ==~~==

trait ==~~== {
  implicit def Iso[F[_]]: (F =~~= F) = new (F =~~= F) {
    def ~~=>[A](a: F[A]) = a

    def <=~~[A](a: F[A]) = a
  }

  implicit def ~~=>[F[_], A](a: F[A])(implicit i: F =~~= Identity): A =
    (i ~~=> a).value

  implicit def <=~~[F[_], A](a: A)(implicit i: F =~~= Identity): F[A] =
    i <=~~ Identity.id(a)

  implicit def IsoFunctor[F[_]](implicit i: F =~~= Identity): Functor[F] =
    new Functor[F] {
      def fmap[A, B](f: A => B) =
        k => <=~~[F, B](f(~~=>(k)))
    }

  implicit def IsoPointed[F[_]](implicit i: F =~~= Identity): Pointed[F] =
    new Pointed[F] {
      def point[A](a: => A) = i <=~~ Identity.id(a)
    }

  implicit def IsoBind[F[_]](implicit i: F =~~= Identity): Bind[F] =
    new Bind[F] {
      def bind[A, B](f: A => F[B]) =
        k => <=~~[F, B](~~=>(f(~~=>(k))))
    }

  implicit def IsoMonad[F[_]](implicit i: F =~~= Identity): Monad[F] =
    Monad.monadBP

}
