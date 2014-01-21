package scalaz

/** The yoneda lemma */
abstract class Yoneda[F[_], +A] {
  def map[B](f: A => B): F[B]
}

/** The dual view of the yoneda lemma */
abstract class Coyoneda[F[_], +A] {
  type I
  def fi: F[I]
  def k(i: I): A
}

object Yoneda {

  /** `Yoneda[F,_]` is a functor for any `F` */
  implicit def yonedaFunctor[F[_]]: Functor[({type λ[α] = Yoneda[F,α]})#λ] =
    new Functor[({type λ[α] = Yoneda[F,α]})#λ] {
      def map[A,B](ya: Yoneda[F,A])(f: A => B) = new Yoneda[F,B] {
        def map[C](g: B => C) = ya.map(f andThen g)
      }
    }

  /** `Coyoneda[F,_]` is a functor for any `F` */
  implicit def coyonedaFunctor[F[_]]: Functor[({type λ[α] = Coyoneda[F,α]})#λ] =
    new Functor[({type λ[α] = Coyoneda[F,α]})#λ] {
      def map[A,B](ya: Coyoneda[F,A])(f: A => B) = new Coyoneda[F,B] {
        type I = ya.I
        val fi = ya.fi
        def k(i: I) = f(ya.k(i))
      }
    }

  /** `F[A]` is isomorphic to `Yoneda[F,A]` */
  def toYo[F[_]:Functor,A](fa: F[A]) = new Yoneda[F,A] {
    def map[B](f: A => B) = Functor[F].map(fa)(f)
  }

  /** `Yoneda[F,A]` is isomorphic to `F[A]` */
  def froYo[F[_],A](yo: Yoneda[F,A]) = yo.map(a => a)

  /** `F[A]` is isomorphic to `Coyoneda[F,A]` */
  def toCoyo[F[_],A](fa: F[A]) = new Coyoneda[F,A] {
    type I = A
    def k(a: A) = a
    val fi = fa
  }

  /** `Coyoneda[F,A]` is isomorphic to `F[A]` */
  def froCoyo[F[_]:Functor,A](yo: Coyoneda[F,A]) = Functor[F].map(yo.fi)(yo.k)
}
