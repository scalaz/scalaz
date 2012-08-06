package scalaz

private[scalaz] trait CoproductFunctor[F[_], G[_]] extends Functor[({type λ[α] = (F[α] \/ G[α])})#λ] {
  implicit def F: Functor[F]

  implicit def G: Functor[G]

  override def map[A, B](e: (F[A] \/ G[A]))(f: (A) => B): (F[B] \/ G[B]) =
    e match {
      case -\/(fa) => -\/(F.map(fa)(f))
      case \/-(gb) => \/-(G.map(gb)(f))
    }
}
