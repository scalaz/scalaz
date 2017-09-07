package scalaz
package typeclass

import Liskov.<~<

trait IsCovariantClass[F[_]] extends IsCovariant[F] {
  final def isCovariant: IsCovariant[F] = this
}

object IsCovariantClass {

  trait SubstCv[F[_]] extends Alt[SubstCv[F]] { self: IsCovariant[F] =>
    override def substCv[G[+_], A, B](g: G[F[A]])(implicit ev: A <~< B): G[F[B]]
 
    override def substCt[G[-_], A, B](g: G[F[B]])(implicit ev: A <~< B): G[F[A]] = {
      type H[+a] = G[a] => G[F[A]]
      substCv[H, A, B](identity[G[F[A]]]).apply(g)
    }
 
    override def liftLiskov[A, B](implicit ev: A <~< B): F[A] <~< F[B] =
      substCv[F[A] <~< +?, A, B](Liskov.refl[F[A]])
 
    override def widen[A, B](fa: F[A])(implicit ev: A <~< B): F[B] =
      substCv[F[A] => +?, A, B](identity[F[A]]).apply(fa)
  }

  trait SubstCt[F[_]] extends Alt[SubstCt[F]] { self: IsCovariant[F] =>
    override def substCt[G[-_], A, B](g: G[F[B]])(implicit ev: A <~< B): G[F[A]]
 
    override def substCv[G[+_], A, B](g: G[F[A]])(implicit ev: A <~< B): G[F[B]] = {
      type H[-a] = G[a] => G[F[B]]
      substCt[H, A, B](identity[G[F[B]]]).apply(g)
    }
 
    override def liftLiskov[A, B](implicit ev: A <~< B): F[A] <~< F[B] =
      substCt[-? <~< F[B], A, B](Liskov.refl[F[B]])
 
    override def widen[A, B](fa: F[A])(implicit ev: A <~< B): F[B] =
      substCt[-? => F[B], A, B](identity[F[B]]).apply(fa)
  }

  trait LiftLiskov[F[_]] extends Alt[LiftLiskov[F]] { self: IsCovariant[F] =>
    override def liftLiskov[A, B](implicit ev: A <~< B): F[A] <~< F[B]

    override def substCv[G[+_], A, B](g: G[F[A]])(implicit ev: A <~< B): G[F[B]] =
      liftLiskov[A, B].substCv[G](g)

    override def substCt[G[-_], A, B](g: G[F[B]])(implicit ev: A <~< B): G[F[A]] =
      liftLiskov[A, B].subst[G](g)

    override def widen[A, B](fa: F[A])(implicit ev: A <~< B): F[B] =
      liftLiskov[A, B].apply(fa)
  }

  trait Alt[D <: Alt[D]] { self: D => }
}
