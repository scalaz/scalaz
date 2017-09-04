package scalaz
package typeclass

import Liskov.<~<

trait IsContravariantClass[F[_]] extends IsContravariant[F] {
  final def isContravariant: IsContravariant[F] = this
}

object IsContravariantClass {

  trait SubstCv[F[_]] extends Alt[SubstCv[F]] { self: IsContravariant[F] =>
    override def substCv[G[+_], A, B](g: G[F[B]])(implicit ev: A <~< B): G[F[A]]
 
    override def substCt[G[-_], A, B](g: G[F[A]])(implicit ev: A <~< B): G[F[B]] = {
      type H[+a] = G[a] => G[F[B]]
      substCv[H, A, B](identity[G[F[B]]]).apply(g)
    }
 
    override def liftLiskov[A, B](implicit ev: A <~< B): F[B] <~< F[A] =
      substCv[F[B] <~< +?, A, B](Liskov.refl[F[B]])
 
    override def widen[A, B](fb: F[B])(implicit ev: A <~< B): F[A] =
      substCv[F[B] => +?, A, B](identity[F[B]]).apply(fb)
  }

  trait SubstCt[F[_]] extends Alt[SubstCt[F]] { self: IsContravariant[F] =>
    override def substCt[G[-_], A, B](g: G[F[A]])(implicit ev: A <~< B): G[F[B]]
 
    override def substCv[G[+_], A, B](g: G[F[B]])(implicit ev: A <~< B): G[F[A]] = {
      type H[-a] = G[a] => G[F[A]]
      substCt[H, A, B](identity[G[F[A]]]).apply(g)
    }
 
    override def liftLiskov[A, B](implicit ev: A <~< B): F[B] <~< F[A] =
      substCt[-? <~< F[A], A, B](Liskov.refl[F[A]])
 
    override def widen[A, B](fb: F[B])(implicit ev: A <~< B): F[A] =
      substCt[-? => F[A], A, B](identity[F[A]]).apply(fb)
  }

  trait LiftLiskov[F[_]] extends Alt[LiftLiskov[F]] { self: IsContravariant[F] =>
    override def liftLiskov[A, B](implicit ev: A <~< B): F[B] <~< F[A]

    override def substCv[G[+_], A, B](g: G[F[B]])(implicit ev: A <~< B): G[F[A]] =
      liftLiskov[A, B].substCv[G](g)

    override def substCt[G[-_], A, B](g: G[F[A]])(implicit ev: A <~< B): G[F[B]] =
      liftLiskov[A, B].subst[G](g)

    override def widen[A, B](fb: F[B])(implicit ev: A <~< B): F[A] =
      liftLiskov[A, B].apply(fb)
  }

  trait Alt[D <: Alt[D]] { self: D => }
}
