package scalaz

final case class Cokleisli[F[_], A, B](run: F[A] => B) { self =>
  def apply(fa: F[A]): B =
    run(fa)


  def dimap[C, D](f: C => A, g: B => D)(implicit b: Functor[F]): Cokleisli[F, C, D] =
    Cokleisli(c => g(run(b.map(c)(f)))) // b.map(run(f(c)))(g))

  def contramapValue[C](f: F[C] => F[A]): Cokleisli[F, C,  B] = Cokleisli(run compose f)

  def map[C](f: B => C): Cokleisli[F, A, C] = Cokleisli(f compose run)

  def flatMap[C](f: B => Cokleisli[F, A, C]): Cokleisli[F, A, C] =
    Cokleisli(fa => f(self.run(fa)).run(fa))

  def <<=(a: F[A])(implicit F: Cobind[F]): F[B] =
    F.extend(a)(run)

  def =>=[C](c: Cokleisli[F, B, C])(implicit F: Cobind[F]): Cokleisli[F, A, C] =
    Cokleisli(fa => c run (<<=(fa)))

  def compose[C](c: Cokleisli[F, C, A])(implicit F: Cobind[F]): Cokleisli[F, C, B] =
    c =>= this

  def =<=[C](c: Cokleisli[F, C, A])(implicit F: Cobind[F]): Cokleisli[F, C, B] =
    compose(c)

  import Leibniz.===
  def endo(implicit ev: B === A): Endomorphic[Cokleisli[F, *, *], A] =
    Endomorphic[Cokleisli[F, *, *], A](ev.subst[Cokleisli[F, A, *]](this))
}

object Cokleisli extends CokleisliInstances {

}

sealed abstract class CokleisliInstances0 {
  implicit def cokleisliCompose[F[_]](implicit F0: Cobind[F]): Compose[Cokleisli[F, *, *]] =
    new CokleisliCompose[F] {
      override def F = F0
    }
  implicit def cokleisliProfunctor[F[_]: Functor]: Profunctor[Cokleisli[F, *, *]] =
    new CokleisliProfunctor[F] {
      def F = implicitly
    }
}

sealed abstract class CokleisliInstances extends CokleisliInstances0 {
  implicit def cokleisliMonad[F[_], R]: Monad[Cokleisli[F, R, *]] with BindRec[Cokleisli[F, R, *]] =
    new CokleisliMonad[F, R] {}

  implicit def cokleisliArrow[F[_]](implicit F0: Comonad[F]): Arrow[Cokleisli[F, *, *]] with ProChoice[Cokleisli[F, *, *]] =
    new CokleisliArrow[F] {
      override def F = F0
    }
}

private trait CokleisliMonad[F[_], R] extends Monad[Cokleisli[F, R, *]] with BindRec[Cokleisli[F, R, *]] {
  override def map[A, B](fa: Cokleisli[F, R, A])(f: A => B) = fa map f
  override def ap[A, B](fa: => Cokleisli[F, R, A])(f: => Cokleisli[F, R, A => B]) = f flatMap (fa map _)
  def point[A](a: => A) = Cokleisli(_ => a)
  def bind[A, B](fa: Cokleisli[F, R, A])(f: A => Cokleisli[F, R, B]) = fa flatMap f
  def tailrecM[A, B](f: A => Cokleisli[F, R, A \/ B])(a: A): Cokleisli[F, R, B] = {
    @annotation.tailrec
    def go(a0: A)(r: F[R]): B =
      f(a0).run(r) match {
        case -\/(a1) => go(a1)(r)
        case \/-(b) => b
      }

    Cokleisli(go(a))
  }
}

private trait CokleisliCompose[F[_]] extends Compose[Cokleisli[F, *, *]] {
  implicit def F: Cobind[F]

  override def compose[A, B, C](f: Cokleisli[F, B, C], g: Cokleisli[F, A, B]) = f compose g
}

private trait CokleisliProfunctor[F[_]] extends Profunctor[Cokleisli[F, *, *]] {
  implicit def F: Functor[F]

  override def dimap[A, B, C, D](fab: Cokleisli[F, A, B])(f: C => A)(g: B => D) =
    fab.dimap(f, g)

  override final def mapfst[A, B, C](fa: Cokleisli[F, A, B])(f: C => A) =
    Cokleisli[F, C, B](fc => fa(F.map(fc)(f)))

  override final def mapsnd[A, B, C](fa: Cokleisli[F, A, B])(f: B => C) =
    fa map f
}

private trait CokleisliArrow[F[_]]
  extends Arrow[Cokleisli[F, *, *]]
  with ProChoice[Cokleisli[F, *, *]]
  with CokleisliProfunctor[F]
  with CokleisliCompose[F] {

  implicit def F: Comonad[F]

  def left[A, B, C](fa: Cokleisli[F, A, B]): Cokleisli[F, A \/ C, B \/ C] =
    Cokleisli { (ac: F[A \/ C]) =>
      F.copoint(ac) match {
        case -\/(a) => -\/(fa run (F.map(ac)(_ => a)))
        case \/-(b) => \/-(b)
      }
    }

  def right[A, B, C](fa: Cokleisli[F, A, B]): Cokleisli[F, C \/ A, C \/ B] =
    Cokleisli { (ac: F[C \/ A]) =>
      F.copoint(ac) match {
        case -\/(b) => -\/(b)
        case \/-(a) => \/-(fa run (F.map(ac)(_ => a)))
      }
    }

  def arr[A, B](f: A => B) = Cokleisli(a => f(F.copoint(a)))
  def id[A] = Cokleisli[F, A, A](F.copoint)

  def first[A, B, C](f: Cokleisli[F, A, B]) =
      Cokleisli[F, (A, C), (B, C)](w => (f.run(F.map(w)(ac => ac._1)), F.copoint(w)._2))
}
