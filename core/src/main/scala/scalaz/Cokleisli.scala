package scalaz

final case class Cokleisli[F[_], A, B](run: F[A] => B) { self =>
  def apply(fa: F[A]): B =
    run(fa)

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
  def endo(implicit ev: B === A): Endomorphic[({type λ[α, β] = Cokleisli[F, α, β]})#λ, A] =
    Endomorphic[({type λ[α, β] = Cokleisli[F, α, β]})#λ, A](ev.subst[({type λ[α] = Cokleisli[F, A, α]})#λ](this))
}

object Cokleisli extends CokleisliInstances with CokleisliFunctions

sealed abstract class CokleisliInstances0 {
  implicit def cokleisliCompose[F[_]](implicit F0: Cobind[F]): Compose[({type λ[α, β]=Cokleisli[F, α, β]})#λ] = new CokleisliCompose[F] {
    override implicit def F = F0
  }
  implicit def cokleisliProfunctor[F[_]: Functor]: Profunctor[({type λ[α, β]=Cokleisli[F, α, β]})#λ] = new CokleisliProfunctor[F] {
    def F = implicitly
  }
}

sealed abstract class CokleisliInstances extends CokleisliInstances0 {
  implicit def cokleisliMonad[F[_], R]: Monad[({type λ[α]=Cokleisli[F, R, α]})#λ] = new CokleisliMonad[F, R] {}

  implicit def cokleisliArrow[F[_]](implicit F0: Comonad[F]): Arrow[({type λ[α, β]=Cokleisli[F, α, β]})#λ] = new CokleisliArrow[F] {
    override implicit def F = F0
  }
}

trait CokleisliFunctions

private trait CokleisliMonad[F[_], R] extends Monad[({type λ[α] = Cokleisli[F, R, α]})#λ] {
  override def ap[A, B](fa: => Cokleisli[F, R, A])(f: => Cokleisli[F, R, A => B]) = f flatMap (fa map _)
  def point[A](a: => A) = Cokleisli(_ => a)
  def bind[A, B](fa: Cokleisli[F, R, A])(f: A => Cokleisli[F, R, B]) = fa flatMap f
}

private trait CokleisliCompose[F[_]] extends Compose[({type λ[α, β] = Cokleisli[F, α, β]})#λ] {
  implicit def F: Cobind[F]

  override def compose[A, B, C](f: Cokleisli[F, B, C], g: Cokleisli[F, A, B]) = f compose g
}

private trait CokleisliProfunctor[F[_]] extends Profunctor[({type λ[α, β] = Cokleisli[F, α, β]})#λ] {
  implicit def F: Functor[F]

  override final def mapfst[A, B, C](fa: Cokleisli[F, A, B])(f: C => A) =
    Cokleisli[F, C, B](fc => fa(F.map(fc)(f)))

  override final def mapsnd[A, B, C](fa: Cokleisli[F, A, B])(f: B => C) =
    fa map f
}

private trait CokleisliArrow[F[_]]
  extends Arrow[({type λ[α, β] = Cokleisli[F, α, β]})#λ]
  with CokleisliProfunctor[F]
  with CokleisliCompose[F] {

  implicit def F: Comonad[F]

  def arr[A, B](f: A => B) = Cokleisli(a => f(F.copoint(a)))
  def id[A] = Cokleisli[F, A, A](F.copoint)

  def first[A, B, C](f: Cokleisli[F, A, B]) =
      Cokleisli[F, (A, C), (B, C)](w => (f.run(F.map(w)(ac => ac._1)), F.copoint(w)._2))
}
