package scalaz

import Id._

/** Functors that annihilate each other. */
trait Zap[F[_], G[_]] { self =>
  def zapWith[A, B, C](fa: F[A], gb: G[B])(f: (A, B) => C): C
  def zap[A, B](f: F[A => B], g: G[A]): B = zapWith(f, g)(_(_))
  def flip: Zap[G, F] = new Zap[G, F] {
    def zapWith[A, B, C](ga: G[A], fb: F[B])(f: (A, B) => C): C =
      self.zapWith(fb, ga)((b, a) => f(a, b))
    override def flip = self
  }
}

trait ZapInstances {

  /** The identity functor annihilates itself. */
  implicit val identityZap: Zap[Id, Id] = new Zap[Id, Id] {
    def zapWith[A, B, C](a: A, b: B)(f: (A, B) => C): C = f(a, b)
  }

  /** The product of two functors annihilates their coproduct. */
  implicit def productCoproductZap[F[_], FF[_], G[_], GG[_]](implicit d1: Zap[F, FF], d2: Zap[G, GG]):
    Zap[({ type λ[α] = (F[α] \/ G[α]) })#λ, ({ type λ[α] = (FF[α], GG[α]) })#λ] =
      new Zap[({ type λ[α] = (F[α] \/ G[α]) })#λ, ({ type λ[α] = (FF[α], GG[α]) })#λ] {
        def zapWith[A, B, C](a: (F[A] \/ G[A]), b: (FF[B], GG[B]))(f: (A, B) => C) =
          a match {
            case -\/(fa) => d1.zapWith(fa, b._1)(f)
            case \/-(ga) => d2.zapWith(ga, b._2)(f)
          }
      }

  /** The coproduct of two functors annihilates their product. */
  implicit def coproductProductZap[F[_], FF[_], G[_], GG[_]](implicit d1: Zap[FF, F], d2: Zap[GG, G]):
    Zap[({ type λ[α] = (FF[α], GG[α]) })#λ, ({ type λ[α] = (F[α] \/ G[α]) })#λ] =
      new Zap[({ type λ[α] = (FF[α], GG[α]) })#λ, ({ type λ[α] = (F[α] \/ G[α]) })#λ] {
        def zapWith[A, B, C](a: (FF[A], GG[A]), b: (F[B] \/ G[B]))(f: (A, B) => C) =
          b match {
            case -\/(fb) => d1.zapWith(a._1, fb)(f)
            case \/-(gb) => d2.zapWith(a._2, gb)(f)
          }
      }

  /** A free monad and a cofree comonad annihilate each other */
  implicit def monadComonadZap[F[_], G[_]](implicit d: Zap[F, G], F: Functor[F], G: Functor[G]):
    Zap[({type λ[α] = Free[F, α]})#λ, ({type λ[α] = Cofree[G, α]})#λ] =
      new Zap[({type λ[α] = Free[F, α]})#λ, ({type λ[α] = Cofree[G, α]})#λ] {
        def zapWith[A, B, C](ma: Free[F, A], wb: Cofree[G, B])(f: (A, B) => C): C =
          ma.resume match {
            case \/-(a) => f(a, wb.head)
            case -\/(k) => d.zapWith(k, wb.tail)(zapWith(_, _)(f))
          }
      }

  /** A cofree comonad and a free monad annihilate each other */
  implicit def comonadMonadZap[F[_], G[_]](implicit d: Zap[F, G], F: Functor[F], G: Functor[G]):
    Zap[({type λ[α] = Cofree[F, α]})#λ, ({type λ[α] = Free[G, α]})#λ] =
      new Zap[({type λ[α] = Cofree[F, α]})#λ, ({type λ[α] = Free[G, α]})#λ] {
        def zapWith[A, B, C](wa: Cofree[F, A], mb: Free[G, B])(f: (A, B) => C): C =
          mb.resume match {
            case \/-(b) => f(wa.head, b)
            case -\/(k) => d.zapWith(wa.tail, k)(zapWith(_, _)(f))
          }
      }
}

object Zap extends ZapInstances 
