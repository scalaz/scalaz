package scalaz
package data

import scala.inline

import tc._

sealed trait KleisliModule {
  type Kleisli[F[_], A, B]

  def runKleisli[F[_], A, B](k: Kleisli[F, A, B]): A => F[B]

  def wrapKleisli[F[_], A, B](k: A => F[B]): Kleisli[F, A, B]

  def hoist[F[_], G[_], A, B](k: Kleisli[F, A, B])(η: F ~> G): Kleisli[G, A, B]

  def first[F[_], A, B, C](k: Kleisli[F, A, B])(implicit F: Functor[F]): Kleisli[F, (A, C), (B, C)]

  def second[F[_], A, B, C](k: Kleisli[F, A, B])(implicit F: Functor[F]): Kleisli[F, (C, A), (C, B)]

  def dimap[F[_], A, B, C, D](k: Kleisli[F, A, B])(ca: C => A)(bd: B => D)(implicit F: Functor[F]): Kleisli[F, C, D]

  def compose[F[_], A, B, C](j: Kleisli[F, B, C], k: Kleisli[F, A, B])(implicit B: Bind[F]): Kleisli[F, A, C]

  def liftA[F[_]: Applicative, A, B](fb: A => B): Kleisli[F, A, B]

  def const[F[_], A, B](fb: F[B]): Kleisli[F, A, B]

}

object KleisliModule {
  import Kleisli.{ runKleisli, wrapKleisli }

  implicit def kleisliMonad[M[_], A0](implicit M: Monad[M]): Monad[Kleisli[M, A0, ?]] =
    instanceOf(
      new MonadClass[Kleisli[M, A0, ?]] {
        override def pure[A](a: A): Kleisli[M, A0, A] =
          wrapKleisli(_ => M.pure(a))
        override def flatMap[A, B](ma: Kleisli[M, A0, A])(f: A => Kleisli[M, A0, B]): Kleisli[M, A0, B] =
          wrapKleisli(a0 => M.flatMap(runKleisli(ma)(a0))(a => runKleisli(f(a))(a0)))
      }
    )

  implicit def kleisliSemicategory[M[_]](implicit B: Bind[M]): Semicategory[Kleisli[M, ?, ?]] =
    instanceOf(new SemicategoryClass[Kleisli[M, ?, ?]] {
      def compose[A, B, C](f: Kleisli[M, B, C], g: Kleisli[M, A, B]): Kleisli[M, A, C] =
        wrapKleisli(a => B.flatMap(runKleisli(g)(a))(runKleisli(f)))
    })

  implicit def kleisliMonoid[M[_], A, B](implicit M: Monoid[M[B]]): Monoid[Kleisli[M, A, B]] =
    instanceOf(new MonoidClass[Kleisli[M, A, B]] {
      override def mempty: Kleisli[M, A, B] =
        wrapKleisli(_ => M.mempty)

      override def mappend(a1: Kleisli[M, A, B], a2: Kleisli[M, A, B]): Kleisli[M, A, B] =
        wrapKleisli(a => M.mappend(runKleisli(a1)(a), runKleisli(a2)(a)))
    })

  implicit def kleisliStrong[M[_], A, B](implicit M: Functor[M]): Strong[Kleisli[M, ?, ?]] =
    instanceOf(
      new StrongClass[Kleisli[M, ?, ?]] {
        override def first[A, B, C](pab: Kleisli[M, A, B]): Kleisli[M, (A, C), (B, C)] =
          Kleisli.first(pab)

        override def dimap[A, B, C, D](fab: Kleisli[M, A, B])(ca: C => A)(bd: B => D): Kleisli[M, C, D] =
          wrapKleisli(c => M.map(runKleisli(fab)(ca(c)))(bd))
      }
    )
}

private[data] object KleisliImpl extends KleisliModule {
  type Kleisli[F[_], A, B] = A => F[B]

  override def runKleisli[F[_], A, B](k: Kleisli[F, A, B]): A => F[B] = k

  override def wrapKleisli[F[_], A, B](k: A => F[B]): Kleisli[F, A, B] = k

  override def hoist[F[_], G[_], A, B](k: Kleisli[F, A, B])(η: F ~> G): Kleisli[G, A, B] =
    k andThen η.apply

  override def first[F[_], A, B, C](
    k: Kleisli[F, A, B]
  )(implicit F: Functor[F]): Kleisli[F, (A, C), (B, C)] =
    t => F.map(k(t._1))((_, t._2))

  override def second[F[_], A, B, C](
    k: Kleisli[F, A, B]
  )(implicit F: Functor[F]): Kleisli[F, (C, A), (C, B)] =
    t => F.map(k(t._2))((t._1, _))

  override def dimap[F[_], A, B, C, D](
    k: Kleisli[F, A, B]
  )(
    ca: C => A
  )(
    bd: B => D
  )(implicit F: Functor[F]): Kleisli[F, C, D] =
    c => F.map(k(ca(c)))(bd)

  override def compose[F[_], A, B, C](
    j: Kleisli[F, B, C],
    k: Kleisli[F, A, B]
  )(implicit B: Bind[F]): Kleisli[F, A, C] =
    a => B.flatMap(k(a))(j)

  override def liftA[F[_], A, B](f: A => B)(implicit F: Applicative[F]): Kleisli[F, A, B] =
    a => F.pure(f(a))

  override def const[F[_], A, B](fb: F[B]): Kleisli[F, A, B] =
    _ => fb
}

trait KleisliFunctions {
  @inline def wrapKleisli[F[_], A, B](k: A => F[B]): Kleisli[F, A, B] = Kleisli.wrapKleisli(k)
  @inline def runKleisli[F[_], A, B](k: Kleisli[F, A, B]): A => F[B]  = Kleisli.runKleisli(k)
}

trait KleisliSyntax {
  import Kleisli.{ runKleisli, wrapKleisli }

  implicit final class ToKleisliOps[F[_], A, B](k: Kleisli[F, A, B]) {

    def hoist[G[_]](η: F ~> G): Kleisli[G, A, B] =
      Kleisli.hoist(k)(η)

    def andThen[C](j: Kleisli[F, B, C])(implicit B: Bind[F]): Kleisli[F, A, C] =
      Kleisli.compose(j, k)

    def compose[E](j: Kleisli[F, E, A])(implicit B: Bind[F]): Kleisli[F, E, B] =
      Kleisli.compose(k, j)

    def first[C](implicit F: Functor[F]): Kleisli[F, (A, C), (B, C)] =
      Kleisli.first(k)

    def second[C](implicit F: Functor[F]): Kleisli[F, (C, A), (C, B)] =
      Kleisli.second(k)

    def >=>[C](j: Kleisli[F, B, C])(implicit B: Bind[F]): Kleisli[F, A, C] =
      Kleisli.compose(j, k)

    def <=<[E](j: Kleisli[F, E, A])(implicit B: Bind[F]): Kleisli[F, E, B] =
      Kleisli.compose(k, j)

    def >>>[C](j: Kleisli[F, B, C])(implicit B: Bind[F]): Kleisli[F, A, C] =
      Kleisli.compose(j, k)

    def =<<(fa: F[A])(implicit B: Bind[F]): F[B] =
      B.flatMap(fa)(runKleisli(k))

    def ***[C, D](j: Kleisli[F, C, D])(
      implicit A: Apply[F]
    ): Kleisli[F, (A, C), (B, D)] =
      wrapKleisli(t => A.ap(runKleisli(j)(t._2))(A.map(runKleisli(k)(t._1))(a => (a, _))))

    def &&&[C](j: Kleisli[F, A, C])(
      implicit A: Apply[F]
    ): Kleisli[F, A, (B, C)] =
      wrapKleisli(a => A.ap(runKleisli(j)(a))(A.map(runKleisli(k)(a))(a => (a, _))))
  }
}
