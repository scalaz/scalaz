package scalaz

import Maybe.just

/**
 * CommutativeListT monad transformer, which is lawful only when `M` is commutative.
 */

final case class CommutativeListT[M[_], A](run: M[IList[A]]){
  def uncons(implicit M: Applicative[M]): M[Maybe[(A, CommutativeListT[M, A])]] = {
    M.map(run) {
      case INil() => Maybe.empty
      case ICons(listHead, listTail) => just((listHead, new CommutativeListT(M.point(listTail))))
    }
  }

  def ::(a: A)(implicit M: Functor[M]) : CommutativeListT[M, A] = new CommutativeListT(M.map(run)(list => a :: list))

  def collect[B](pf: PartialFunction[A,B])(implicit M: Functor[M]): CommutativeListT[M, B] = new CommutativeListT(M.map(run)(_.collect(pf)))

  def isEmpty(implicit M: Functor[M]) : M[Boolean] = M.map(run)(_.isEmpty)

  def headOption(implicit M: Functor[M]) : OptionT[M, A] = new OptionT(M.map(run)(_.headOption))

  def find(predicate: A => Boolean)(implicit M: Functor[M]) : MaybeT[M, A] = new MaybeT(M.map(run)(_.find(predicate)))

  def headMaybe(implicit M: Functor[M]) : MaybeT[M, A] = new MaybeT(M.map(run)(_.headMaybe))

  def tailM(implicit M: Applicative[M]) : M[CommutativeListT[M, A]] = M.map(uncons)(_.map(_._2).toOption.get)

  def filter(p: A => Boolean)(implicit M: Functor[M]): CommutativeListT[M, A] = new CommutativeListT(M.map(run)(_.filter(p)))

  def drop(n: Int)(implicit M: Functor[M]) : CommutativeListT[M, A] = new CommutativeListT(M.map(run)(_.drop(n)))

  def dropWhile(p: A => Boolean)(implicit M: Functor[M]) : CommutativeListT[M, A] = new CommutativeListT(M.map(run)(_.dropWhile(p)))

  def take(n: Int)(implicit M: Functor[M]) : CommutativeListT[M, A] = new CommutativeListT(M.map(run)(_.take(n)))

  def takeWhile(p: A => Boolean)(implicit M: Functor[M]) : CommutativeListT[M, A] = new CommutativeListT(M.map(run)(_.takeWhile(p)))

  def ++(bs: => CommutativeListT[M, A])(implicit M: Bind[M]) : CommutativeListT[M, A] = new CommutativeListT(M.bind(run){list1 =>
    M.map(bs.run){list2 =>
      list1 ++ list2
    }
  })

  def flatMap[B](f: A => CommutativeListT[M, B])(implicit M: Monad[M]) : CommutativeListT[M, B] =
    new CommutativeListT(M.bind(run)(Foldable[IList].foldMap(_)(f).run))

  def flatMapF[B](f: A => M[IList[B]])(implicit M: Monad[M]) : CommutativeListT[M, B] = flatMap(f andThen CommutativeListT.apply)

  def map[B](f: A => B)(implicit M: Functor[M]): CommutativeListT[M, B] = new CommutativeListT(
    M.map(run)(_.map(f))
  )

  def mapF[B](f: A => M[B])(implicit M: Monad[M]): CommutativeListT[M, B] = {
    flatMapF {
      f andThen (mb => M.map(mb)(b => IList(b)))
    }
  }

  def mapT[F[_], B](f: M[IList[A]] => F[IList[B]]): CommutativeListT[F, B] =
    CommutativeListT(f(run))

  /**Don't use iteratively! */
  def tail(implicit M: Functor[M]) : CommutativeListT[M, A] = new CommutativeListT(M.map(run)(_.tailMaybe.toOption.get))

  def tailMaybe(implicit M: Functor[M]) : CommutativeListT[λ[a => M[Maybe[a]]], A] = new CommutativeListT[λ[a => M[Maybe[a]]], A](M.map(run)(_.tailMaybe))

  def foldLeft[B](z: => B)(f: (=> B, => A) => B)(implicit M: Functor[M]) : M[B] = M.map(run)(_.foldLeft(z){(left, right) => f(left, right)})

  def toIList : M[IList[A]] = run

  def toList(implicit M: Functor[M]): M[List[A]] = M.map(run)(_.toList)

  def foldRight[B](z: => B)(f: (=> A, => B) => B)(implicit M: Functor[M]) : M[B] = M.map(run)(_.foldRight(z){(right, left) => f(right, left)})

  def length(implicit M: Functor[M]) : M[Int] = M.map(run)(_.length)
}

//
// Prioritized Implicits for type class instances
//

sealed abstract class CommutativeListTInstances2 {
  implicit def commutativeListTFunctor[F[_]](implicit F0: Functor[F]): Functor[CommutativeListT[F, *]] =
    new CommutativeListTFunctor[F]{
      implicit def F: Functor[F] = F0
    }

  implicit def commutativeListTSemigroup[F[_], A](implicit F0: Bind[F]): Semigroup[CommutativeListT[F, A]] =
    new CommutativeListTSemigroup[F, A]{
      implicit def F: Bind[F] = F0
    }
}

sealed abstract class CommutativeListTInstances1 extends CommutativeListTInstances2 {
  implicit def commutativeListTMonoid[F[_], A](implicit F0: Monad[F]): Monoid[CommutativeListT[F, A]] =
    new CommutativeListTMonoid[F, A] {
      implicit def F: Monad[F] = F0
    }
}

sealed abstract class CommutativeListTInstances extends CommutativeListTInstances1 {
  implicit def commutativeListTMonadPlus[F[_]](implicit F0: Monad[F]): MonadPlus[CommutativeListT[F, *]] with Alt[CommutativeListT[F, *]] =
    new CommutativeListTMonadPlus[F] with Alt[CommutativeListT[F, *]] {
      implicit def F: Monad[F] = F0

      def alt[A](a: => CommutativeListT[F, A], b: => CommutativeListT[F, A]): CommutativeListT[F, A] =
        plus(a, b)
    }

  implicit def commutativeListTEqual[F[_], A](implicit E: Equal[F[IList[A]]]): Equal[CommutativeListT[F, A]] =
    E.contramap((_: CommutativeListT[F, A]).toIList)

  implicit def commutativeListTShow[F[_], A](implicit E: Show[F[IList[A]]]): Show[CommutativeListT[F, A]] =
    Contravariant[Show].contramap(E)((_: CommutativeListT[F, A]).toIList)

  implicit val commutativeListTHoist: Hoist[CommutativeListT] =
    new CommutativeListTHoist {}

  implicit def commutativeListTDecidable[F[_]](implicit F0: Divisible[F]): Decidable[CommutativeListT[F, *]] =
    new CommutativeListTDecidable[F] {
      implicit def F: Divisible[F] = F0
    }
}

object CommutativeListT extends CommutativeListTInstances {
  def commutativeListT[M[_]]: (λ[α => M[IList[α]]] ~> CommutativeListT[M, *]) =
    new (λ[α => M[IList[α]]] ~> CommutativeListT[M, *]) {
      def apply[A](a: M[IList[A]]) = new CommutativeListT[M, A](a)
    }

  def empty[M[_], A](implicit M: Applicative[M]): CommutativeListT[M, A] =
    new CommutativeListT[M, A](M.point(INil()))

  def fromIList[M[_], A](mas: M[IList[A]]): CommutativeListT[M, A] =
    new CommutativeListT(mas)

  def fromList[M[_], A](mas: M[List[A]])(implicit M: Functor[M]): CommutativeListT[M, A] =
    new CommutativeListT(M.map(mas)(IList.fromList))
}

//
// Implementation traits for type class instances
//

private trait CommutativeListTDecidable[F[_]] extends Decidable[CommutativeListT[F, *]] {
  implicit def F: Divisible[F]
  override def conquer[A]: CommutativeListT[F, A] = CommutativeListT(F.conquer)

  override def divide2[A1, A2, Z](a1: => CommutativeListT[F, A1], a2: => CommutativeListT[F, A2])(f: Z => (A1, A2)): CommutativeListT[F, Z] =
    CommutativeListT(F.divide2(a1.run, a2.run)((z: IList[Z]) => Unzip[IList].unzip(z.map(f))))

  override def choose2[Z, A1, A2](a1: => CommutativeListT[F, A1], a2: => CommutativeListT[F, A2])(f: Z => A1 \/ A2): CommutativeListT[F, Z] =
    CommutativeListT(
      F.divide2(a1.run, a2.run) (
        (z: IList[Z]) => z.map(f)
          ./:((IList.empty[A1], IList.empty[A2])) {
            case (x, y) => y.fold(a1 => (x._1 :+ a1, x._2), a2 => (x._1, x._2 :+ a2))
          }
      )
    )
}

private trait CommutativeListTFunctor[F[_]] extends Functor[CommutativeListT[F, *]] {
 implicit def F: Functor[F]
 override def map[A, B](fa: CommutativeListT[F, A])(f: A => B): CommutativeListT[F, B] = fa map f
}

private trait CommutativeListTSemigroup[F[_], A] extends Semigroup[CommutativeListT[F, A]] {
 implicit def F: Bind[F]
 def append(f1: CommutativeListT[F, A], f2: => CommutativeListT[F, A]): CommutativeListT[F, A] = f1 ++ f2
}

private trait CommutativeListTMonoid[F[_], A] extends Monoid[CommutativeListT[F, A]] with CommutativeListTSemigroup[F, A] {
  implicit def F: Monad[F]

  def zero: CommutativeListT[F, A] = CommutativeListT.empty[F, A]
}

private trait CommutativeListTMonadPlus[F[_]] extends MonadPlus[CommutativeListT[F, *]] with CommutativeListTFunctor[F] {
  implicit def F: Monad[F]

  def bind[A, B](fa: CommutativeListT[F, A])(f: A => CommutativeListT[F, B]): CommutativeListT[F, B] = fa flatMap f

  def point[A](a: => A): CommutativeListT[F, A] = a :: CommutativeListT.empty[F, A]

  def empty[A]: CommutativeListT[F, A] = CommutativeListT.empty[F, A]

  def plus[A](a: CommutativeListT[F, A], b: => CommutativeListT[F, A]): CommutativeListT[F, A] = a ++ b
}

private trait CommutativeListTHoist extends Hoist[CommutativeListT] {
  import CommutativeListT._

  implicit def apply[G[_] : Monad]: Monad[CommutativeListT[G, *]] =
    commutativeListTMonadPlus[G]

  def liftM[G[_], A](a: G[A])(implicit G: Monad[G]): CommutativeListT[G, A] =
    fromIList(G.map(a)(entry => entry :: INil()))

  def hoist[M[_], N[_]](f: M ~> N)(implicit M: Monad[M]): CommutativeListT[M, *] ~> CommutativeListT[N, *] =
    new (CommutativeListT[M, *] ~> CommutativeListT[N, *]) {
      def apply[A](a: CommutativeListT[M, A]): CommutativeListT[N, A] =
        a.mapT(f.apply)
    }
}
