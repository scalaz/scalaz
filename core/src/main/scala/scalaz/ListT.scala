package scalaz

import std.list._

/**
 * ListT monad transformer.
 */
final case class ListT[M[_], A](run: M[List[A]]) extends TraverseTImpl[ListT, List, M, A] {
  def T = ListT.T

  def uncons(implicit M: Applicative[M]): M[Option[(A, ListT[M, A])]] = {
    M.map(run){list =>
      list match {
        case Nil => None
        case listHead :: listTail => Some(listHead, new ListT(M.point(listTail)))
      }
    }
  }

  def ::(a: A)(implicit M: Functor[M]) : ListT[M, A] = new ListT(M.map(run)(list => a :: list))

  def isEmpty(implicit M: Functor[M]) : M[Boolean] = M.map(run)(_.isEmpty)

  @deprecated("Head is deprecated. Use ListT#headOption instead", "7.1")
  def head(implicit M: Functor[M]) : M[A] = M.map(run)(_.head)

  def headOption(implicit M: Functor[M]) : OptionT[M, A] = new OptionT(M.map(run)(_.headOption))
  
  def tailM(implicit M: Applicative[M]) : M[ListT[M, A]] = M.map(uncons)(_.get._2)

  def filter(p: A => Boolean)(implicit M: Functor[M]): ListT[M, A] = new ListT(M.map(run)(_.filter(p)))
  
  def drop(n: Int)(implicit M: Functor[M]) : ListT[M, A] = new ListT(M.map(run)(_.drop(n)))

  def dropWhile(p: A => Boolean)(implicit M: Functor[M]) : ListT[M, A] = new ListT(M.map(run)(_.dropWhile(p)))
  
  def take(n: Int)(implicit M: Functor[M]) : ListT[M, A] = new ListT(M.map(run)(_.take(n)))

  def takeWhile(p: A => Boolean)(implicit M: Functor[M]) : ListT[M, A] = new ListT(M.map(run)(_.takeWhile(p)))

  def ++(bs: => ListT[M, A])(implicit M: Bind[M]) : ListT[M, A] = new ListT(M.bind(run){list1 =>
    M.map(bs.run){list2 =>
      list1 ++ list2
    }
  })

  /**Don't use iteratively! */
  def tail(implicit M: Functor[M]) : ListT[M, A] = new ListT(M.map(run)(_.tail))

  def foldLeft[B](z: => B)(f: (=> B, => A) => B)(implicit M: Functor[M]) : M[B] = M.map(run)(_.foldLeft(z){(left, right) => f(left, right)})

  def toList: M[List[A]] = run

  def foldRight[B](z: => B)(f: (=> A, => B) => B)(implicit M: Functor[M]) : M[B] = M.map(run)(_.foldRight(z){(right, left) => f(right, left)})

  def length(implicit M: Functor[M]) : M[Int] = M.map(run)(_.length)
}

object ListT extends ListTInstances with TraverseTImpl.`(*->*)->*->*`[ListT, List] {
  def T = new TraverseTWrapper[ListT, List] {
    def wrap[M[_], A](run: M[List[A]]): ListT[M, A] = ListT(run)
    def unwrap[M[_], A](t: ListT[M, A]): M[List[A]] = t.run
    def TraverseN = Traverse[List]
  }

  def empty[M[_], A](implicit M: Applicative[M]): ListT[M, A] = new ListT[M, A](M.point(Nil))

  def fromList[M[_], A](mas: M[List[A]]): ListT[M, A] = new ListT(mas)
}

//
// Prioritized Implicits for type class instances
//

sealed abstract class ListTInstances1 {
  implicit def listTMonoid[F[_], A](implicit F0: Monad[F]): Monoid[ListT[F, A]] = new ListTMonoid[F, A] {
    implicit def F: Monad[F] = F0
  }
}

sealed abstract class ListTInstances extends ListTInstances1 {
  implicit def listTEqual[F[_], A](implicit E: Equal[F[List[A]]]): Equal[ListT[F, A]] = E.contramap((_: ListT[F, A]).toList)
  implicit def listTShow[F[_], A](implicit E: Show[F[List[A]]]): Show[ListT[F, A]] = Contravariant[Show].contramap(E)((_: ListT[F, A]).toList)
}

//
// Implementation traits for type class instances
//

private trait ListTSemigroup[F[_], A] extends Semigroup[ListT[F, A]] {
 implicit def F: Bind[F]
 def append(f1: ListT[F, A], f2: => ListT[F, A]): ListT[F, A] = f1 ++ f2
}

private trait ListTMonoid[F[_], A] extends Monoid[ListT[F, A]] with ListTSemigroup[F, A] {
  implicit def F: Monad[F]

  def zero: ListT[F, A] = ListT.empty[F, A]
}
