package scalaz

/**
 * ListT monad transformer.
 */

final case class ListT[M[_], A](underlying: M[List[A]]){
  def uncons(implicit M: Applicative[M]): M[Option[(A, ListT[M, A])]] = {
    M.map(underlying){list =>
      list match {
        case Nil => None
        case listHead :: listTail => Some(listHead, new ListT(M.point(listTail)))
      }
    }
  }

  def ::(a: A)(implicit M: Functor[M]) : ListT[M, A] = new ListT(M.map(underlying)(list => a :: list))

  def isEmpty(implicit M: Functor[M]) : M[Boolean] = M.map(underlying)(_.isEmpty)

  @deprecated("Head is deprecated. Use ListT#headOption instead", "7.1")
  def head(implicit M: Functor[M]) : M[A] = M.map(underlying)(_.head)

  def headOption(implicit M: Functor[M]) : OptionT[M, A] = new OptionT(M.map(underlying)(_.headOption))
  
  def tailM(implicit M: Applicative[M]) : M[ListT[M, A]] = M.map(uncons)(_.get._2)

  def filter(p: A => Boolean)(implicit M: Functor[M]): ListT[M, A] = new ListT(M.map(underlying)(_.filter(p)))
  
  def drop(n: Int)(implicit M: Functor[M]) : ListT[M, A] = new ListT(M.map(underlying)(_.drop(n)))

  def dropWhile(p: A => Boolean)(implicit M: Functor[M]) : ListT[M, A] = new ListT(M.map(underlying)(_.dropWhile(p)))
  
  def take(n: Int)(implicit M: Functor[M]) : ListT[M, A] = new ListT(M.map(underlying)(_.take(n)))

  def takeWhile(p: A => Boolean)(implicit M: Functor[M]) : ListT[M, A] = new ListT(M.map(underlying)(_.takeWhile(p)))

  def ++(bs: => ListT[M, A])(implicit M: Bind[M]) : ListT[M, A] = new ListT(M.bind(underlying){list1 =>
    M.map(bs.underlying){list2 =>
      list1 ++ list2
    }
  })

  def flatMap[B](f: A => ListT[M, B])(implicit M: Monad[M]) : ListT[M, B] = new ListT(M.bind(underlying){list =>
    list match {
      case Nil => M.point(Nil)
      case nonEmpty => nonEmpty.map(f).reduce(_ ++ _).underlying
    }
  })

  def map[B](f: A => B)(implicit M: Functor[M]) : ListT[M, B] = new ListT(M.map(underlying)(_.map(f)))

  /**Don't use iteratively! */
  def tail(implicit M: Functor[M]) : ListT[M, A] = new ListT(M.map(underlying)(_.tail))

  def foldLeft[B](z: => B)(f: (=> B, => A) => B)(implicit M: Functor[M]) : M[B] = M.map(underlying)(_.foldLeft(z){(left, right) => f(left, right)})

  def toList : M[List[A]] = underlying

  def foldRight[B](z: => B)(f: (=> A, => B) => B)(implicit M: Functor[M]) : M[B] = M.map(underlying)(_.foldRight(z){(right, left) => f(right, left)})

  def length(implicit M: Functor[M]) : M[Int] = M.map(underlying)(_.length)
}

//
// Prioritized Implicits for type class instances
//

trait ListTInstances2 {
  implicit def listTFunctor[F[_]](implicit F0: Functor[F]): Functor[({type λ[α] = ListT[F, α]})#λ] = new ListTFunctor[F]{
    implicit def F: Functor[F] = F0
  }

  implicit def listTSemigroup[F[_], A](implicit F0: Bind[F]): Semigroup[ListT[F, A]] = new ListTSemigroup[F, A]{
    implicit def F: Bind[F] = F0
  }
}

trait ListTInstances1 extends ListTInstances2 {

  implicit def listTMonoid[F[_], A](implicit F0: Monad[F]): Monoid[ListT[F, A]] = new ListTMonoid[F, A] {
    implicit def F: Monad[F] = F0
  }
}

trait ListTInstances extends ListTInstances1 {
  implicit def listTMonadPlus[F[_]](implicit F0: Monad[F]): MonadPlus[({type λ[α] = ListT[F, α]})#λ] = new ListTMonadPlus[F] {
    implicit def F: Monad[F] = F0
  }
  implicit def listTEqual[F[_], A](implicit E: Equal[F[List[A]]]): Equal[ListT[F, A]] = E.contramap((_: ListT[F, A]).toList)
  implicit def listTShow[F[_], A](implicit E: Show[F[List[A]]]): Show[ListT[F, A]] = Contravariant[Show].contramap(E)((_: ListT[F, A]).toList)

  implicit def listTHoist: Hoist[ListT] = new ListTHoist {}
}

object ListT extends ListTInstances {
  def empty[M[_], A](implicit M: Applicative[M]): ListT[M, A] = new ListT[M, A](M.point(Nil))

  def fromList[M[_], A](mas: M[List[A]]): ListT[M, A] = new ListT(mas)
}

//
// Implementation traits for type class instances
//

private[scalaz] trait ListTFunctor[F[_]] extends Functor[({type λ[α] = ListT[F, α]})#λ] {
 implicit def F: Functor[F]
 override def map[A, B](fa: ListT[F, A])(f: A => B): ListT[F, B] = fa map f
}

private[scalaz] trait ListTSemigroup[F[_], A] extends Semigroup[ListT[F, A]] {
 implicit def F: Bind[F]
 def append(f1: ListT[F, A], f2: => ListT[F, A]): ListT[F, A] = f1 ++ f2
}

private[scalaz] trait ListTMonoid[F[_], A] extends Monoid[ListT[F, A]] with ListTSemigroup[F, A] {
  implicit def F: Monad[F]

  def zero: ListT[F, A] = ListT.empty[F, A]
}

private[scalaz] trait ListTMonadPlus[F[_]] extends MonadPlus[({type λ[α] = ListT[F, α]})#λ] with ListTFunctor[F] {
  implicit def F: Monad[F]

  def bind[A, B](fa: ListT[F, A])(f: A => ListT[F, B]): ListT[F, B] = fa flatMap f

  def point[A](a: => A): ListT[F, A] = a :: ListT.empty[F, A]

  def empty[A]: ListT[F, A] = ListT.empty[F, A]

  def plus[A](a: ListT[F, A], b: => ListT[F, A]): ListT[F, A] = a ++ b
}

private[scalaz] trait ListTHoist extends Hoist[ListT] {
  import ListT._
  
  implicit def apply[G[_] : Monad]: Monad[({type λ[α] = ListT[G, α]})#λ] = listTMonadPlus[G]
  
  def liftM[G[_], A](a: G[A])(implicit G: Monad[G]): ListT[G, A] = fromList(G.map(a)(entry => entry :: Nil))
  
  def hoist[M[_], N[_]](f: M ~> N)(implicit M: Monad[M]): ({type f[x] = ListT[M, x]})#f ~> ({type f[x] = ListT[N, x]})#f =
    new (({type f[x] = ListT[M, x]})#f ~> ({type f[x] = ListT[N, x]})#f) {
      def apply[A](a: ListT[M, A]): ListT[N, A] = fromList(f(a.underlying))
    }
}
