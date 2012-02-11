package scalaz

/**
 * ListT monad transformer.
 */
sealed class ListT[M[_], A](val step: M[ListT.Step[A, ListT[M, A]]]) {

  import ListT._

  def uncons(implicit M: Monad[M]): M[Option[(A, ListT[M, A])]] =
    M.bind(step) {
      case Yield(a, s) => M.point(Some((a, s)))
      case Skip(s) => s.uncons
      case Done => M.point(None)
    }

  def ::(a: A)(implicit M: Pointed[M]): ListT[M, A] = ListT[M, A](M point (Yield(a, this)))

  def isEmpty(implicit M: Monad[M]) = M.map(uncons)(_.isDefined)

  def head(implicit M: Monad[M]) = M.map(uncons)(_.get._1)

  def headOption(implicit M: Monad[M]) = M.map(uncons)(_.map(_._1))
  
  def tailM(implicit M: Monad[M]): M[ListT[M, A]] = M.map(uncons)(_.get._2)

  def filter(p: A => Boolean)(implicit M: Functor[M]): ListT[M, A] = stepMap {
    case Yield(a, as) => if (p(a)) Yield(a, as filter p) else Skip(as filter p)
    case Skip(as) => Skip(as filter p)
    case Done => Done
  }
  
  def drop(n: Int)(implicit M: Functor[M]): ListT[M, A] = stepMap {
    case Yield(a, as) => if (n > 0) Skip(as drop (n-1)) else Yield(a, as)
    case Skip(as) => Skip(as drop n)
    case Done => Done
  }

  def dropWhile(p: A => Boolean)(implicit M: Functor[M]): ListT[M, A] = stepMap {
    case Yield(a, as) => if (p(a)) Skip(as dropWhile p) else Yield(a, as)
    case Skip(as) => Skip(as dropWhile p)
    case Done => Done
  }
  
  def take(n: Int)(implicit M: Functor[M]): ListT[M, A] = stepMap {
    case Yield(a, as) => if (n <= 0) Done else Yield(a, as take (n-1))
    case Skip(as) => Skip(as take n)
    case Done => Done
  }

  def takeWhile(p: A => Boolean)(implicit M: Functor[M]): ListT[M, A] = stepMap {
    case Yield(a, as) => if (!p(a)) Done else Yield(a, as takeWhile p)
    case Skip(as) => Skip(as takeWhile p)
    case Done => Done
  }

  def ++[B >: A](bs: => ListT[M, B])(implicit M: Functor[M]): ListT[M, B] = stepMap {
    case Yield(a, as) => Yield(a, as ++ bs)
    case Skip(as) => Skip(as ++ bs)
    case Done => Skip(bs)
  }

  def flatMap[B](f: A => ListT[M, B])(implicit M: Functor[M]): ListT[M, B] = stepMap {
    case Yield(a, s) => Skip(f(a) ++ (s flatMap f))
    case Skip(s) => Skip(s flatMap f)
    case Done => Done
  }

  def map[B](f: A => B)(implicit M: Functor[M]): ListT[M, B] = stepMap {
    case Yield(a, s) => Yield(f(a), s map f)
    case Skip(s) => Skip(s map f)
    case Done => Done
  }

  /**Don't use iteratively! */
  def tail(implicit M: Functor[M]): ListT[M, A] = stepMap {
    case Yield(a, s) => Skip(s)
    case Skip(s) => Skip(s.tail)
    case Done => sys.error("tail: empty ListT")
  }

  def foldLeft[B](z: => B)(f: (=> B, => A) => B)(implicit M: Monad[M]): M[B] =
    M.bind(step) {
      case Yield(a, s) => s.foldLeft(f(z, a))(f)
      case Skip(s) => s.foldLeft(z)(f)
      case Done => M point z
    }

  def toList(implicit M: Monad[M]): M[List[A]] = M.map(rev(Nil))(_.reverse)

  def foldRight[B](z: => B)(f: (=> A, => B) => B)(implicit M: Monad[M]): M[B] =
    M.map(rev(Nil)) {
      _.foldLeft(z)((a, b) => f(b, a))
    }

  def length(implicit M: Monad[M]): M[Int] = {
    def addOne(c: => Int, a: => A) = 1 + c
    foldLeft(0)(addOne _)
  }

  private def stepMap[B](f: Step[A, ListT[M, A]] => Step[B, ListT[M, B]])(implicit M: Functor[M]): ListT[M, B] = ListT[M, B](M.map(step)(f))

  private def rev(xs: List[A])(implicit M: Monad[M]): M[List[A]] =
    M.bind(step) {
      case Yield(a, s) => s rev (a :: xs)
      case Skip(s) => s rev xs
      case Done => M point xs
    }
}

//
// Prioritized Implicits for type class instances
//

trait ListTInstances2 {
  implicit def listTFunctor[F[_]](implicit F0: Functor[F]): Functor[({type λ[α] = ListT[F, α]})#λ] = new ListTFunctor[F] {
    implicit def F: Functor[F] = F0
  }

  implicit def listTSemigroup[F[_], A](implicit F0: Functor[F]): Semigroup[ListT[F, A]] = new ListTSemigroup[F, A] {
    implicit def F: Functor[F] = F0
  }
}

trait ListTInstances1 extends ListTInstances2 {
  implicit def listTPointedPlus[F[_]](implicit F0: Pointed[F]): Pointed[({type λ[α] = ListT[F, α]})#λ] with Plus[({type λ[α] = ListT[F, α]})#λ] = new ListTPointed[F] {
    implicit def F: Pointed[F] = F0
  }

  implicit def listTMonoid[F[_], A](implicit F0: Pointed[F]): Monoid[ListT[F, A]] = new ListTMonoid[F, A] {
    implicit def F: Pointed[F] = F0
  }
}

trait ListTInstances0 extends ListTInstances1 {
  implicit def listTMonad[F[_]](implicit F0: Monad[F]): Monad[({type λ[α] = ListT[F, α]})#λ] = new ListTMonad[F] {
    implicit def F: Monad[F] = F0
  }
}

trait ListTInstances extends ListTInstances0 {
  // TODO Doen't satisfy PlusEmpty laws.
//  implicit def listTMonadPlus[F[_]](implicit F0: MonadPlus[F]): MonadPlus[({type λ[α] = ListT[F, α]})#λ] = new ListTMonadPlus[F] {
//    implicit def F: MonadPlus[F] = F0
//  }
  implicit def listTEqual[F[_], A](implicit E: Equal[F[List[A]]], F: Monad[F]): Equal[ListT[F, A]] = E.contramap((_: ListT[F, A]).toList)
  implicit def listTShow[F[_], A](implicit E: Show[F[List[A]]], F: Monad[F]): Show[ListT[F, A]] = Contravariant[Show].contramap(E)((_: ListT[F, A]).toList)

  implicit def listTHoist: Hoist[ListT] = new ListTHoist {}
}

object ListT extends ListTInstances {
  def apply[M[_], A](step: M[Step[A, ListT[M, A]]]): ListT[M, A] = new ListT[M, A](step)

  def empty[M[_], A](implicit M: Pointed[M]): ListT[M, A] = new ListT[M, A](M point Done)

  def fromList[M[_], A](mas: M[List[A]])(implicit M: Pointed[M]): ListT[M, A] = {
    def yieldd(a: A, rest: ListT[M, A]): Step[A, ListT[M, A]] = Yield[A, ListT[M, A]](a, rest)
    def done: Step[A, ListT[M, A]] = Done

    def fromList1(as: List[A]): Step[A, ListT[M, A]] = as match {
      case Nil => done
      case head :: tail => yieldd(head, apply(M point fromList1(tail)))
    }

    apply[M, A](M.map(mas)(fromList1))
  }

  abstract sealed class Step[+A, +S]

  case class Yield[+A, +S](a: A, s: S) extends Step[A, S]

  case class Skip[+S](s: S) extends Step[Nothing, S]

  case object Done extends Step[Nothing, Nothing]
}

//
// Implementation traits for type class instances
//

private[scalaz] trait ListTFunctor[F[_]] extends Functor[({type λ[α] = ListT[F, α]})#λ] {
  implicit def F: Functor[F]

  override def map[A, B](fa: ListT[F, A])(f: A => B): ListT[F, B] = fa map f
}

private[scalaz] trait ListTSemigroup[F[_], A] extends Semigroup[ListT[F, A]] {
  implicit def F: Functor[F]

  def append(f1: ListT[F, A], f2: => ListT[F, A]): ListT[F, A] = f1 ++ f2
}

private[scalaz] trait ListTMonoid[F[_], A] extends Monoid[ListT[F, A]] with ListTSemigroup[F, A] {
  implicit def F: Pointed[F]

  def zero: ListT[F, A] = ListT.empty[F, A]
}

private[scalaz] trait ListTPointed[F[_]] extends Pointed[({type λ[α] = ListT[F, α]})#λ] with Plus[({type λ[α] = ListT[F, α]})#λ] with ListTFunctor[F] {
  implicit def F: Pointed[F]

  def point[A](a: => A): ListT[F, A] = a :: ListT.empty[F, A]

  def empty[A]: ListT[F, A] = ListT.empty

  def plus[A](a: ListT[F, A], b: => ListT[F, A]): ListT[F, A] = a ++ b
}

private[scalaz] trait ListTMonad[F[_]] extends Monad[({type λ[α] = ListT[F, α]})#λ] with ListTPointed[F] {
  implicit def F: Monad[F]

  def bind[A, B](fa: ListT[F, A])(f: A => ListT[F, B]): ListT[F, B] = fa flatMap f
}

private[scalaz] trait ListTMonadPlus[F[_]] extends MonadPlus[({type λ[α] = ListT[F, α]})#λ] with ListTMonad[F] {
  implicit def F: MonadPlus[F]
}

private[scalaz] trait ListTHoist extends Hoist[ListT] {
  import ListT._
  
  implicit def apply[G[_] : Monad]: Monad[({type λ[α] = ListT[G, α]})#λ] = listTMonad[G]
  
  def liftM[G[_], A](a: G[A])(implicit G: Monad[G]): ListT[G, A] = ListT[G, A](G.map(a)(Yield(_, empty)))
  
  def hoist[M[_], N[_]](f: M ~> N)(implicit M: Monad[M]): ({type f[x] = ListT[M, x]})#f ~> ({type f[x] = ListT[N, x]})#f =
    new (({type f[x] = ListT[M, x]})#f ~> ({type f[x] = ListT[N, x]})#f) {
      def apply[A](a: ListT[M, A]): ListT[N, A] = ListT[N, A](f(M.map(a.step)(_ match {
        case Yield(a, as) => Yield(a, hoist(f) apply as)
        case Skip(as) => Skip(hoist(f) apply as)
        case Done => Done
      })))
    }
}
