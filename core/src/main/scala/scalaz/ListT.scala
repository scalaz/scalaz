package scalaz

sealed class ListT[M[_],A](val step : M[ListT.Step[A, ListT[M,A]]]) {
  import ListT._
  import Scalaz._

  def uncons(implicit M:Monad[M]): M[Option[(A, ListT[M,A])]] = 
    step flatMap {
      case Yield(a,s) => M.pure(some((a,s)))
      case Skip(s) => s.uncons
      case Done => M.pure(none)
    }
  def ::(a: A)(implicit M:Pure[M]): ListT[M,A] = ListT[M,A](M pure (Yield(a, this)))

  def isEmpty(implicit M:Monad[M]) = uncons map { _.isDefined } 
  def head(implicit M:Monad[M]) = uncons map { _.get._1 }
  def tailM(implicit M:Monad[M]): M[ListT[M,A]] = uncons map { _.get._2 } 

  def filter(p: A => Boolean)(implicit M:Functor[M]): ListT[M,A] = ListT[M,A](
    step map { 
      case Yield(a,as) => if (p(a)) Yield(a, as filter p) else Skip(as filter p)
      case Skip(as) => Skip(as filter p)
      case Done => Done
    }
  )
  def dropWhile(p: A => Boolean)(implicit M: Functor[M]): ListT[M,A] = ListT[M,A](
    step map { 
      case Yield(a,as) => if (p(a)) Skip(as dropWhile p) else Yield(a,as)
      case Skip(as) => Skip(as dropWhile p)
      case Done => Done
    }
  )
  def takeWhile(p: A => Boolean)(implicit M: Functor[M]): ListT[M,A] = ListT[M,A](
    step map { 
      case Yield(a,as) => if (!p(a)) Skip(as takeWhile p) else Yield(a,as)
      case Skip(as) => Skip(as takeWhile p)
      case Done => Done
    }
  )

  def ++[B>:A](bs: => ListT[M,B])(implicit M:Functor[M]) : ListT[M,B] = ListT[M,B](
    step map { 
      case Yield(a,as) => Yield(a, as ++ bs)
      case Skip(as) => Skip(as ++ bs)
      case Done => Skip(bs)
    }
  )
  def flatMap[B](f: A => ListT[M,B])(implicit M:Functor[M]): ListT[M,B] = ListT[M,B](
    step map {
      case Yield(a,s) => Skip(f(a) ++ (s flatMap f))
      case Skip(s) => Skip(s flatMap f)
      case Done => Done
    }
  )
  def map[B](f: A => B)(implicit M:Functor[M]): ListT[M,B] = ListT[M,B](
    step map { 
      case Yield(a,s) => Yield(f(a), s map f)
      case Skip(s) => Skip(s map f)
      case Done => Done
    }
  )
  /** Don't use iteratively! */
  def tail(implicit M:Functor[M]): ListT[M,A] = ListT[M,A](
    step map { 
      case Yield(a,s) => Skip(s)
      case Skip(s) => Skip(s.tail)
      case Done => error("tail: empty ListT")
    }
  )
  def foldLeft[B](z: => B)(f: (=> B, => A) => B)(implicit M: Monad[M]): M[B] = 
    step flatMap {
      case Yield(a,s) => s.foldLeft(f(z,a))(f)
      case Skip(s) => s.foldLeft(z)(f)
      case Done => M pure z
    } 

  def toList(implicit M:Monad[M]): M[List[A]] = rev(Nil) map (_.reverse)

  private def rev(xs : List[A])(implicit M:Monad[M]): M[List[A]] = 
    step flatMap { 
      case Yield(a,s) => s rev (a :: xs)
      case Skip(s) => s rev xs
      case Done => M pure xs
    }
  
  def foldRight[B](z: => B)(f: (=> A, => B) => B)(implicit M: Monad[M]): M[B] =
    rev(Nil) map { _.foldLeft(z)((a, b) => f(b, a)) } 

  def length(implicit M: Monad[M]) : M[Int] = {
    def addOne(c: => Int, a: => A) = 1 + c
    foldLeft(0)(addOne _)
  }
}
object ListT { 
  def apply[M[_],A](step: M[Step[A, ListT[M,A]]]): ListT[M,A] = new ListT[M,A](step)
  def empty[M[_],A](implicit M:Pure[M]): ListT[M,A] = new ListT[M,A](M pure Done)

  abstract sealed class Step[+A,+S]
  case class Yield[+A, +S](a: A, s: S) extends Step[A,S]
  case class Skip[+S](s: S) extends Step[Nothing,S]
  case object Done extends Step[Nothing,Nothing]

  implicit def listTEmpty[M[_]:Pure]
    : Empty[({type λ[X] = ListT[M,X]})#λ] =
  new Empty[({type λ[X] = ListT[M,X]})#λ] {
    def empty[A] = ListT.empty[M,A]
  }
  implicit def listTFunctor[M[_]:Functor]
    : Functor[({type λ[X] = ListT[M,X]})#λ] =
  new Functor[({type λ[X] = ListT[M,X]})#λ] {
    def fmap[A,B](r: ListT[M, A], f: A => B) = r map f
  }
  implicit def listTPure[M[_]:Pure]
    : Pure[({type λ[X] = ListT[M,X]})#λ] =
  new Pure[({type λ[X] = ListT[M,X]})#λ] {
    def pure[A](a: => A) = a :: empty[M,A]
  }

  implicit def listTBind[M[_]:Functor]
    : Bind[({type λ[X] = ListT[M,X]})#λ] =
  new Bind[({type λ[X] = ListT[M,X]})#λ] {
    def bind[A, B](a: ListT[M,A], f: A => ListT[M,B]) = a flatMap f 
  }
  implicit def listTSemigroup[M[_]:Functor,A]
    : Semigroup[ListT[M,A]] = 
  new Semigroup[ListT[M,A]] { 
    def append(s1: ListT[M,A], s2: => ListT[M,A]) = s1 ++ s2
  }

  implicit def listTZero[M[_]:Pure,A]
    : Zero[ListT[M,A]] =
  new Zero[ListT[M,A]] {
    val zero = empty[M,A]
  }
}
