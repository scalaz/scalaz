package scalaz

/**
 * StreamT monad transformer.
 */
sealed class StreamT[M[_], A](val step: M[StreamT.Step[A, StreamT[M, A]]]) {

  import StreamT._

  def uncons(implicit M: Monad[M]): M[Option[(A, StreamT[M, A])]] =
    M.bind(step) (
      _( yieldd = (a, s) => M.point(Some((a, s)))
       , skip = s => s.uncons
       , done = M.point(None)
       ))
  
  def ::(a: => A)(implicit M: Pointed[M]): StreamT[M, A] = StreamT[M, A](M.point(yieldd(a, this)))
    
  def isEmpty(implicit M: Monad[M]): M[Boolean] = M.map(uncons)(_.isDefined)

  def head(implicit M: Monad[M]): M[A] = M.map(uncons)(_.getOrElse(sys.error("head: empty StreamT"))._1)
    
  def headOption(implicit M: Monad[M]): M[Option[A]] = M.map(uncons)(_.map(_._1))
  
  def tailM(implicit M: Monad[M]): M[StreamT[M, A]] = M.map(uncons)(_.getOrElse(sys.error("tailM: empty StreamT"))._2)

  def filter(p: A => Boolean)(implicit m: Functor[M]): StreamT[M, A] = stepMap {
    _( yieldd = (a, as) => if (p(a)) yieldd(a, as filter p) else skip(as filter p)
     , skip = as => skip(as filter p)
     , done = done
     )
  }

  def drop(n: Int)(implicit M: Functor[M]): StreamT[M, A] = stepMap {
    _( yieldd = (a, as) => if (n > 0) skip(as drop (n-1)) else yieldd(a, as)
     , skip = as => skip(as drop n)
     , done = done
     )
  }
  
  def dropWhile(p: A => Boolean)(implicit m: Functor[M]): StreamT[M, A] = stepMap {
    _( yieldd = (a, as) => if (p(a)) skip(as dropWhile p) else yieldd(a, as)
     , skip = as => skip(as dropWhile p)
     , done = done
     )
  }
  
  def take(n: Int)(implicit M: Functor[M]): StreamT[M, A] = stepMap {
    _( yieldd = (a, as) => if (n <= 0) done else yieldd(a, as take (n-1))
     , skip = as => skip(as take n)
     , done = done
     )
  }
  
  def takeWhile(p: A => Boolean)(implicit m: Functor[M]): StreamT[M, A] = stepMap {
    _( yieldd = (a, as) => if (p(a)) yieldd(a, as takeWhile p) else done
     , skip = as => skip(as takeWhile p)
     , done = done
     )
  }
      
  def ++(bs: => StreamT[M, A])(implicit m: Functor[M]): StreamT[M, A] = stepMap {
    _( yieldd = (a, as) => yieldd(a, as ++ bs)
     , skip = as => skip(as ++ bs)
     , done = skip(bs)
     )
  }

  def flatMap[B](f: A => StreamT[M, B])(implicit m: Functor[M]): StreamT[M, B] = stepMap {
    _( yieldd = (a, s) => skip(f(a) ++ (s flatMap f))
     , skip = s => skip(s flatMap f)
     , done = done
     )
  }
      
  def map[B](f: A => B)(implicit m: Functor[M]): StreamT[M, B] = stepMap {
    _( yieldd = (a, s) => yieldd(f(a), s map f)
     , skip = s => skip(s map f)
     , done = done
     )
  }
      
  /**Don't use iteratively! */
  def tail(implicit m: Functor[M]): StreamT[M, A] = stepMap {
    _( yieldd = (a, s) => skip(s)
     , skip = s => skip(s.tail)
     , done = sys.error("tail: empty StreamT")
     )
  }

  def foldLeft[B](z: => B)(f: (=> B, => A) => B)(implicit M: Monad[M]): M[B] =
    M.bind(step) {
      _( yieldd = (a, s) => s.foldLeft(f(z, a))(f)
       , skip = s => s.foldLeft(z)(f)
       , done = M.point(z)
       )
    }
  
  def toStream(implicit M: Monad[M]): M[Stream[A]] = M.map(rev(Stream.Empty))(_.reverse)
    
  def foldRight[B](z: => B)(f: (=> A, => B) => B)(implicit M: Monad[M]): M[B] =
    M.map(rev(Stream.Empty)) {
      _.foldLeft(z)((a, b) => f(b, a))
    }

  def length(implicit m: Monad[M]): M[Int] = {
    def addOne(c: => Int, a: => A) = 1 + c
    foldLeft(0)(addOne _)
  }
  
  private def stepMap[B](f: Step[A, StreamT[M, A]] => Step[B, StreamT[M, B]])(implicit M: Functor[M]): StreamT[M, B] = StreamT(M.map(step)(f))

  private def rev(xs: Stream[A])(implicit M: Monad[M]): M[Stream[A]] =
    M.bind(step) {
      _( yieldd = (a, s) => s rev (a #:: xs)
       , skip = s => s rev xs
       , done = M.point(xs)
       )
    }
}

//
// Prioritized Implicits for type class instances
//

trait StreamTInstances2 {
  implicit def StreamTFunctor[F[_]](implicit F0: Functor[F]): Functor[({type λ[α] = StreamT[F, α]})#λ] = new StreamTFunctor[F] {
    implicit def F: Functor[F] = F0
  }

  implicit def StreamTSemigroup[F[_], A](implicit F0: Functor[F]): Semigroup[StreamT[F, A]] = new StreamTSemigroup[F, A] {
    implicit def F: Functor[F] = F0
  }
}

trait StreamTInstances1 extends StreamTInstances2 {
  implicit def StreamTPointedPlus[F[_]](implicit F0: Pointed[F]): Pointed[({type λ[α] = StreamT[F, α]})#λ] with Plus[({type λ[α] = StreamT[F, α]})#λ] = new StreamTPointed[F] {
    implicit def F: Pointed[F] = F0
  }

  implicit def StreamTMonoid[F[_], A](implicit F0: Pointed[F]): Monoid[StreamT[F, A]] = new StreamTMonoid[F, A] {
    implicit def F: Pointed[F] = F0
  }
}

trait StreamTInstances0 extends StreamTInstances1 {
  implicit def StreamTMonad[F[_]](implicit F0: Monad[F]): Monad[({type λ[α] = StreamT[F, α]})#λ] = new StreamTMonad[F] {
    implicit def F: Monad[F] = F0
  }
}

trait StreamTInstances extends StreamTInstances0 {
  implicit def StreamTMonadPlus[F[_]](implicit F0: Monad[F]): MonadPlus[({type λ[α] = StreamT[F, α]})#λ] = new StreamTMonadPlus[F] {
    implicit def F: Monad[F] = F0
  }
  implicit def StreamTEqual[F[_], A](implicit E: Equal[F[Stream[A]]], F: Monad[F]): Equal[StreamT[F, A]] = E.contramap((_: StreamT[F, A]).toStream)
  implicit def StreamTShow[F[_], A](implicit E: Show[F[Stream[A]]], F: Monad[F]): Show[StreamT[F, A]] = Contravariant[Show].contramap(E)((_: StreamT[F, A]).toStream)
  implicit def StreamTHoist: Hoist[StreamT] = new StreamTHoist {}
}

object StreamT extends StreamTInstances {
  def apply[M[_], A](step: M[Step[A, StreamT[M, A]]]): StreamT[M, A] = new StreamT[M, A](step)

  def empty[M[_], A](implicit M: Pointed[M]): StreamT[M, A] = new StreamT[M, A](M point done)

  def fromStream[M[_], A](mas: M[Stream[A]])(implicit M: Pointed[M]): StreamT[M, A] = {
    def loop(as: Stream[A]): Step[A, StreamT[M, A]] = as match {
      case head #:: tail => yieldd(head, apply(M.point(loop(tail))))
      case _ => done
    }

    apply[M, A](M.map(mas)(loop))
  }
  
  abstract sealed class Step[+A, +S] {
    def apply[Z](yieldd: (A, => S) => Z, skip: => S => Z, done: => Z): Z
  }
  
  def yieldd[A, S](a: A, s: => S): Step[A, S] = new Step[A, S] {
    def apply[Z](yieldd: (A, => S) => Z, skip: => S => Z, done: => Z) = yieldd(a, s)
  }
  
  def skip[S](s: => S): Step[Nothing, S] = new Step[Nothing, S] {
    def apply[Z](yieldd: (Nothing, => S) => Z, skip: => S => Z, done: => Z) = skip(s)
  }

  def done: Step[Nothing, Nothing] = new Step[Nothing, Nothing] {
    def apply[Z](yieldd: (Nothing, => Nothing) => Z, skip: => Nothing => Z, done: => Z) = done
  }
}

//
// Implementation traits for type class instances
//

private[scalaz] trait StreamTFunctor[F[_]] extends Functor[({type λ[α] = StreamT[F, α]})#λ] {
  implicit def F: Functor[F]

  override def map[A, B](fa: StreamT[F, A])(f: A => B): StreamT[F, B] = fa map f
}

private[scalaz] trait StreamTSemigroup[F[_], A] extends Semigroup[StreamT[F, A]] {
  implicit def F: Functor[F]

  def append(f1: StreamT[F, A], f2: => StreamT[F, A]): StreamT[F, A] = f1 ++ f2
}

private[scalaz] trait StreamTMonoid[F[_], A] extends Monoid[StreamT[F, A]] with StreamTSemigroup[F, A] {
  implicit def F: Pointed[F]

  def zero: StreamT[F, A] = StreamT.empty[F, A]
}

private[scalaz] trait StreamTPointed[F[_]] extends Pointed[({type λ[α] = StreamT[F, α]})#λ] with Plus[({type λ[α] = StreamT[F, α]})#λ] with StreamTFunctor[F] {
  implicit def F: Pointed[F]

  def point[A](a: => A): StreamT[F, A] = a :: StreamT.empty[F, A]

  def empty[A]: StreamT[F, A] = StreamT.empty

  def plus[A](a: StreamT[F, A], b: => StreamT[F, A]): StreamT[F, A] = a ++ b
}

private[scalaz] trait StreamTMonad[F[_]] extends Monad[({type λ[α] = StreamT[F, α]})#λ] with StreamTPointed[F] {
  implicit def F: Monad[F]

  def bind[A, B](fa: StreamT[F, A])(f: A => StreamT[F, B]): StreamT[F, B] = fa flatMap f
}

private[scalaz] trait StreamTMonadPlus[F[_]] extends MonadPlus[({type λ[α] = StreamT[F, α]})#λ] with StreamTMonad[F] {
  implicit def F: Monad[F]
}

private[scalaz] trait StreamTHoist extends Hoist[StreamT] {
  import StreamT._
  
  implicit def apply[G[_] : Monad]: Monad[({type λ[α] = StreamT[G, α]})#λ] = StreamTMonad[G]
  
  def liftM[G[_], A](a: G[A])(implicit G: Monad[G]): StreamT[G, A] = StreamT[G, A](G.map(a)(yieldd(_, empty)))
  
  def hoist[M[_], N[_]](f: M ~> N)(implicit M: Monad[M]): ({type f[x] = StreamT[M, x]})#f ~> ({type f[x] = StreamT[N, x]})#f =
    new (({type f[x] = StreamT[M, x]})#f ~> ({type f[x] = StreamT[N, x]})#f) {
      def apply[A](a: StreamT[M, A]): StreamT[N, A] = StreamT[N, A](f(M.map(a.step)(
        _( yieldd = (a, as) => yieldd(a, hoist(f) apply as)
         , skip = as => skip(hoist(f) apply as)
         , done = done
         ))))
    }
}
