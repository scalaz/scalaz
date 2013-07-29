package scalaz

import Id._

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
  
  def ::(a: => A)(implicit M: Applicative[M]): StreamT[M, A] = StreamT[M, A](M.point(Yield(a, this)))
    
  def isEmpty(implicit M: Monad[M]): M[Boolean] = M.map(uncons)(!_.isDefined)

  def head(implicit M: Monad[M]): M[A] = M.map(uncons)(_.getOrElse(sys.error("head: empty StreamT"))._1)
    
  def headOption(implicit M: Monad[M]): M[Option[A]] = M.map(uncons)(_.map(_._1))
  
  def tailM(implicit M: Monad[M]): M[StreamT[M, A]] = M.map(uncons)(_.getOrElse(sys.error("tailM: empty StreamT"))._2)

  def trans[N[_]](t: M ~> N)(implicit M: Monad[M], N: Functor[N]): StreamT[N, A] =
    StreamTHoist.hoist(t).apply(this)

  def filter(p: A => Boolean)(implicit m: Functor[M]): StreamT[M, A] = stepMap {
    _( yieldd = (a, as) => if (p(a)) Yield(a, as filter p) else Skip(as filter p)
     , skip = as => Skip(as filter p)
     , done = Done
     )
  }

  def drop(n: Int)(implicit M: Functor[M]): StreamT[M, A] = stepMap {
    _( yieldd = (a, as) => if (n > 0) Skip(as drop (n-1)) else Yield(a, as)
     , skip = as => Skip(as drop n)
     , done = Done
     )
  }
  
  def dropWhile(p: A => Boolean)(implicit m: Functor[M]): StreamT[M, A] = stepMap {
    _( yieldd = (a, as) => if (p(a)) Skip(as dropWhile p) else Yield(a, as)
     , skip = as => Skip(as dropWhile p)
     , done = Done
     )
  }
  
  def take(n: Int)(implicit M: Functor[M]): StreamT[M, A] = stepMap {
    _( yieldd = (a, as) => if (n <= 0) Done else Yield(a, as take (n-1))
     , skip = as => Skip(as take n)
     , done = Done
     )
  }
  
  def takeWhile(p: A => Boolean)(implicit m: Functor[M]): StreamT[M, A] = stepMap {
    _( yieldd = (a, as) => if (p(a)) Yield(a, as takeWhile p) else Done
     , skip = as => Skip(as takeWhile p)
     , done = Done
     )
  }
      
  def ++(bs: => StreamT[M, A])(implicit m: Functor[M]): StreamT[M, A] = stepMap {
    _( yieldd = (a, as) => Yield(a, as ++ bs)
     , skip = as => Skip(as ++ bs)
     , done = Skip(bs)
     )
  }

  def flatMap[B](f: A => StreamT[M, B])(implicit m: Functor[M]): StreamT[M, B] = stepMap {
    _( yieldd = (a, s) => Skip(f(a) ++ (s flatMap f))
     , skip = s => Skip(s flatMap f)
     , done = Done
     )
  }
      
  def map[B](f: A => B)(implicit m: Functor[M]): StreamT[M, B] = stepMap {
    _( yieldd = (a, s) => Yield(f(a), s map f)
     , skip = s => Skip(s map f)
     , done = Done
     )
  }
      
  def mapM[B](f: A => M[B])(implicit m: Monad[M]): StreamT[M, B] = stepBind {
    _( yieldd = (a, s) => m.map(f(a)) { Yield(_, s mapM f) }
     , skip = s => m.point(Skip(s mapM f))
     , done = m.point(Done)
     )
  }

  /**Don't use iteratively! */
  def tail(implicit m: Functor[M]): StreamT[M, A] = stepMap {
    _( yieldd = (a, s) => Skip(s)
     , skip = s => Skip(s.tail)
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
  
  def toStream(implicit M: Monad[M]): M[Stream[A]] = M.map(rev)(_.reverse)
    
  def foldRight[B](z: => B)(f: (=> A, => B) => B)(implicit M: Monad[M]): M[B] =
    M.map(rev) {
      _.foldLeft(z)((a, b) => f(b, a))
    }

  def length(implicit m: Monad[M]): M[Int] = {
    def addOne(c: => Int, a: => A) = 1 + c
    foldLeft(0)(addOne _)
  }

  def foreach(f: A => M[Unit])(implicit M: Monad[M]): M[Unit] = M.bind(step) {
    case Yield(a,s) => M.bind(f(a))(_ => s().foreach(f))
    case Skip(s) => s().foreach(f)
    case Done => M.pure(())
  }
  
  private def stepMap[B](f: Step[A, StreamT[M, A]] => Step[B, StreamT[M, B]])(implicit M: Functor[M]): StreamT[M, B] = StreamT(M.map(step)(f))

  private def stepBind[B](f: Step[A, StreamT[M, A]] => M[Step[B, StreamT[M, B]]])(implicit M: Monad[M]): StreamT[M, B] = StreamT(M.bind(step)(f))

  private def rev(implicit M: Monad[M]): M[Stream[A]] = {
    def loop(xs: StreamT[M, A], ys: Stream[A]): M[Stream[A]] =
      M.bind(xs.step) {
        _( yieldd = (a, s) => loop(s, a #:: ys)
         , skip = s => loop(s, ys)
         , done = M.point(ys)
         )
      }
    loop(this, Stream.Empty)
  }
}

//
// Prioritized Implicits for type class instances
//

sealed abstract class StreamTInstances0 {
  implicit def StreamTFunctor[F[_]](implicit F0: Functor[F]): Functor[({type λ[α] = StreamT[F, α]})#λ] = new StreamTFunctor[F] {
    implicit def F: Functor[F] = F0
  }

  implicit def StreamTSemigroup[F[_], A](implicit F0: Functor[F]): Semigroup[StreamT[F, A]] = new StreamTSemigroup[F, A] {
    implicit def F: Functor[F] = F0
  }
}

sealed abstract class StreamTInstances extends StreamTInstances0 {
  implicit def StreamTMonoid[F[_], A](implicit F0: Applicative[F]): Monoid[StreamT[F, A]] = new StreamTMonoid[F, A] {
    implicit def F: Applicative[F] = F0
  }
  implicit def StreamTMonadPlus[F[_]](implicit F0: Applicative[F]): MonadPlus[({type λ[α] = StreamT[F, α]})#λ] = new StreamTMonadPlus[F] {
    implicit def F: Applicative[F] = F0
  }
  implicit def StreamTEqual[F[_], A](implicit E: Equal[F[Stream[A]]], F: Monad[F]): Equal[StreamT[F, A]] = E.contramap((_: StreamT[F, A]).toStream)
  implicit def StreamTShow[F[_], A](implicit E: Show[F[Stream[A]]], F: Monad[F]): Show[StreamT[F, A]] = Contravariant[Show].contramap(E)((_: StreamT[F, A]).toStream)
  implicit val StreamTHoist: Hoist[StreamT] = new StreamTHoist {}
}

object StreamT extends StreamTInstances {
  def apply[M[_], A](step: M[Step[A, StreamT[M, A]]]): StreamT[M, A] = new StreamT[M, A](step)

  def empty[M[_], A](implicit M: Applicative[M]): StreamT[M, A] = new StreamT[M, A](M point Done)

  def fromStream[M[_], A](mas: M[Stream[A]])(implicit M: Applicative[M]): StreamT[M, A] = {
    def loop(as: Stream[A]): Step[A, StreamT[M, A]] = as match {
      case head #:: tail => Yield(head, apply(M.point(loop(tail))))
      case _ => Done
    }

    apply[M, A](M.map(mas)(loop))
  }

  def unfoldM[M[_],A,B](start: B)(f: B => M[Option[(A,B)]])(implicit M: Functor[M]): StreamT[M,A] =
    StreamT[M,A](M.map(f(start)) {
      case Some((a, b)) => Yield(a, unfoldM(b)(f))
      case None => Done
    })

  def unfold[A,B](b: B)(f: B => Option[(A,B)]): StreamT[Id,A] = unfoldM[Id,A,B](b)(f)

  def fromIterable[A](s: Iterable[A]): StreamT[Id,A] = {
    def stepper(b: Iterable[A]): Option[(A,Iterable[A])] = if (b.isEmpty) None else Some((b.head, b.tail))
    unfold(s)(stepper)
  }

  def wrapEffect[M[_]:Functor,A](m: M[StreamT[M,A]]): StreamT[M,A] = StreamT(Functor[M].map(m)(Skip(_)))
  
  abstract sealed class Step[+A, +S] {
    def apply[Z](yieldd: (A, => S) => Z, skip: => S => Z, done: => Z): Z
  }

  def runStreamT[S,A](stream : StreamT[({type λ[X] = State[S,X]})#λ,A], s0: S): StreamT[Id,A] =
    StreamT[Id,A]({
      val (s1, sa) = stream.step(s0)
      sa((a, as) => Yield(a, runStreamT(as, s1)),
         as => Skip(runStreamT(as, s1)),
         Done)
    })
  
  object Yield {
    def apply[A, S](a: A, s: => S): Step[A, S] = new Step[A, S] {
      def apply[Z](yieldd: (A, => S) => Z, skip: => S => Z, done: => Z) = yieldd(a, s)
    }
    def unapply[A, S](s: Step[A, S]): Option[(A, () => S)] =
      s((aa, sa) => Some((aa, () => sa)), _ => None, None)
  }
  
  object Skip {
    def apply[S](s: => S): Step[Nothing, S] = new Step[Nothing, S] {
      def apply[Z](yieldd: (Nothing, => S) => Z, skip: => S => Z, done: => Z) = skip(s)
    }
    def unapply[A, S](s: Step[A, S]): Option[(() => S)] =
      s((_, _) => None, s => Some(() => s), None)
  }

  object Done extends Step[Nothing, Nothing] {
    def apply[Z](yieldd: (Nothing, => Nothing) => Z, skip: => Nothing => Z, done: => Z) = done
    def unapply[A, S](s: Step[A, S]): Boolean = s((_, _) => false, _ => false, true)
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
  implicit def F: Applicative[F]

  def zero: StreamT[F, A] = StreamT.empty[F, A]
}

private[scalaz] trait StreamTMonadPlus[F[_]] extends MonadPlus[({type λ[α] = StreamT[F, α]})#λ] with StreamTFunctor[F] {
  implicit def F: Applicative[F]

  def point[A](a: => A): StreamT[F, A] = a :: StreamT.empty[F, A]

  def empty[A]: StreamT[F, A] = StreamT.empty

  def plus[A](a: StreamT[F, A], b: => StreamT[F, A]): StreamT[F, A] = a ++ b

  def bind[A, B](fa: StreamT[F, A])(f: A => StreamT[F, B]): StreamT[F, B] = fa flatMap f
}

private[scalaz] trait StreamTHoist extends Hoist[StreamT] {
  import StreamT._
  
  implicit def apply[G[_] : Monad]: Monad[({type λ[α] = StreamT[G, α]})#λ] = StreamTMonadPlus[G]
  
  def liftM[G[_], A](a: G[A])(implicit G: Monad[G]): StreamT[G, A] = StreamT[G, A](G.map(a)(Yield(_, empty)))
  
  def hoist[M[_], N[_]](f: M ~> N)(implicit M: Monad[M]): ({type f[x] = StreamT[M, x]})#f ~> ({type f[x] = StreamT[N, x]})#f =
    new (({type f[x] = StreamT[M, x]})#f ~> ({type f[x] = StreamT[N, x]})#f) {
      def apply[A](a: StreamT[M, A]): StreamT[N, A] = StreamT[N, A](f(M.map(a.step)(
        _( yieldd = (a, as) => Yield(a, hoist(f) apply as)
         , skip = as => Skip(hoist(f) apply as)
         , done = Done
         ))))
    }
}
