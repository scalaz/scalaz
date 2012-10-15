package scalaz

import Id._

/**
 * StreamT monad transformer.
 */
sealed case class StreamT[M[+_], +A](underlying: M[Stream[A]]){
  import StreamT._
  
  def trans[N[+_]](t: M ~> N)(implicit M: Monad[M], N: Functor[N]): StreamT[N, A] =
    StreamTHoist.hoist(t).apply(this)
  
  def uncons(implicit M: Monad[M]): M[Option[(A, StreamT[M, A])]] = {
    M.map(underlying){stream =>
      stream match {
        case emptyStream if (stream.isEmpty) => None
        case streamHead #:: streamTail => Some(streamHead, new StreamT(M.point(streamTail)))
      }
    }
  }

  def ::[AA >: A](a: AA)(implicit M: Monad[M]) : StreamT[M, AA] = new StreamT(M.map(underlying)(stream => a +: stream))

  def isEmpty(implicit M: Functor[M]) : M[Boolean] = M.map(underlying)(_.isEmpty)

  def head(implicit M: Functor[M]) : M[A] = M.map(underlying)(_.head)

  def headOption(implicit M: Functor[M]) : M[Option[A]] = M.map(underlying)(_.headOption)
  
  def tailM(implicit M: Monad[M]) : M[StreamT[M, A]] = M.map(uncons)(_.get._2)

  def filter(p: A => Boolean)(implicit M: Functor[M]): StreamT[M, A] = new StreamT(M.map(underlying)(_.filter(p)))
  
  def drop(n: Int)(implicit M: Functor[M]) : StreamT[M, A] = new StreamT(M.map(underlying)(_.drop(n)))

  def dropWhile(p: A => Boolean)(implicit M: Functor[M]) : StreamT[M, A] = new StreamT(M.map(underlying)(_.dropWhile(p)))
  
  def take(n: Int)(implicit M: Functor[M]) : StreamT[M, A] = new StreamT(M.map(underlying)(_.take(n)))

  def takeWhile(p: A => Boolean)(implicit M: Functor[M]) : StreamT[M, A] = new StreamT(M.map(underlying)(_.takeWhile(p)))

  def ++[B >: A](bs: => StreamT[M, B])(implicit M: Bind[M]) : StreamT[M, B] = new StreamT(M.bind(underlying){stream1 =>
    M.map(bs.underlying){stream2 =>
      stream1 ++ stream2
    }
  })

  def flatMap[B](f: A => StreamT[M, B])(implicit M: Monad[M]) : StreamT[M, B] = new StreamT(M.bind(underlying){stream =>
    stream match {
      case emptyStream if(emptyStream.isEmpty) => M.point(Stream.empty[B])
      case nonEmpty => nonEmpty.map(f).reduce(_ ++ _).underlying
    }
  })

  def map[B](f: A => B)(implicit M: Functor[M]) : StreamT[M, B] = new StreamT(M.map(underlying)(_.map(f)))

  /**Don't use iteratively! */
  def tail(implicit M: Functor[M]) : StreamT[M, A] = new StreamT(M.map(underlying)(_.tail))

  def foldLeft[B](z: => B)(f: (=> B, => A) => B)(implicit M: Functor[M]) : M[B] = M.map(underlying)(_.foldLeft(z){(left, right) => f(left, right)})

  def toStream() : M[Stream[A]] = underlying

  def foldRight[B](z: => B)(f: (=> A, => B) => B)(implicit M: Functor[M]) : M[B] = M.map(underlying)(_.foldRight(z){(right, left) => f(right, left)})

  def length(implicit M: Functor[M]) : M[Int] = M.map(underlying)(_.length)
}

//
// Prioritized Implicits for type class instances
//

trait StreamTInstances2 {
  implicit def StreamTFunctor[F[+_]](implicit F0: Functor[F]): Functor[({type λ[α] = StreamT[F, α]})#λ] = new StreamTFunctor[F] {
    implicit def F: Functor[F] = F0
  }

  implicit def StreamTSemigroup[F[+_], A](implicit F0: Monad[F]): Semigroup[StreamT[F, A]] = new StreamTSemigroup[F, A] {
    implicit def F: Monad[F] = F0
  }
}

trait StreamTInstances1 extends StreamTInstances2 {
  implicit def StreamTPointedPlus[F[+_]](implicit F0: Monad[F]): Pointed[({type λ[α] = StreamT[F, α]})#λ] with Plus[({type λ[α] = StreamT[F, α]})#λ] = new StreamTPointed[F] {
    implicit def F: Monad[F] = F0
  }

  implicit def StreamTMonoid[F[+_], A](implicit F0: Monad[F]): Monoid[StreamT[F, A]] = new StreamTMonoid[F, A] {
    implicit def F: Monad[F] = F0
  }
}

trait StreamTInstances0 extends StreamTInstances1 {
  implicit def StreamTMonad[F[+_]](implicit F0: Monad[F]): Monad[({type λ[α] = StreamT[F, α]})#λ] = new StreamTMonad[F] {
    implicit def F: Monad[F] = F0
  }
}

trait StreamTInstances extends StreamTInstances0 {
  implicit def StreamTMonadPlus[F[+_]](implicit F0: Monad[F]): MonadPlus[({type λ[α] = StreamT[F, α]})#λ] = new StreamTMonadPlus[F] {
    implicit def F: Monad[F] = F0
  }
  implicit def StreamTEqual[F[+_], A](implicit E: Equal[F[Stream[A]]], F: Monad[F]): Equal[StreamT[F, A]] = E.contramap((_: StreamT[F, A]).toStream)
  implicit def StreamTShow[F[+_], A](implicit E: Show[F[Stream[A]]], F: Monad[F]): Show[StreamT[F, A]] = Contravariant[Show].contramap(E)((_: StreamT[F, A]).toStream)
  implicit def StreamTHoist: Hoist[StreamT] = new StreamTHoist {}
}

object StreamT extends StreamTInstances {
  def empty[M[+_], A](implicit M: Pointed[M]): StreamT[M, A] = new StreamT[M, A](M.point(Stream.empty[A]))

  def fromStream[M[+_], A](mas: M[Stream[A]]): StreamT[M, A] = new StreamT[M, A](mas)

  // TODO: Will investigate how to resolve this later.
  /*
  def unfoldM[M[+_], A, B](start: B)(f: B => M[Option[(A,B)]])(implicit M: Functor[M]): StreamT[M,A] =
    StreamT[M,A](M.map(f(start)) {
      case Some((a, b)) => Yield(a, unfoldM(b)(f))
      case None => Done
    })

  def unfold[A,B](b: B)(f: B => Option[(A,B)]): StreamT[Id,A] = unfoldM[Id,A,B](b)(f)
  
  def wrapEffect[M[+_]:Functor, A](m: M[StreamT[M, A]]): StreamT[M, A] = StreamT(Functor[M].map(m)(Skip(_)))

  def runStreamT[S,A](stream : StreamT[({type λ[+X] = State[S,X]})#λ,A], s0: S): StreamT[Id,A] =
    StreamT[Id,A]({
      val (s1, sa) = stream.step(s0)
      sa((a, as) => Yield(a, runStreamT(as, s1)),
         as => Skip(runStreamT(as, s1)),
         Done)
    })

 */

  def fromIterable[A](s: Iterable[A]): StreamT[Id, A] = new StreamT(Pointed[Id].point(s.toStream))
}

//
// Implementation traits for type class instances
//

private[scalaz] trait StreamTFunctor[F[+_]] extends Functor[({type λ[α] = StreamT[F, α]})#λ] {
  implicit def F: Functor[F]

  override def map[A, B](fa: StreamT[F, A])(f: A => B): StreamT[F, B] = fa map f
}

private[scalaz] trait StreamTSemigroup[F[+_], A] extends Semigroup[StreamT[F, A]] {
  implicit def F: Monad[F]

  def append(f1: StreamT[F, A], f2: => StreamT[F, A]): StreamT[F, A] = f1 ++ f2
}

private[scalaz] trait StreamTMonoid[F[+_], A] extends Monoid[StreamT[F, A]] with StreamTSemigroup[F, A] {
  implicit def F: Monad[F]

  def zero: StreamT[F, A] = StreamT.empty[F, A]
}

private[scalaz] trait StreamTPointed[F[+_]] extends Pointed[({type λ[α] = StreamT[F, α]})#λ] with Plus[({type λ[α] = StreamT[F, α]})#λ] with StreamTFunctor[F] {
  implicit def F: Monad[F]

  def point[A](a: => A): StreamT[F, A] = a :: StreamT.empty[F, A]

  def empty[A]: StreamT[F, A] = StreamT.empty

  def plus[A](a: StreamT[F, A], b: => StreamT[F, A]): StreamT[F, A] = a ++ b
}

private[scalaz] trait StreamTMonad[F[+_]] extends Monad[({type λ[α] = StreamT[F, α]})#λ] with StreamTPointed[F] {
  implicit def F: Monad[F]

  def bind[A, B](fa: StreamT[F, A])(f: A => StreamT[F, B]): StreamT[F, B] = fa flatMap f
}

private[scalaz] trait StreamTMonadPlus[F[+_]] extends MonadPlus[({type λ[α] = StreamT[F, α]})#λ] with StreamTMonad[F] {
  implicit def F: Monad[F]
}

private[scalaz] trait StreamTHoist extends Hoist[StreamT] {
  import StreamT._
  
  implicit def apply[G[+_] : Monad]: Monad[({type λ[α] = StreamT[G, α]})#λ] = StreamTMonad[G]
  
  def liftM[G[+_], A](a: G[A])(implicit G: Monad[G]): StreamT[G, A] = fromStream(G.map(a)(entry => Stream(entry)))
  
  def hoist[M[+_], N[+_]](f: M ~> N)(implicit M: Monad[M]): ({type f[x] = StreamT[M, x]})#f ~> ({type f[x] = StreamT[N, x]})#f =
    new (({type f[x] = StreamT[M, x]})#f ~> ({type f[x] = StreamT[N, x]})#f) {
      def apply[A](a: StreamT[M, A]): StreamT[N, A] = fromStream(f(a.underlying))
    }
}
