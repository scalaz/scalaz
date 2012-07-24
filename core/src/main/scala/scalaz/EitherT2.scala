package scalaz

/**
 * Represents a computation of type `F[A \/ B]`.
 *
 * Example:
 * {{{
 * val x: Option[String \/ Int] = Some(\/-(1))
 * EitherT(x).map(1+).run // Some(\/-(2)
 * }}}
 * */
sealed trait EitherT2[F[+_], +A, +B] {
  val run: F[A \/ B]

  import OptionT._

  sealed trait Switching_\/[X] {
    def r: X

    def <<?:(left: => X)(implicit F: Functor[F]): F[X] =
      F.map(EitherT2.this.run){
        case -\/(_) => left
        case \/-(_) => r
      }
  }

  def :?>>[X](right: => X): Switching_\/[X] =
    new Switching_\/[X] {
      def r = right
    }

  def isLeft(implicit F: Functor[F]): F[Boolean] =
    F.map(run)(_.isLeft)

  def isRight(implicit F: Functor[F]): F[Boolean] =
    F.map(run)(_.isRight)

  def swap(implicit F: Functor[F]): EitherT2[F, B, A] =
    EitherT2(F.map(run)(_.swap))

  def unary_~(implicit F: Functor[F]): EitherT2[F, B, A] =
    swap

  def swapped[AA >: A, BB >: B](k: (B \/ A) => (BB \/ AA))(implicit F: Functor[F]): EitherT2[F, AA, BB] =
    EitherT2(F.map(run)(_ swapped k))

  def ~[AA >: A, BB >: B](k: (B \/ A) => (BB \/ AA))(implicit F: Functor[F]): EitherT2[F, AA, BB] =
    swapped(k)

  def bimap[C, D](f: A => C, g: B => D)(implicit F: Functor[F]): EitherT2[F, C, D] =
    EitherT2(F.map(run)(_.bimap(f, g)))

  def bitraverse[G[_], C, D](f: (A) => G[C], g: (B) => G[D])(implicit F: Traverse[F], G: Applicative[G]): G[EitherT2[F, C, D]] =
    Applicative[G].map(F.traverse(run)(Bitraverse[\/].bitraverseF(f, g)))(EitherT2(_: F[C \/ D]))

  def map[C](f: B => C)(implicit F: Functor[F]): EitherT2[F, A, C] =
    EitherT2(F.map(run)(_.map(f)))

  def traverse[G[_], AA >: A, C](f: (B) => G[C])(implicit F: Traverse[F], G: Applicative[G]): G[EitherT2[F, AA, C]] =
    G.map(F.traverse(run)(o => Traverse[({type λ[α] = (AA \/ α)})#λ].traverse(o)(f)))(EitherT2(_))

  def foreach(f: B => Unit)(implicit F: Each[F]): Unit =
    F.each(run)(_ foreach f)

  def ap[AA >: A, C](f: => EitherT2[F, AA, B => C])(implicit F: Apply[F]): EitherT2[F, AA, C] =
    EitherT2(F.map2(run, f.run)((a, b) => a flatMap (x => b map (_(x)))))

  def flatMap[AA >: A, C](f: B => EitherT2[F, AA, C])(implicit F: Monad[F]): EitherT2[F, AA, C] =
    EitherT2(F.bind(run)(_.fold(a => F.point(-\/(a): (AA \/ C)), b => f(b).run)))

  def foldRight[Z](z: => Z)(f: (B, => Z) => Z)(implicit F: Foldable[F]): Z = {
    F.foldRight[A \/ B, Z](run, z)((a, b) => a.foldRight(b)(f))
  }

  def filter[BB >: B](p: BB => Boolean)(implicit M: Monoid[BB], F: Functor[F]): EitherT2[F, A, BB] =
    EitherT2(F.map(run)(_ filter p))

  def exists(f: B => Boolean)(implicit F: Functor[F]): F[Boolean] =
    F.map(run)(_ exists f)

  def forall(f: B => Boolean)(implicit F: Functor[F]): F[Boolean] =
    F.map(run)(_ forall f)

  def toList(implicit F: Functor[F]): F[List[B]] =
    F.map(run)(_.fold(_ => Nil, List(_)))

  def toStream(implicit F: Functor[F]): F[Stream[B]] =
    F.map(run)((_: (A \/ B)).fold(_ => Stream(), Stream(_)))

  def toOption(implicit F: Functor[F]): OptionT[F, B] =
    optionT[F](F.map(run)((_: (A \/ B)).toOption))

  def toEither(implicit F: Functor[F]): F[Either[A, B]] =
    F.map(run)(_.toEither)

  def getOrElse[BB >: B](default: => BB)(implicit F: Functor[F]): F[BB] =
    F.map(run)(_ getOrElse default)

  def ?[BB >: B](default: => BB)(implicit F: Functor[F]): F[BB] =
    getOrElse(default)

  def valueOr[BB >: B](x: A => BB)(implicit F: Functor[F]): F[BB] =
    F.map(run)(_ valueOr x)

  def orElse[AA >: A, BB >: B](x: => EitherT2[F, AA, BB])(implicit F: Bind[F]): EitherT2[F, AA, BB] = {
    val g = run
    EitherT2(F.bind(g) {
      case -\/(_) => x.run
      case \/-(_) => g
    })
  }

  def |||[AA >: A, BB >: B](x: => EitherT2[F, AA, BB])(implicit F: Bind[F]): EitherT2[F, AA, BB] =
    |||(x)

  def ++[AA >: A, BB >: B](x: => EitherT2[F, AA, BB])(implicit M: Semigroup[BB], F: Apply[F]): EitherT2[F, AA, BB] =
    EitherT2(F.map2(run, x.run)(_ ++ _))

  def ===[AA >: A, BB >: B](x: EitherT2[F, AA, BB])(implicit EA: Equal[AA], EB: Equal[BB], F: Apply[F]): F[Boolean] =
    F.map2(run, x.run)(_ == _)

  def compare[AA >: A, BB >: B](x: EitherT2[F, AA, BB])(implicit EA: Order[AA], EB: Order[BB], F: Apply[F]): F[Ordering] =
    F.map2(run, x.run)(_ compare _)

  def show[AA >: A, BB >: B](implicit SA: Show[AA], SB: Show[BB], F: Functor[F]): F[List[Char]] =
    F.map(run)(_.show[AA, BB])

  def cozip(implicit Z: Cozip2[F]): (F[A] \/ F[B]) =
    Z.cozip(run)
}

object EitherT2 extends EitherT2Functions with EitherT2Instances {
  def apply[F[+_], A, B](a: F[A \/ B]): EitherT2[F, A, B] =
    eitherT[F, A, B](a)
}

trait EitherT2Instances4 {
  implicit def eitherTFunctor[F[+_], L](implicit F0: Functor[F]) = new EitherT2Functor[F, L] {
    implicit def F = F0
  }
}

trait EitherT2Instances3 extends EitherT2Instances4 {
  implicit def eitherTPointed[F[+_], L](implicit F0: Pointed[F]) = new EitherT2Pointed[F, L] {
    implicit def F = F0
  }
}

trait EitherT2Instances2 extends EitherT2Instances3 {
  implicit def eitherTApply[F[+_], L](implicit F0: Apply[F]) = new EitherT2Apply[F, L] {
    implicit def F = F0
  }
}

trait EitherT2Instances1 extends EitherT2Instances2 {
  implicit def eitherTApplicative[F[+_], L](implicit F0: Applicative[F]) = new EitherT2Applicative[F, L] {
    implicit def F = F0
  }
}

trait EitherT2Instances0 extends EitherT2Instances1 {
  implicit def eitherTBifunctor[F[+_]](implicit F0: Functor[F]) = new EitherT2Bifunctor[F] {
    implicit def F = F0
  }

  implicit def eitherTMonad[F[+_], L](implicit F0: Monad[F]) = new EitherT2Monad[F, L] {
    implicit def F = F0
  }
  implicit def eitherTFoldable[F[+_], L](implicit F0: Foldable[F]) = new EitherT2Foldable[F, L] {
    implicit def F = F0
  }
}

trait EitherT2Instances extends EitherT2Instances0 {
  implicit def eitherTBitraverse[F[+_]](implicit F0: Traverse[F]) = new EitherT2Bitraverse[F] {
    implicit def F = F0
  }

  implicit def eitherTTraverse[F[+_], L](implicit F0: Traverse[F]) = new EitherT2Traverse[F, L] {
    implicit def F = F0
  }

  implicit def eitherTMonadTrans[A]: MonadTrans[({type λ[α[+_], β] = EitherT2[α, A, β]})#λ] = new EitherT2MonadTrans[A] {}

  implicit def eitherTEqual[F[+_], A, B](implicit F0: Equal[F[Either[A, B]]]): Equal[EitherT[F, A, B]] = F0.contramap((_: EitherT[F, A, B]).run)
}
           
trait EitherT2Functions {
   def eitherT[F[+_], A, B](a: F[A \/ B]): EitherT2[F, A, B] = new EitherT2[F, A, B] {
     val run = a
   }
}


trait EitherT2Functor[F[+_], E] extends Functor[({type λ[α]=EitherT2[F, E, α]})#λ] {
  implicit def F: Functor[F]

  override def map[A, B](fa: EitherT2[F, E, A])(f: (A) => B): EitherT2[F, E, B] = fa map f
}

trait EitherT2Pointed[F[+_], E] extends Pointed[({type λ[α]=EitherT2[F, E, α]})#λ] with EitherT2Functor[F, E] {
  implicit def F: Pointed[F]

  def point[A](a: => A): EitherT2[F, E, A] = EitherT2(F.point(\/-(a)))
}

trait EitherT2Apply[F[+_], E] extends Apply[({type λ[α]=EitherT2[F, E, α]})#λ] with EitherT2Functor[F, E] {
  implicit def F: Apply[F]

  override def ap[A, B](fa: => EitherT2[F, E, A])(f: => EitherT2[F, E, (A) => B]): EitherT2[F, E, B] = fa ap f
}

trait EitherT2Applicative[F[+_], E] extends Applicative[({type λ[α]=EitherT2[F, E, α]})#λ] with EitherT2Apply[F, E] with EitherT2Pointed[F, E] {
  implicit def F: Applicative[F]
}

trait EitherT2Monad[F[+_], E] extends Monad[({type λ[α]=EitherT2[F, E, α]})#λ] with EitherT2Applicative[F, E] {
  implicit def F: Monad[F]

  def bind[A, B](fa: EitherT2[F, E, A])(f: (A) => EitherT2[F, E, B]): EitherT2[F, E, B] = fa flatMap f
}

trait EitherT2Foldable[F[+_], E] extends Foldable.FromFoldr[({type λ[α]=EitherT2[F, E, α]})#λ] {
  implicit def F: Foldable[F]

  override def foldRight[A, B](fa: EitherT2[F, E, A], z: => B)(f: (A, => B) => B): B = fa.foldRight(z)(f)
}

trait EitherT2Traverse[F[+_], E] extends Traverse[({type λ[α]=EitherT2[F, E, α]})#λ] with EitherT2Foldable[F, E] {
  implicit def F: Traverse[F]

  def traverseImpl[G[_]: Applicative, A, B](fa: EitherT2[F, E, A])(f: (A) => G[B]): G[EitherT2[F, E, B]] = fa traverse f
}

trait EitherT2Bifunctor[F[+_]] extends Bifunctor[({type λ[α, β]=EitherT2[F, α, β]})#λ] {
  implicit def F: Functor[F]

  override def bimap[A, B, C, D](fab: EitherT2[F, A, B])(f: (A) => C, g: (B) => D): EitherT2[F, C, D] = fab.bimap(f, g)
}

trait EitherT2Bitraverse[F[+_]] extends Bitraverse[({type λ[α, β] = EitherT2[F, α, β]})#λ] with EitherT2Bifunctor[F] {
  implicit def F: Traverse[F]

  def bitraverseImpl[G[_] : Applicative, A, B, C, D](fab: EitherT2[F, A, B])
                                                (f: (A) => G[C], g: (B) => G[D]): G[EitherT2[F, C, D]] =
    fab.bitraverse(f, g)
}

trait EitherT2MonadTrans[A] extends MonadTrans[({type λ[α[+_], β] = EitherT2[α, A, β]})#λ] {
  def hoist[M[+_], N[+_]](f: M ~> N)(implicit M: Monad[M]) = new (({type λ[α] = EitherT2[M, A, α]})#λ ~> ({type λ[α] = EitherT2[N, A, α]})#λ) {
    def apply[B](mb: EitherT2[M, A, B]): EitherT2[N, A, B] = EitherT2(f.apply(mb.run))
  }

  def liftM[M[+_], B](mb: M[B])(implicit M: Monad[M]): EitherT2[M, A, B] = EitherT2(M.map(mb)(\/-(_)))

  implicit def apply[M[+_] : Monad]: Monad[({type λ[α] = EitherT2[M, A, α]})#λ] = EitherT2.eitherTMonad
}
