package scalaz

sealed trait ValidationT[F[_], E, A] {
  self =>

  import ValidationT._

  def run: F[Validation[E, A]]
  
  def map[B](f: A => B)(implicit F: Functor[F]): ValidationT[F, E, B] =
    ValidationT(F.map(run)(_.map(f)))
  
  def flatMap[B](f: A => ValidationT[F, E, B])(implicit F: Monad[F]): ValidationT[F, E, B] = new ValidationT[F, E, B]{
    def run = F.bind(self.run) {
      case Success(a) => f(a).run
      case Failure(e) => F.point(Failure(e))
    }
  }
  
  def flatMapF[B](f: A => F[B])(implicit F: Monad[F]): ValidationT[F, E, B] =
    ValidationT(F.bind(run)(_.traverse(f)))
  
  def either(implicit F: Functor[F]): EitherT[F, E, A] = EitherT(F.map(self.run)(_.either))
  
  def isSuccess(implicit F: Functor[F]): F[Boolean] = F.map(self.run)(_.isSuccess)
  
  def isFailure(implicit F: Functor[F]): F[Boolean] = F.map(self.run)(_.isFailure)
  
  def toOption(implicit F: Functor[F]): OptionT[F, A] = OptionT(F.map(self.run)(_.toOption))
  
  def append[EE >: E, AA >: A](x: ValidationT[F, EE, AA])(implicit F: Monad[F], es: Semigroup[EE], as: Semigroup[AA]): ValidationT[F, EE, AA] =
    ValidationT(F.lift2[Validation[E, A], Validation[EE, AA], Validation[EE, AA]](_ >>*<< _)(run, x.run))
  
  def fail: FailProjectionT[F, E, A] = new FailProjectionT[F, E, A] {
    val validationT = self
  }
  
  def toValidationTNel[EE >: E, AA >: A](implicit F: Functor[F]): ValidationTNEL[F, EE, AA] = fail.pointT[NonEmptyList, EE, AA]

  def |||[AA >: A](f: E => AA)(implicit F: Functor[F]): F[AA] =
    F.map(run)(_ ||| f)
  
  def orElse[EE >: E, AA >: A](that: => ValidationT[F, EE, AA])(implicit E: Semigroup[EE], F: Monad[F]): ValidationT[F, EE, AA] =
    ValidationT(F.bind(self.run){
      case r @ Success(_) => F.point((r: Validation[EE, AA]))
      case Failure(e1) => F.bind(that.run){
        case Success(_) => that.run
        case Failure(e2) => F.point((Failure(E.append(e1, e2)): Validation[EE, AA]))
      }
    })
  
  def getOrElse[AA >: A](d: => AA)(implicit F: Functor[F]): F[AA] = |||(_ => d)

  def |[AA >: A](d: => AA)(implicit F: Functor[F]): F[AA] = getOrElse(d)
  
  def exists(f: A => Boolean)(implicit F: Functor[F]): F[Boolean] =
    F.map(run)(_.exists(f))

  def forall(f: A => Boolean)(implicit F: Functor[F]): F[Boolean] =
    F.map(run)(_.forall(f))
  
  def traverse[G[_], B, EE >: E](f: A => G[B])(implicit G: Applicative[G], F: Traverse[F]): G[ValidationT[F, E, B]] =
    G.map(F.traverse(run)(_.traverse(f)))(ValidationT(_))
  
  def foldRight[B](z: => B)(f: (A, => B) => B)(implicit F: Foldable[F]): B =
    F.foldRight(run, z)(_.foldRight(_)(f))
  
  def ap[B, EE >: E](f: => ValidationT[F, EE, A => B])(implicit F: Applicative[F], E: Semigroup[EE]): ValidationT[F, EE, B] =
    ValidationT(F.lift2[Validation[E, A], Validation[EE, A => B], Validation[EE, B]](_.ap(_))(run, f.run))
  
  def bimap[C, D](f: E => C, g: A => D)(implicit F: Functor[F]): ValidationT[F, C, D] =
    ValidationT(F.map(run)(_.bimap(f, g)))
  
  def bitraverse[G[_], C, D](f: E => G[C], g: A => G[D])(implicit G: Applicative[G], F: Traverse[F]) =
    G.map(F.traverse(run)(_.bitraverse(f, g)))(ValidationT(_))
}

object ValidationT extends ValidationTFunctions with ValidationTInstances {
  def apply[F[_], E, A](m: F[Validation[E, A]]) = new ValidationT[F, E, A] {
    def run = m
  }

  sealed trait FailProjectionT[F[_], E, A]{
    self =>

    val validationT: ValidationT[F, E, A]
    
    def toOption(implicit M: Functor[F]): OptionT[F, E] =
      OptionT(M.map(validationT.run)(_.fail.toOption))
    
    def |||[EE >: E](f: A => EE)(implicit M: Functor[F]): F[EE] =
      M.map(validationT.run)(_.fail.|||(f))

    def getOrElse[EE >: E](f: => EE)(implicit M: Functor[F]): F[EE] = |||(_ => f)

    def |[EE >: E](f: => EE)(implicit M: Functor[F]): F[EE] = getOrElse(f)
    
    def pointT[G[_], EE >: E, AA >: A](implicit M: Functor[F], G: Pointed[G]): ValidationT[F, G[EE], AA] =
      ValidationT(M.map(validationT.run)(_.fail.point))

    def exists(f: E => Boolean)(implicit M: Functor[F]): F[Boolean] =
      M.map(validationT.run)(_.fail.exists(f))

    def forall(f: E => Boolean)(implicit M: Functor[F]): F[Boolean] =
      M.map(validationT.run)(_.fail.forall(f))

    def map[B](f: E => B)(implicit M: Functor[F]): FailProjectionT[F, B, A] = new FailProjectionT[F, B, A]{
      val validationT = ValidationT(M.map(self.validationT.run)(_.fail.map(f).validation))
    }
  }
}

trait ValidationTFunctions {
  def successT[F[_], E, A](a: => A)(implicit F: Pointed[F]): ValidationT[F, E, A] =
    ValidationT(F.point(Validation.success(a)))

  def failureT[F[_], E, A](e: => E)(implicit F: Pointed[F]): ValidationT[F, E, A] =
    ValidationT(F.point(Validation.failure(e)))

  import Isomorphism.{IsoFunctorTemplate, IsoBiFunctorTemplate}

  implicit def validationTFailProjectionTEIso2[F[_], E]: IsoFunctorTemplate[({type λ[α] = ValidationT.FailProjectionT[F, E, α]})#λ, ({type λ[α] = ValidationT[F, E, α]})#λ] = new IsoFunctorTemplate[({type λ[α] = ValidationT.FailProjectionT[F, E, α]})#λ, ({type λ[α] = ValidationT[F, E, α]})#λ] {
    def to[A](fa: ValidationT.FailProjectionT[F, E, A]): ValidationT[F, E, A] = fa.validationT
    def from[A](ga: ValidationT[F, E, A]): ValidationT.FailProjectionT[F, E, A] = ga.fail
  }
  implicit def validationTFailProjectionTIso2[F[_]]: IsoBiFunctorTemplate[({type λ[α, β]=ValidationT.FailProjectionT[F, α, β]})#λ, ({type λ[α, β]=ValidationT[F, α, β]})#λ] = new IsoBiFunctorTemplate[({type λ[α, β]=ValidationT.FailProjectionT[F, α, β]})#λ, ({type λ[α, β]=ValidationT[F, α, β]})#λ] {
    def to[E, A](fa: ValidationT.FailProjectionT[F, E, A]): ValidationT[F, E, A] = fa.validationT
    def from[E, A](ga: ValidationT[F, E, A]): ValidationT.FailProjectionT[F, E, A] = ga.fail
  }
}

trait ValidationTInstances3 {
  implicit def validationTFunctor[F[_], E](implicit F0: Functor[F]): ValidationTFunctor[F, E] = new ValidationTFunctor[F, E] {
    implicit def F = F0
  }
  implicit def FailProjectionTFunctor[F[_], E](implicit F0: Functor[F]): IsomorphismFunctor[({type λ[α] = ValidationT.FailProjectionT[F, E, α]})#λ, ({type λ[α] = ValidationT[F, E, α]})#λ] = new IsomorphismFunctor[({type λ[α] = ValidationT.FailProjectionT[F, E, α]})#λ, ({type λ[α] = ValidationT[F, E, α]})#λ] {
    implicit def G = validationTFunctor[F, E]
    def iso = ValidationT.validationTFailProjectionTIso2[F]
  }
}

trait ValidationTInstances2 extends ValidationTInstances3 {
  implicit def validationTPointed[F[_], E](implicit F0: Pointed[F]): ValidationTPointed[F, E] = new ValidationTPointed[F, E] {
    implicit def F = F0
  }
  implicit def validationTFailProjectionTPointed[F[_], E](implicit F0: Pointed[F]): IsomorphismPointed[({type λ[α] = ValidationT.FailProjectionT[F, E, α]})#λ, ({type λ[α] = ValidationT[F, E, α]})#λ] = new IsomorphismPointed[({type λ[α] = ValidationT.FailProjectionT[F, E, α]})#λ, ({type λ[α] = ValidationT[F, E, α]})#λ] {
    implicit def G = validationTPointed[F, E]
    def iso = ValidationT.validationTFailProjectionTIso2[F]
  }
}

trait ValidationTInstances1 extends ValidationTInstances2 {
  implicit def validationTApplicative[F[_], E](implicit F0: Applicative[F]): ValidationTApplicative[F, E] = new ValidationTApplicative[F, E] {
    implicit def F = F0
  }
  implicit def validationTFailProjectionTApplicative[F[_], E](implicit F0: Applicative[F]): IsomorphismApplicative[({type λ[α] = ValidationT.FailProjectionT[F, E, α]})#λ, ({type λ[α] = ValidationT[F, E, α]})#λ] = new IsomorphismApplicative[({type λ[α] = ValidationT.FailProjectionT[F, E, α]})#λ, ({type λ[α] = ValidationT[F, E, α]})#λ] {
    implicit def G = validationTApplicative[F, E]
    def iso = ValidationT.validationTFailProjectionTIso2[F]
  }
}

trait ValidationTInstances0 extends ValidationTInstances1 {
  implicit def validationTBiFunctor[F[_]](implicit F0: Functor[F]): ValidationTBiFunctor[F] = new ValidationTBiFunctor[F] {
    implicit def F = F0
  }
  implicit def validationTFailProjectionTBiFunctor[F[_]](implicit F0: Functor[F]): IsomorphismBiFunctor[({type λ[α, β]=ValidationT.FailProjectionT[F, α, β]})#λ, ({type λ[α, β]=ValidationT[F, α, β]})#λ] = new IsomorphismBiFunctor[({type λ[α, β]=ValidationT.FailProjectionT[F, α, β]})#λ, ({type λ[α, β]=ValidationT[F, α, β]})#λ] {
    implicit def G = validationTBiFunctor[F]
    def iso = ValidationT.validationTFailProjectionTEIso2[F]
  }

  implicit def validationTMonad[F[_], E](implicit F0: Monad[F]): ValidationTMonad[F, E] = new ValidationTMonad[F, E] {
    implicit def F = F0
  }
  implicit def validationTFailProjectionTMonad[F[_], E](implicit F0: Monad[F]): IsomorphismMonad[({type λ[α] = ValidationT.FailProjectionT[F, E, α]})#λ, ({type λ[α] = ValidationT[F, E, α]})#λ] = new IsomorphismMonad[({type λ[α] = ValidationT.FailProjectionT[F, E, α]})#λ, ({type λ[α] = ValidationT[F, E, α]})#λ] {
    implicit def G = validationTMonad[F, E]
    def iso = ValidationT.validationTFailProjectionTIso2[F]
  }
  implicit def validationTFoldable[F[_], E](implicit F0: Foldable[F]): ValidationTFoldable[F, E] = new ValidationTFoldable[F, E] {
    implicit def F = F0
  }
}

trait ValidationTInstances extends ValidationTInstances0 {
  implicit def validationTBiTraverse[F[_]](implicit F0: Traverse[F]): ValidationTBiTraverse[F] = new ValidationTBiTraverse[F] {
    implicit def F = F0
  }
  implicit def validationTFailProjectionTBiTraverse[F[_]](implicit F0: Traverse[F]): IsomorphismBiTraverse[({type λ[α, β] = ValidationT.FailProjectionT[F, α, β]})#λ, ({type λ[α, β] = ValidationT[F, α, β]})#λ] = new IsomorphismBiTraverse[({type λ[α, β] = ValidationT.FailProjectionT[F, α, β]})#λ, ({type λ[α, β] = ValidationT[F, α, β]})#λ] {
    implicit def G = validationTBiTraverse[F]
    def iso = ValidationT.validationTFailProjectionTIso2[F]
  }

  implicit def validationTTraverse[F[_], E](implicit F0: Traverse[F]): ValidationTTraverse[F, E] = new ValidationTTraverse[F, E] {
    implicit def F = F0
  }

  implicit def validationTFailProjectionTTraverse[F[_], E](implicit F0: Traverse[F]): IsomorphismTraverse[({type λ[α]=ValidationT.FailProjectionT[F, E, α]})#λ, ({type λ[α]=ValidationT[F, E, α]})#λ] = new IsomorphismTraverse[({type λ[α]=ValidationT.FailProjectionT[F, E, α]})#λ, ({type λ[α]=ValidationT[F, E, α]})#λ] {
    implicit def G = validationTTraverse[F, E]
    def iso = ValidationT.validationTFailProjectionTEIso2[F, E]
  }

  implicit def validationTMonadTrans[E]: MonadTrans[({type λ[α[_], β] = ValidationT[α, E, β]})#λ] = new ValidationTMonadTrans[E] {}
}

trait ValidationTFunctor[F[_], E] extends Functor[({type λ[α]=ValidationT[F, E, α]})#λ] {
  implicit def F: Functor[F]

  override def map[A, B](fa: ValidationT[F, E, A])(f: (A) => B): ValidationT[F, E, B] = fa map f
}

trait ValidationTPointed[F[_], E] extends Pointed[({type λ[α]=ValidationT[F, E, α]})#λ] with ValidationTFunctor[F, E] {
  implicit def F: Pointed[F]

  def point[A](a: => A): ValidationT[F, E, A] = ValidationT.successT(a)
}

trait ValidationTApplicative[F[_], E] extends Applicative[({type λ[α]=ValidationT[F, E, α]})#λ] with ValidationTPointed[F, E] {
  implicit def F: Applicative[F]
  implicit def E: Semigroup[E]

  override def ap[A, B](fa: => ValidationT[F, E, A])(f: => ValidationT[F, E, (A) => B]): ValidationT[F, E, B] = fa ap f
}

trait ValidationTMonad[F[_], E] extends Monad[({type λ[α]=ValidationT[F, E, α]})#λ] with ValidationTApplicative[F, E] {
  implicit def F: Monad[F]

  def bind[A, B](fa: ValidationT[F, E, A])(f: (A) => ValidationT[F, E, B]): ValidationT[F, E, B] = fa flatMap f
}

trait ValidationTFoldable[F[_], E] extends Foldable.FromFoldr[({type λ[α]=ValidationT[F, E, α]})#λ] {
  implicit def F: Foldable[F]

  override def foldRight[A, B](fa: ValidationT[F, E, A], z: => B)(f: (A, => B) => B): B = fa.foldRight(z)(f)
}

trait ValidationTTraverse[F[_], E] extends Traverse[({type λ[α]=ValidationT[F, E, α]})#λ] with ValidationTFoldable[F, E] {
  implicit def F: Traverse[F]

  def traverseImpl[G[_]: Applicative, A, B](fa: ValidationT[F, E, A])(f: (A) => G[B]): G[ValidationT[F, E, B]] = fa traverse f
}

trait ValidationTBiFunctor[F[_]] extends BiFunctor[({type λ[α, β]=ValidationT[F, α, β]})#λ] {
  implicit def F: Functor[F]

  override def bimap[A, B, C, D](fab: ValidationT[F, A, B])(f: (A) => C, g: (B) => D): ValidationT[F, C, D] = fab.bimap(f, g)
}

trait ValidationTBiTraverse[F[_]] extends BiTraverse[({type λ[α, β] = ValidationT[F, α, β]})#λ] with ValidationTBiFunctor[F] {
  implicit def F: Traverse[F]

  def bitraverse[G[_] : Applicative, A, B, C, D](fab: ValidationT[F, A, B])
                                                (f: (A) => G[C], g: (B) => G[D]): G[ValidationT[F, C, D]] =
    fab.bitraverse(f, g)
}

trait ValidationTMonadTrans[A] extends MonadTrans[({type λ[α[_], β] = ValidationT[α, A, β]})#λ] {
  def hoist[M[_], N[_]](f: M ~> N)(implicit M: Monad[M]) = new (({type λ[α] = ValidationT[M, A, α]})#λ ~> ({type λ[α] = ValidationT[N, A, α]})#λ) {
    def apply[B](mb: ValidationT[M, A, B]): ValidationT[N, A, B] = ValidationT(f.apply(mb.run))
  }

  def liftM[M[_], B](mb: M[B])(implicit M: Monad[M]): ValidationT[M, A, B] = ValidationT(M.map(mb)(Success[A, B](_)))

  implicit def apply[M[_] : Monad]: Monad[({type λ[α] = ValidationT[M, A, α]})#λ] = ValidationT.validationTMonad
}