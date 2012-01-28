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

object ValidationT extends ValidationTFunctions {
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
}