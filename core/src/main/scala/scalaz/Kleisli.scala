package scalaz

sealed trait Kleisli[A, F[_], B] {
  val run: A => F[B]

  import Kleisli._
  import StateT._

  def *->* : (({type λ[α] = Kleisli[A, F, α]})#λ *->* B) =
    scalaz.*->*.!**->**![({type λ[α] = Kleisli[A, F, α]})#λ, B](this)

  def *->*->* : *->*->*[A, ({type λ[α, β] = Kleisli[α, F, β]})#λ, B] =
    scalaz.*->*->*.!**->**->**![A, ({type λ[α, β] = Kleisli[α, F, β]})#λ, B](this)

  def mapValue[C](f: F[B] => F[C]): Kleisli[A, F, C] =
    kleisli(f compose run)

  def contramapRead[C](f: C => A): Kleisli[C, F, B] =
    kleisli[C, F, B](run compose f)

  def reader(implicit i: F[B] =:= Ident[B]): A => B =
    a => run(a).value

  def toStateT(implicit ftr: Functor[F]): StateT[A, F, B] =
    stateT(a => ftr.fmap((b: B) => (b, a))(run(a)))

  def toState(implicit i: F[B] =:= Ident[B]): State[A, B] =
    state(a => (run(a).value, a))

  def map[C](f: B => C)(implicit ftr: Functor[F]): Kleisli[A, F, C] =
    kleisli[A, F, C](ftr.fmap(f) compose run)

  def flatMap[C](f: B => Kleisli[A, F, C])(implicit bnd: Bind[F]): Kleisli[A, F, C] =
    kleisli[A, F, C](a => bnd.bind((b: B) => f(b) run a)(run(a)))

  def =<<(a: F[A])(implicit b: Bind[F]): F[B] =
    b.bind(run)(a)

  def >=>[C](c: Kleisli[B, F, C])(implicit b: Bind[F]): Kleisli[A, F, C] =
    kleisli(a => c =<< run(a))

  def <=<[C](c: Kleisli[C, F, A])(implicit b: Bind[F]): Kleisli[C, F, B] =
    c >=> this
}

object Kleisli extends Kleislis {

  def apply[A, F[_], B](r: A => F[B]): Kleisli[A, F, B] =
    kleisli(r)
}

trait Kleislis {
  type ReaderT[A, F[_], B] = Kleisli[A, F, B]
  type Reader[A, B] = Kleisli[A, Ident, B]

  def kleisli[A, F[_], B](r: A => F[B]): Kleisli[A, F, B] = new Kleisli[A, F, B] {
    val run = r
  }

  def reader[A, B](r: A => B): Reader[A, B] =
    kleisli[A, Ident, B](a => Ident.ident(r(a)))

  def ask[F[_] : Pointed, A]: Kleisli[A, F, A] = kleisli(a => implicitly[Pointed[F]].point(a))

  implicit def KleisliFunctor[F[_], R](implicit ftr: Functor[F]): Functor[({type λ[α] = Kleisli[R, F, α]})#λ] =
    new Functor[({type λ[α] = Kleisli[R, F, α]})#λ] {
      def fmap[A, B](f: A => B) =
        _ map f
    }

  implicit def KleisliPointed[F[_], R](implicit p: Pointed[F]): Pointed[({type λ[α] = Kleisli[R, F, α]})#λ] =
    new Pointed[({type λ[α] = Kleisli[R, F, α]})#λ] {
      def point[A](a: => A) =
        kleisli(_ => p.point(a))
    }

  implicit def KleisliPointedFunctor[F[_], R](implicit pt: PointedFunctor[F]): PointedFunctor[({type λ[α] = Kleisli[R, F, α]})#λ] = {
    implicit val p = pt.pointed
    implicit val f = pt.functor
    PointedFunctor.pointedFunctor[({type λ[α] = Kleisli[R, F, α]})#λ]
  }

  implicit def KleisliApplic[F[_], R](implicit ap: Applic[F]): Applic[({type λ[α] = Kleisli[R, F, α]})#λ] = new Applic[({type λ[α] = Kleisli[R, F, α]})#λ] {
    def applic[A, B](f: Kleisli[R, F, A => B]) =
      a => kleisli(r =>
        ap.applic(f.run(r))(a.run(r)))
  }

  implicit def KleisliApplicative[F[_], R](implicit ap: Applicative[F]): Applicative[({type λ[α] = Kleisli[R, F, α]})#λ] = {
    implicit val a = ap.applic
    implicit val p = ap.pointedFunctor
    implicit val f = ap.functor
    Applicative.applicative[({type λ[α] = Kleisli[R, F, α]})#λ]
  }

  implicit def KleisliBind[F[_], R](implicit bd: Bind[F]): Bind[({type λ[α] = Kleisli[R, F, α]})#λ] = new Bind[({type λ[α] = Kleisli[R, F, α]})#λ] {
    def bind[A, B](f: A => Kleisli[R, F, B]) =
      _ flatMap f
  }

  implicit def KleisliJoin[F[_], R](implicit bd: Bind[F]): Join[({type λ[α] = Kleisli[R, F, α]})#λ] = new Join[({type λ[α] = Kleisli[R, F, α]})#λ] {
    def join[A] =
      _ flatMap (z => z)
  }

  implicit def KleisliMonad[F[_], R](implicit m: Monad[F]): Monad[({type λ[α] = Kleisli[R, F, α]})#λ] = {
    implicit val b = m.bind
    implicit val p = m.pointed
    Monad.monadBP[({type λ[α] = Kleisli[R, F, α]})#λ]
  }

  implicit def KleisliId[F[_]](implicit p: Pointed[F]): Id[({type λ[α, β] = Kleisli[α, F, β]})#λ] = new Id[({type λ[α, β] = Kleisli[α, F, β]})#λ] {
    def id[A] = kleisli(p.point(_))
  }

  implicit def KleisliCompose[F[_]](implicit bd: Bind[F]): Compose[({type λ[α, β] = Kleisli[α, F, β]})#λ] = new Compose[({type λ[α, β] = Kleisli[α, F, β]})#λ] {
    def compose[A, B, C](f: Kleisli[B, F, C], g: Kleisli[A, F, B]) =
      f <=< g
  }

  implicit def KleisliCategory[F[_]](implicit md: Monad[F]): Category[({type λ[α, β] = Kleisli[α, F, β]})#λ] = {
    implicit val p = md.pointed
    implicit val b = md.bind
    Category.category[({type λ[α, β] = Kleisli[α, F, β]})#λ]
  }

  implicit def KleisliArr[F[_]](implicit p: Pointed[F]): Arr[({type λ[α, β] = Kleisli[α, F, β]})#λ] = new Arr[({type λ[α, β] = Kleisli[α, F, β]})#λ] {
    def arr[A, B](f: A => B) =
      kleisli(a => p.point(f(a)))
  }

  implicit def KleisliFirst[F[_]](implicit ftr: Functor[F]): First[({type λ[α, β] = Kleisli[α, F, β]})#λ] = new First[({type λ[α, β] = Kleisli[α, F, β]})#λ] {
    def first[A, B, C](f: Kleisli[A, F, B]) =
      kleisli[(A, C), F, (B, C)] {
        case (a, c) => ftr.fmap((b: B) => (b, c))(f.run(a))
      }
  }

  implicit def KleisliArrow[F[_]](implicit md: Monad[F]): Arrow[({type λ[α, β] = Kleisli[α, F, β]})#λ] = {
    implicit val p = md.pointed
    implicit val f = md.functor
    Arrow.arrow[({type λ[α, β] = Kleisli[α, F, β]})#λ]
  }

  implicit def KleisliMonadTrans[T]: MonadTrans[({type λ[α[_], β] = Kleisli[T, α, β]})#λ] = new MonadTrans[({type λ[α[_], β] = Kleisli[T, α, β]})#λ] {
    def lift[G[_] : Monad, A](a: G[A]): Kleisli[T, G, A] =
      kleisli(_ => a)
  }
}
