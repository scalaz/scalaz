package scalaz

sealed trait Kleisli[A, F[_], B] {
  val run: A => F[B]

  import Kleisli._
  import StateT._
  import =~~=._

  def *->* : (({type λ[α] = Kleisli[A, F, α]})#λ *->* B) =
    scalaz.*->*.!**->**![({type λ[α] = Kleisli[A, F, α]})#λ, B](this)

  def *->*->* : *->*->*[A, ({type λ[α, β] = Kleisli[α, F, β]})#λ, B] =
    scalaz.*->*->*.!**->**->**![A, ({type λ[α, β] = Kleisli[α, F, β]})#λ, B](this)

  def mapValue[C](f: F[B] => F[C]): Kleisli[A, F, C] =
    kleisli(f compose run)

  def contramapRead[C](f: C => A): Kleisli[C, F, B] =
    kleisli[C, F, B](run compose f)

  def reader(a: A)(implicit i: F =~~= Identity): B =
    run(a)

  def toStateT(implicit ftr: Functor[F]): StateT[A, F, B] =
    stateT(a => ftr.fmap((b: B) => (b, a))(run(a)))

  def toState(implicit i: F =~~= Identity): State[A, B] =
    state(a => (run(a), a))

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

  def rws[W, S](implicit ftr: Functor[F], z: Zero[W]): ReaderWriterStateT[A, W, S, F, B] =
    ReaderWriterStateT.readerWriterStateT(r => s =>
      implicitly[Functor[F]].fmap((b: B) => (b, s, z.zero))(run(r)))

  def |(b: => B)(implicit i: F =~~= Option): Reader[A, B] =
    Kleisli.reader(a => i ~~=> run(a) getOrElse b)
}

object Kleisli extends Kleislis {

  def apply[A, F[_], B](r: A => F[B]): Kleisli[A, F, B] =
    kleisli(r)
}

trait Kleislis {
  type ReaderT[A, F[_], B] = Kleisli[A, F, B]
  type Reader[A, B] = Kleisli[A, Identity, B]

  def kleisli[A, F[_], B](r: A => F[B]): Kleisli[A, F, B] = new Kleisli[A, F, B] {
    val run = r
  }

  def reader[A, B](r: A => B): Reader[A, B] =
    kleisli[A, Identity, B](a => Identity.id(r(a)))

  def ask[F[_] : Pointed, A]: Kleisli[A, F, A] = kleisli(a => implicitly[Pointed[F]].point(a))
}
