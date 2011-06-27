package scalaz

trait CoKleisli[A, W[_], B] {
  def run: W[A] => B

  import CoKleisli._
  import Identity._

  def *->* : (({type λ[α] = CoKleisli[A, W, α]})#λ *->* B) =
    scalaz.*->*.!**->**![({type λ[α] = CoKleisli[A, W, α]})#λ, B](this)

  def *->*->* : *->*->*[A, ({type λ[α, β] = CoKleisli[α, W, β]})#λ, B] =
    scalaz.*->*->*.!**->**->**![A, ({type λ[α, β] = CoKleisli[α, W, β]})#λ, B](this)

  def contramapValue[C](f: W[C] => W[A]): CoKleisli[C, W, B] =
    coKleisli(run compose f)

  def map[C](f: B => C): CoKleisli[A, W, C] =
    coKleisli(f compose run)

  def redaer(implicit i: Identity[A] =:= W[A]): A => B =
    a => run(id(a))

  def <<=(a: W[A])(implicit e: Extend[W]): W[B] =
    e.functor.fmap(run)(e.coJoin.coJoin(a))

  def =>=[C](c: CoKleisli[B, W, C])(implicit e: Extend[W]): CoKleisli[A, W, C] =
    CoKleisli.coKleisli(w => c run (<<=(w)))

  def =<=[C](c: CoKleisli[C, W, A])(implicit b: Extend[W]): CoKleisli[C, W, B] =
    c =>= this
}

object CoKleisli extends CoKleislis {
  def apply[A, W[_], B](f: W[A] => B): CoKleisli[A, W, B] =
    coKleisli(f)
}

trait CoKleislis {
  type RedaerT[A, F[_], B] = CoKleisli[A, F, B]
  type Redaer[A, B] = CoKleisli[A, Identity, B]

  def coKleisli[A, W[_], B](f: W[A] => B): CoKleisli[A, W, B] = new CoKleisli[A, W, B] {
    def run = f
  }

  def redaer[A, B](r: A => B): Redaer[A, B] =
    coKleisli[A, Identity, B](a => r(a.value))

  def ksa[F[_] : CoPointed, A]: CoKleisli[A, F, A] =
    coKleisli(a => implicitly[CoPointed[F]].coPoint(a))
}
