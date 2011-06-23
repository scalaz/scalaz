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

  implicit def CoKleisliId[F[_]](implicit p: CoPointed[F]): Id[({type λ[α, β] = CoKleisli[α, F, β]})#λ] = new Id[({type λ[α, β] = CoKleisli[α, F, β]})#λ] {
    def id[A] = coKleisli(p.coPoint(_))
  }

  implicit def CoKleisliCompose[F[_]](implicit ex: Extend[F]): Compose[({type λ[α, β] = CoKleisli[α, F, β]})#λ] = new Compose[({type λ[α, β] = CoKleisli[α, F, β]})#λ] {
    def compose[A, B, C](f: CoKleisli[B, F, C], g: CoKleisli[A, F, B]) =
      f =<= g
  }

  implicit def CoKleisliCategory[F[_]](implicit cm: CoMonad[F]): Category[({type λ[α, β] = CoKleisli[α, F, β]})#λ] = {
    implicit val e = cm.extend
    implicit val p = cm.coPointed
    Category.category[({type λ[α, β] = CoKleisli[α, F, β]})#λ]
  }

  implicit def CoKleisliArr[F[_]](implicit p: CoPointed[F]): Arr[({type λ[α, β] = CoKleisli[α, F, β]})#λ] = new Arr[({type λ[α, β] = CoKleisli[α, F, β]})#λ] {
    def arr[A, B](f: A => B) =
      coKleisli(a => f(p.coPoint(a)))
  }

  implicit def CoKleisliFirst[F[_]](implicit ftr: CoPointedFunctor[F]): First[({type λ[α, β] = CoKleisli[α, F, β]})#λ] = new First[({type λ[α, β] = CoKleisli[α, F, β]})#λ] {
    def first[A, B, C](f: CoKleisli[A, F, B]) =
      coKleisli[(A, C), F, (B, C)](w =>
        (f.run(ftr.fmap((ac: (A, C)) => ac._1)(w)), ftr.coPoint(w)._2))
  }

  implicit def CoKleisliArrow[F[_]](implicit cm: CoMonad[F]): Arrow[({type λ[α, β] = CoKleisli[α, F, β]})#λ] = {
    implicit val f = cm.coPointedFunctor
    implicit val p = cm.coPointed
    Arrow.arrow[({type λ[α, β] = CoKleisli[α, F, β]})#λ]
  }
}
