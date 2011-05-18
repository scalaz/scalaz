package scalaz
package data

sealed trait WriterT[W, F[_], A] {
  val runT: F[(W, A)]

  import WriterT._

  def *->* : (({type λ[α] = WriterT[W, F, α]})#λ *->* A) =
    data.*->*.**->**[({type λ[α] = WriterT[W, F, α]})#λ, A](this)

  def *->*->* : *->*->*[W, ({type λ[α, β] = WriterT[α, F, β]})#λ, A] =
    data.*->*->*.**->**->**[W, ({type λ[α, β] = WriterT[α, F, β]})#λ, A](this)

  def run(implicit i: F[(W, A)] =:= Ident[(W, A)]): (W, A) =
    runT.value

  def mapValue[X, B](f: ((W, A)) => (X, B))(implicit ftr: Functor[F]): WriterT[X, F, B] =
    writerT(ftr.fmap(f)(runT))

  def mapWritten[X](f: W => X)(implicit ftr: Functor[F]): WriterT[X, F, A] =
    mapValue(wa => (f(wa._1), wa._2))

  def writtenT(implicit ftr: Functor[F]): F[W] =
    ftr.fmap((_: (W, A))._1)(runT)

  def written(implicit i: F[(W, A)] =:= Ident[(W, A)]): W =
    run._1

  def overT(implicit ftr: Functor[F]): F[A] =
    ftr.fmap((_: (W, A))._2)(runT)

  def over(implicit i: F[(W, A)] =:= Ident[(W, A)]): A =
    run._2

  def swapT(implicit ftr: Functor[F]): WriterT[A, F, W] =
    mapValue(wa => (wa._2, wa._1))

  def swap(implicit i: F[(W, A)] =:= Ident[(W, A)]): Writer[A, W] = {
    val (w, a) = run
    writer((a, w))
  }

  def :++>(w: => W)(implicit f: Functor[F], s: Semigroup[W]): WriterT[W, F, A] =
    mapWritten(s.append(_, w))

  def :++>>(f: A => W)(implicit ftr: Functor[F], s: Semigroup[W]): WriterT[W, F, A] =
    mapValue(wa => (s.append(wa._1, f(wa._2)), wa._2))

  def <++:(w: => W)(implicit f: Functor[F], s: Semigroup[W]): WriterT[W, F, A] =
    mapWritten(s.append(w, _))

  def <<++:(f: A => W)(implicit ftr: Functor[F], s: Semigroup[W]): WriterT[W, F, A] =
    mapValue(wa => (s.append(f(wa._2), wa._1), wa._2))

  def reset(implicit z: Zero[W], f: Functor[F]): WriterT[W, F, A] =
    mapWritten(_ => z.zero)

  def map[B](f: A => B)(implicit ftr: Functor[F]): WriterT[W, F, B] =
    writerT(ftr.fmap((wa: (W, A)) => (wa._1, f(wa._2)))(runT))

  def foreach[B](f: A => Unit)(implicit e: Each[F]): Unit =
    e.each((wa: (W, A)) => f(wa._2))(runT)

  def flatMap[B](f: A => WriterT[W, F, B])(implicit b: BindFunctor[F], s: Semigroup[W]): WriterT[W, F, B] =
    writerT(b.bind((wa: (W, A)) => {
      val z = f(wa._2).runT
      b.fmap((wb: (W, B)) => (s.append(wa._1, wb._1), wb._2))(z)
    })(runT))
}

object WriterT extends WriterTs {
  def apply[W, F[_], A](v: F[(W, A)]): WriterT[W, F, A] =
    writerT(v)
}

trait WriterTs {
  type Writer[W, A] = WriterT[W, Ident, A]

  def writerT[W, F[_], A](v: F[(W, A)]): WriterT[W, F, A] = new WriterT[W, F, A] {
    val runT = v
  }

  def writer[W, A](v: (W, A)): Writer[W, A] =
    writerT(Ident.ident(v))

  implicit def WriterTMonadTrans[W: Zero]: MonadTrans[({type λ[α[_], β] = WriterT[W, α, β]})#λ] = new MonadTrans[({type λ[α[_], β] = WriterT[W, α, β]})#λ] {
    def lift[G[_] : Monad, A](a: G[A]): WriterT[W, G, A] =
      writerT(implicitly[Monad[G]].fmap((a: A) => (implicitly[Zero[W]].zero, a))(a))
  }
}