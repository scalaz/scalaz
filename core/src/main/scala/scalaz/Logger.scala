package scalaz

// Logger[L, A] is equivalent to WriterT[Identity, IndSeq[L], A]
sealed trait Logger[L, A] {
  import Logger._

  type LOG =
    LOGC[L]

  val value: A

  val log: LOG

  def map[B](f: A => B): Logger[L, B] = new Logger[L, B] {
    val log = Logger.this.log
    val value = f(Logger.this.value)
  }

  def flatMap[B](f: A => Logger[L, B]): Logger[L, B] = {
    val k = f(Logger.this.value)
    new Logger[L, B] {
      val log = Logger.this.log |+| k.log
      val value = k.value
    }
  }

  def foreach(f: A => Unit) =
    f(Logger.this.value)

  import Scalaz._

  def withLog(f: LOG => LOG): Logger[L, A] = new Logger[L, A] {
    val log = f(Logger.this.log)
    val value = Logger.this.value
  }

  def withEachLog(f: L => L) = withLog(_ map f)

  def setLog(l: LOG) = withLog(_ => l)

  def :++->(l: LOG) = withLog(l |+| _)

  def :+->(l: L) = withLog(l.pure[LOGC] |+| _)

  def <-++:(l: LOG) = withLog(_ |+| l)

  def <-+:(l: L) = withLog(_ |+| l.pure[LOGC])

  def resetLog = withLog(_ => ∅[LOG])

  // CAUTION side-effect
  def flush(f: LOG => Unit) = {
    f(log)
    resetLog
  }

  // CAUTION side-effect
  def flushEach(f: L => Unit) =
    flush(_ foreach f)
}

import Scalaz._

object Logger {
  type LOGC[C] = IndSeq[C]

  implicit def LoggerFunctor[L]: Functor[({type λ[α]=Logger[L, α]})#λ] = new Functor[({type λ[α]=Logger[L, α]})#λ] {
    def fmap[A, B](r: Logger[L, A], f: A => B) =
      r map f
  }

  implicit def LoggerPure[L]: Pure[({type λ[α]=Logger[L, α]})#λ] = new Pure[({type λ[α]=Logger[L, α]})#λ] {
    def pure[A](a: => A) =
      logger[L](a)
  }

  implicit def LoggerApply[L]: Apply[({type λ[α]=Logger[L, α]})#λ] = new Apply[({type λ[α]=Logger[L, α]})#λ] {
    def apply[A, B](f: Logger[L, A => B], a: Logger[L, A]) =
      for {
        ff <- f
        aa <- a
      } yield ff(aa)
  }

  implicit def LoggerBind[L]: Bind[({type λ[α]=Logger[L, α]})#λ] = new Bind[({type λ[α]=Logger[L, α]})#λ] {
    def bind[A, B](a: Logger[L, A], f: A => Logger[L, B]) =
      a flatMap f
  }

  implicit def LoggerEach[L]: Each[({type λ[α]=Logger[L, α]})#λ] = new Each[({type λ[α]=Logger[L, α]})#λ] {
    def each[A](a: Logger[L, A], f: A => Unit) =
      a foreach f
  }

  implicit def LoggerIndex[L]: Index[({type λ[α]=Logger[L, α]})#λ] = new Index[({type λ[α]=Logger[L, α]})#λ] {
    def index[A](a: Logger[L, A], n: Int) =
      if(n== 0) Some(a.value) else None
  }

  implicit def LoggerFoldable[L]: Foldable[({type λ[α]=Logger[L, α]})#λ] = new Foldable[({type λ[α]=Logger[L, α]})#λ] {
    override def foldRight[A, B](t: Logger[L, A], b: => B, f: (A, => B) => B) =
      f(t.value, b)
  }

  implicit def LoggerTraverse[L]: Traverse[({type λ[α]=Logger[L, α]})#λ] = new Traverse[({type λ[α]=Logger[L, α]})#λ] {
    def traverse[F[_] : Applicative, A, B](f: A => F[B], t: Logger[L, A]) = {
      f(t.value) ∘ (b => logger(b) setLog t.log)
    }
  }

  implicit def LoggerShow[L, A : Show]: Show[Logger[L, A]] = new Show[Logger[L, A]] {
    def show(a: Logger[L, A]) =
      ("logger(" + a.value + ")").toList
  }

  implicit def LoggerEqual[L, A: Equal]: Equal[Logger[L, A]] = new Equal[Logger[L, A]] {
    def equal(a1: Logger[L, A], a2: Logger[L, A]) =
      a1.value === a2.value
  }

  implicit def LoggerOrder[L, A: Order]: Order[Logger[L, A]] = new Order[Logger[L, A]] {
    def order(a1: Logger[L, A], a2: Logger[L, A]) =
      a1.value ?|? a2.value
  }

  implicit def LoggerZero[L, A: Zero]: Zero[Logger[L, A]] = zero(logger[L](∅[A]))
}

trait Loggers {
  def logger[L] = new (Id ~> (({type λ[α]=Logger[L, α]})#λ)) {
    def apply[A](v: A) = new Logger[L, A] {
      val log = ∅[LOG]
      val value = v
    }
  }
}