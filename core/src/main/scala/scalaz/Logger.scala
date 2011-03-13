package scalaz

import Scalaz._

trait Logger[L, A] extends NewType[Writer[IndSeq[L], A]] {
  import Logger._

  val log: LOG[L]
  val over: A

  val value = writer(log, over)

  def map[B](f: A => B): Logger[L, B] = new Logger[L, B] {
    val log = Logger.this.log
    val over = f(Logger.this.over)
  }

  def flatMap[B](f: A => Logger[L, B]): Logger[L, B] = {
    val l = f(over)
    new Logger[L, B] {
      val log = Logger.this.log |+| l.log
      val over = l.over
    }
  }

  def foreach(f: A => Unit) =
    f(over)

  /**
   * Transform the log by the given function.
   */
  def withLog(k: LOG[L] => LOG[L]): Logger[L, A] = new Logger[L, A] {
    val log = k(Logger.this.log)
    val over = Logger.this.over
  }

  /**
   * Transform each log value by the given function.
   */
  def withEachLog(k: L => L): Logger[L, A] =
    withLog(_ ∘ k)

  /**
   * Set the log to the given value, losing any previous value.
   */
  def setLog(l: LOG[L]): Logger[L, A] =
    withLog(_ => l)

  /**
   * Append the given value to the current log.
   */
  def :+->(e: L): Logger[L, A] =
    withLog(_ |+| e.η[LOG])

  /**
   * Append the given value to the current log by applying to the underlying value.
   */
  def ::->(e: A => L): Logger[L, A] =
    :+->(e(over))

  /**
   * Prepend the given value to the current log.
   */
  def <-+:(e: L): Logger[L, A] =
    withLog(e.η[LOG] |+| _)

  /**
   * Prepend the given value to the current log by applying to the underlying value.
   */
  def <-::(e: A => L): Logger[L, A] =
    <-+:(e(over))

  /**
   * Append the given value to the current log.
   */
  def :++->(e: LOG[L]): Logger[L, A] =
    withLog(_ |+| e)

  /**
   * Append the given value to the current log by applying to the underlying value.
   */
  def ::+->(e: A => LOG[L]): Logger[L, A] =
    withLog(_ |+| e(over))

  /**
   * Prepend the given value to the current log.
   */
  def <-++:(e: LOG[L]): Logger[L, A] =
    withLog(e |+| _)

  /**
   * Prepend the given value to the current log by applying to the underlying value.
   */
  def <-+::(e: A => LOG[L]): Logger[L, A] =
    <-++:(e(over))

  /**
   * Set the log to be empty.
   */
  def resetLog: Logger[L, A] =
    withLog(_ => ∅[LOG[L]])

  /**
   * Runs the given side-effect on the log, then returns this underlying value. '''CAUTION: side-effect'''
   */
  def effectLog(k: LOG[L] => Unit): Logger[L, A] = {
    k(log)
    this
  }

  /**
   * Runs the given side-effect on each element of the log, then returns this underlying value. '''CAUTION: side-effect'''
   */
  def effectEachLog(k: L => Unit): Logger[L, A] =
    effectLog(_ ∘ k)

  /**
   * Runs the given side-effect on the log, then returns this underlying value with an empty log. '''CAUTION: side-effect'''
   */
  def flushLog(k: LOG[L] => Unit): Logger[L, A] = {
    effectLog(k)
    resetLog
  }

  /**
   * Runs the given side-effect on each element of the log, then returns this underlying value with an empty log. '''CAUTION: side-effect'''
   */
  def flushEachLog(k: L => Unit): Logger[L, A] =
    flushLog(_ ∘ k)

  /**
   * Prints the log, then returns this underlying value. '''CAUTION: side-effect'''
   */
  def printLog(implicit s: Show[L]): Logger[L, A] =
    effectLog(_.println)

  /**
   * Prints each element of the log, then returns this underlying value. '''CAUTION: side-effect'''
   */
  def printEachLog(implicit s: Show[L]): Logger[L, A] =
    effectEachLog(_.println)

  /**
   * Prints the log, then returns this underlying value with an empty log. '''CAUTION: side-effect'''
   */
  def printFlushLog(implicit s: Show[L]): Logger[L, A] =
    flushLog(_.println)

  /**
   * Prints each element of the log, then returns this underlying value with an empty log. '''CAUTION: side-effect'''
   */
  def printFlushEachLog(implicit s: Show[L]): Logger[L, A] =
    flushEachLog(_.println)
}

object Logger {
  type LOG[C] = IndSeq[C]

  implicit def LoggerInjective[L] = Injective[({type λ[α]= Logger[L, α]})#λ]

  implicit def LoggerPure[L]: Pure[({type λ[α]= Logger[L, α]})#λ] = new Pure[({type λ[α]=Logger[L, α]})#λ] {
    def pure[A](a: => A) = new Logger[L, A] {
      val log = ∅[LOG[L]]
      val over = a
    }
  }

  implicit def LoggerFunctor[L]: Functor[({type λ[α]=Logger[L, α]})#λ] = new Functor[({type λ[α]= Logger[L, α]})#λ] {
    def fmap[A, B](x: Logger[L, A], f: A => B) =
      x map f
  }

  implicit def LoggerApply[L]: Apply[({type λ[α]=Logger[L, α]})#λ] = new Apply[({type λ[α]=Logger[L, α]})#λ] {
    def apply[A, B](f: Logger[L, A => B], a: Logger[L, A]): Logger[L, B] = {
      val w1 = f.value
      val w2 = a.value

      new Logger[L, B] {
        val log = w1.written |+| w2.written
        val over = w1.over(w2.over)
      }
    }
  }

  implicit def LoggerBind[L]: Bind[({type λ[α]=Logger[L, α]})#λ] = new Bind[({type λ[α]=Logger[L, α]})#λ] {
    def bind[A, B](a: Logger[L, A], f: A => Logger[L, B]) =
      a flatMap f
  }

  implicit def LoggerEach[L]: Each[({type λ[α]=Logger[L, α]})#λ] = new Each[({type λ[α]= Logger[L, α]})#λ] {
    def each[A](x: Logger[L, A], f: A => Unit) =
      x foreach f
  }

  implicit def LoggerIndex[L]: Index[({type λ[α]=Logger[L, α]})#λ] = new Index[({type λ[α]=Logger[L, α]})#λ] {
    def index[A](a: Logger[L, A], n: Int) =
      if(n == 0) Some(a.over) else None
  }

  implicit def LoggerFoldable[L]: Foldable[({type λ[α]=Logger[L, α]})#λ] = new Foldable[({type λ[α]=Logger[L, α]})#λ] {
    override def foldRight[A, B](t: Logger[L, A], b: => B, f: (A, => B) => B) =
      f(t.over, b)
  }

  implicit def LoggerTraverse[L]: Traverse[({type λ[α]=Logger[L, α]})#λ] = new Traverse[({type λ[α]=Logger[L, α]})#λ] {
    def traverse[F[_] : Applicative, A, B](f: A => F[B], t: Logger[L, A]) =
      f(t.over) ∘ (b => new Logger[L, B] {
        val log = t.log
        val over = b
      })
  }

  implicit def LoggerShow[L : Show, A : Show]: Show[Logger[L, A]] = new Show[Logger[L, A]] {
    def show(a: Logger[L, A]) =
      ("Logger(" + a.log.shows + "," + a.over.shows + ")").toList
  }

  implicit def LoggerEqual[L, A: Equal]: Equal[Logger[L, A]] = new Equal[Logger[L, A]] {
    def equal(a1: Logger[L, A], a2: Logger[L, A]) =
      a1.over === a2.over
  }

  implicit def LoggerOrder[L, A: Order]: Order[Logger[L, A]] = new Order[Logger[L, A]] {
    def order(a1: Logger[L, A], a2: Logger[L, A]) =
      a1.over ?|? a2.over
  }

  implicit def LoggerZero[L, A: Zero]: Zero[Logger[L, A]] = new Zero[Logger[L, A]] {
    val zero = new Logger[L, A] {
      val log = ∅[LOG[L]]
      val over = ∅[A]
    }
  }
}

trait Loggers {
  import Logger._

  def mkLogger[L] = new (Id ~> (({type λ[α]= Logger[L, α]})#λ)) {
    def apply[A](a: A) = new Logger[L, A] {
      val log = ∅[LOG[L]]
      val over = a
    }
  }
}