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
   * Prepend the given value to the current log.
   */
  def <-+:(e: L): Logger[L, A] =
    withLog(e.η[LOG] |+| _)

  /**
   * Append the given value to the current log.
   */
  def :++->(e: LOG[L]): Logger[L, A] =
    withLog(_ |+| e)

  /**
   * Prepend the given value to the current log.
   */
  def <-++:(e: LOG[L]): Logger[L, A] =
    withLog(e |+| _)

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
}

trait Loggers {
  import Logger._

  def logger[L] = new (Id ~> (({type λ[α]= Logger[L, α]})#λ)) {
    def apply[A](a: A) = new Logger[L, A] {
      val log = ∅[LOG[L]]
      val over = a
    }
  }
}