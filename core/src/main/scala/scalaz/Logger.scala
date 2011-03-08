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
}

trait Loggers {
  def logger[L] = new (Id ~> (({type λ[α]=Logger[L, α]})#λ)) {
    def apply[A](v: A) = new Logger[L, A] {
      val log = ∅[LOG]
      val value = v
    }
  }
}