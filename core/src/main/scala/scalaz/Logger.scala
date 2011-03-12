package scalaz

trait Logger[Z, L] {
  import Logger._

  def toLog: Z => LOG[L]
  def setLog: LOG[L] => Z => Z
}

import Scalaz._

object Logger {
  type LOG[C] = IndSeq[C]

  implicit def WriterLogger[W, A]: Logger[Writer[LOG[W], A], W] = new Logger[Writer[LOG[W], A], W] {
    def toLog = _.written
    def setLog = l => w => writer(l, w.over)
  }
}
