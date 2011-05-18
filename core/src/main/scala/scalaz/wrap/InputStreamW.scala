package scalaz
package wrap

import java.io.InputStream

sealed trait InputStreamW {
  val value: InputStream

  import InputStreamW._

  def elements: Iterator[Byte] =
    new Iterator[Byte] {
      var i: Int = _
      var b = false
      var h = true

      def next = if (hasNext) {
        b = false
        i.toByte
      } else error("Iterator.next (no more elements)")

      def hasNext = {
        if (b) h
        else if (h) {
          i = value.read
          b = true
          if (i == -1)
            h = false
          h
        } else false
      }
    }

  def stream: Stream[Byte] = {
    val c = value.read
    if (c == -1) Stream.empty
    else Stream.cons(c.toByte, value.stream)
  }

  def list: List[Byte] = {
    val c = value.read
    if (c == -1) Nil
    else c.toByte :: value.list
  }
}

object InputStreamW extends InputStreamWs

trait InputStreamWs {
  implicit def InputStreamTo(v: InputStream): InputStreamW = new InputStreamW {
    val value = v
  }
}
