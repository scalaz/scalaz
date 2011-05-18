package scalaz

import java.io.InputStream

sealed trait InputStreamW extends PimpedType[InputStream] {
  import Scalaz._

  implicit def elements: Iterator[Byte] =
    new Iterator[Byte] {
      var i: Int = _
      var b = false
      var h = true

      def next = if(hasNext) {
        b = false
        i.toByte
      } else error("Iterator.next (no more elements)")

      def hasNext = {
        if(b) h
        else if(h) {
          i = value.read
          b = true
          if(i == -1)
            h = false
          h
        } else false
      }
  }

  implicit def stream: Stream[Byte] = {
      val c = value.read
      if(c == -1) Stream.empty
      else Stream.cons(c.toByte, value.stream)
    }
}

trait InputStreams {
  implicit def InputStreamTo(v: InputStream): InputStreamW = new InputStreamW {
    val value = v
  }
}
