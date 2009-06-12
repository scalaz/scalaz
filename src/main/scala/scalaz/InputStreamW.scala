package scalaz

import java.io.InputStream

sealed trait InputStreamW {
  val value: InputStream

  import Scalaz._

  implicit def elements =
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

object InputStreamW {
  implicit def InputStreamTo(v: InputStream) = new InputStreamW {
    val value = v
  }

  implicit def InputStreamFrom(v: InputStreamW) = v.value
}
