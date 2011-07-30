
package object scalaz {
  // For source compatibility when cross building with Scala 2.8.1
  private [scalaz] object sys {
    def error(m: String) = throw new RuntimeException(m)
  }
}
