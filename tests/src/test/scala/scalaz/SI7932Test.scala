package scalaz

/**
 * - [[https://issues.scala-lang.org/browse/SI-7932]]
 * - [[https://github.com/scalaz/scalaz/pull/932]]
 */
object SI7932Test extends SpecLite {

  "SI-7932" in {
    classOf[C].getMethods.map(_.toGenericString)
    true
  }

  trait M[F] {
    type X[a, b] = F
    def category: Category[X] = null
  }

  abstract class C extends M[Float]

}
