package scalaz

import reflect.ClassTag

import org.scalacheck._

abstract class SpecLite extends Properties("") with SpecLiteFunctions1 {
  override val name = this.getClass.getName.stripSuffix("$")

  def checkAll(name: String, props: Properties): Unit = {
    for ((name2, prop) <- props.properties) yield {
      property(name + ":" + name2) = prop
    }
  }

  def checkAll(props: Properties): Unit = {
    for ((name, prop) <- props.properties) yield {
      property(name) = prop
    }
  }

  implicit protected[this] class PropertyOps(props: Properties) {
    def withProp(propName: String, prop: Prop): Properties = {
      val p = new Properties(props.name)
      for {(name, x) <- props.properties} p.property(name) = x
      p.property(propName) = prop
      p
    }
  }

  private var context: String = ""

  implicit protected[this] class StringOps(s: String) {
    infix def should[A](a: => Any): Unit = {
      val saved = context
      context = s; try a finally context = saved
    }
    def ![A](a: => A)(implicit ev: (A) => Prop): Unit = in(a)

    infix def in[A](a: => A)(implicit ev: (A) => Prop): Unit = property(context + ":" + s) = Prop { prms =>
      ev(a).apply(prms) // TODO sort out the laziness / implicit conversions properly
    }
  }

  def check(x: => Boolean): Prop = Prop.secure(x)

  def fail(msg: String): Nothing = throw new AssertionError(msg)
  class AnyOps[A](actual: => A) {
    def must_===(expected: A)(implicit show: Show[A], equal: Equal[A]): Unit = {
      val act = actual
      def test = Equal[A].equal(expected, act)
      def koMessage = s"${Show[A].shows(act)} !== ${Show[A].shows(expected)}"
      if (!test)
        fail(koMessage)
    }
    def must_==(expected: A): Unit = {
      val act = actual
      def test = expected == act
      def koMessage = s"$act !== $expected"
      if (!test)
        fail(koMessage)
    }

    def mustMatch(f: PartialFunction[A, Boolean]): Unit = {
      val act = actual
      def test = f.isDefinedAt(act) && f(act)
      def koMessage = s"$act does not satisfy partial function"
      if (!test)
        fail(koMessage)
    }

    infix def and[B](b: => B): B = {
      actual
      b
    }

    def mustBe_<(x: Int)(implicit ev: A <:< Int) = {
      val act = actual
      def test = ev(act) < x
      def koMessage = s"$actual <! $x"
      if (!test)
        fail(koMessage)
    }

    def mustThrowA[T <: Throwable](implicit man: ClassTag[T]): Unit = {
      val erasedClass = man.runtimeClass
      try {
        actual
        fail("no exception thrown, expected " + erasedClass)
      } catch {
        case ex: Throwable =>
          if (!erasedClass.isInstance(ex))
            fail("wrong exception thrown, expected: " + erasedClass + " got: " + ex)
      }
    }
  }
  implicit protected[this] def enrichAny[A](actual: => A): AnyOps[A] = new AnyOps(actual)

  def prop[T, R](result: T => R)(implicit toProp: (=>R) => Prop, a: Arbitrary[T], s: Shrink[T]): Prop = check1(result)
  implicit protected[this] def propToProp(p: => Prop): Prop = p
  implicit protected[this] def check1[T, R](result: T => R)(implicit toProp: (=>R) => Prop, a: Arbitrary[T], s: Shrink[T]): Prop = Prop.forAll((t: T) => toProp(result(t)))
  implicit protected[this] def unitToProp(u: => Unit): Prop = booleanToProp({u; true})
  implicit protected[this] def booleanToProp(b: => Boolean): Prop = Prop.secure(b)

}

sealed trait SpecLiteFunctions1 { self: SpecLite =>
  implicit protected[this] def unitToProp2(u: Unit): Prop = booleanToProp(true)
}
