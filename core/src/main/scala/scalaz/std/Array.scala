package scalaz
package std

import scala.reflect.ClassManifest

object array extends ArrayInstances

trait ArrayInstances{

  implicit def arrayEqual[A: Equal]: Equal[Array[A]] =
    new ArrayEqual[A] {
      def A = implicitly
    }

  implicit def arrayOrder[A: Order]: Order[Array[A]] =
    new ArrayOrder[A] {
      def A = implicitly
    }

  implicit def arrayMonoid[A: ClassManifest]: Monoid[Array[A]] =
    new Monoid[Array[A]] {
      val zero = Array.empty[A]
      def append(a: Array[A], b: => Array[A]) = a ++ b
    }

  implicit val arrayInstance: Plus[Array] with Zip[Array] with Foldable[Array] =
    new Plus[Array] with Zip[Array] with Foldable[Array] with Foldable.FromFoldr[Array] {
      def plus[A](a: Array[A], b: => Array[A]) = {
        val clazz = a.getClass().getComponentType().asInstanceOf[Class[A]]
        implicit val manifest = ClassManifest.fromClass[A](clazz)
        a ++ b
      }
      def zip[A, B](a: => Array[A], b: => Array[B]) =
        a zip b
      def foldRight[A, B](fa: Array[A], z: => B)(f: (A, => B) => B) =
        fa.foldRight(z)((a, b) => f(a, b))
      override def foldLeft[A, B](fa: Array[A], z: B)(f: (B, A) => B) =
        fa.foldLeft(z)(f)
      override def index[A](fa: Array[A], i: Int) =
        fa.lift(i)
      override def count[A](fa: Array[A]) =
        fa.length
      override def empty[A](fa: Array[A]) =
        fa.isEmpty
      override def foldLeft1Opt[A](fa: Array[A])(f: (A, A) => A) =
        fa.reduceLeftOption(f)
    }

}

private[scalaz] trait ArrayEqual[A] extends Equal[Array[A]] {
  def A: Equal[A]

  override def equal(a: Array[A], b: Array[A]) = (a corresponds b)(A.equal)
}

private[scalaz] trait ArrayOrder[A] extends Order[Array[A]] with ArrayEqual[A] {
  def A: Order[A]

  def order(a1: Array[A], a2: Array[A]): Ordering = {
    import scalaz.Ordering._
    val i1 = a1.iterator
    val i2 = a2.iterator

    while (i1.hasNext && i2.hasNext) {
      val a1 = i1.next()
      val a2 = i2.next()

      val o = A.order(a1, a2)
      if (o != EQ) {
        return o
      }
    }
    anyVal.booleanInstance.order(i1.hasNext, i2.hasNext)
  }
}
