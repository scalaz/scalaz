package scalaz
package std

import annotation.tailrec

trait VectorInstances0 {
  implicit def vectorEqual[A](implicit A0: Equal[A]) = new VectorEqual[A] {
    implicit def A = A0
  }
}

trait VectorInstances extends VectorInstances0 {
  implicit val vectorInstance = new Traverse[Vector] with MonadPlus[Vector] with Each[Vector] with Index[Vector] with Length[Vector] with ApplicativePlus[Vector] with Zip[Vector] with Unzip[Vector] {
    def each[A](fa: Vector[A])(f: (A) => Unit) = fa foreach f
    def index[A](fa: Vector[A], i: Int) = if (fa.size > i) Some(fa(i)) else None
    def length[A](fa: Vector[A]) = fa.length
    def point[A](a: => A) = scala.Vector(a)
    def bind[A, B](fa: Vector[A])(f: A => Vector[B]) = fa flatMap f
    def empty[A] = scala.Vector()
    def plus[A](a: Vector[A], b: => Vector[A]) = a ++ b
    override def map[A, B](v: Vector[A])(f: A => B) = v map f

    def zip[A, B](a: => Vector[A], b: => Vector[B]) = a zip b
    def unzip[A, B](a: Vector[(A, B)]) = a.unzip

    def traverseImpl[F[_], A, B](v: Vector[A])(f: A => F[B])(implicit F: Applicative[F]) = {
      DList.fromList(v.toList).foldr(F.point(Vector[B]())) {
         (a, fbs) => F.map2(f(a), fbs)(_ +: _)
      }
    }
    
    override def traverseS[S,A,B](v: Vector[A])(f: A => State[S,B]): State[S,Vector[B]] =
      State((s: S) => 
        v.foldLeft((s, Vector[B]()))((acc, a) => {
          val bs = f(a)(acc._1)
          (bs._1, acc._2 :+ bs._2)
        }))

    override def foldRight[A, B](fa: Vector[A], z: => B)(f: (A, => B) => B) = {
      import scala.collection.mutable.ArrayStack
      val s = new ArrayStack[A]
      fa.foreach(a => s += a)
      var r = z
      while (!s.isEmpty) {
        // force and copy the value of r to ensure correctness
        val w = r
        r = f(s.pop, w)
      }
      r
    }

  }

  implicit def vectorMonoid[A]: Monoid[Vector[A]] = new Monoid[Vector[A]] {
    def append(f1: Vector[A], f2: => Vector[A]) = f1 ++ f2
    def zero: Vector[A] = Vector()
  }

  implicit def vectorShow[A: Show]: Show[Vector[A]] = new Show[Vector[A]] {
    def show(as: Vector[A]) =
      (List('[') +: (vector.intersperse(as.map(Show[A].show(_)), List(',')) :+ List(']'))).flatten.toList
  }

  implicit def vectorOrder[A](implicit A0: Order[A]): Order[Vector[A]] = new VectorOrder[A] {
    implicit def A = A0
  }

}

trait VectorFunctions extends IndexedSeqSubFunctions {
  type IxSq[+A] = Vector[A]
  protected def buildIxSq[A, B] = implicitly
  protected def monad = vector.vectorInstance
  protected def empty[A] = Vector()
}

object vector extends VectorInstances with VectorFunctions {
  object vectorSyntax extends scalaz.syntax.std.ToVectorOps
}

trait VectorEqual[A] extends Equal[Vector[A]] {
  implicit def A: Equal[A]

  override def equalIsNatural: Boolean = A.equalIsNatural

  override def equal(a1: Vector[A], a2: Vector[A]) = (a1 corresponds a2)(Equal[A].equal)
}

trait VectorOrder[A] extends Order[Vector[A]] with VectorEqual[A] {
  implicit def A: Order[A]

  import Ordering._

  def order(a1: Vector[A], a2: Vector[A]) =
    (a1, a2) match {
      case (Vector(), Vector()) => EQ
      case (Vector(), y)        => LT
      case (x, Vector())        => GT
      case (as, bs) => Order[A].order(as.head, bs.head) match {
        case EQ => order(as.tail, bs.tail)
        case x  => x
      }
    }

}
