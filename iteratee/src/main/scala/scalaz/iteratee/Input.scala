package scalaz
package iteratee

import LazyOption._
import Iteratee._

/**The input to an iteratee. **/
sealed trait Input[E] {
  
  def fold[Z](empty: => Z, el: (=> E) => Z, eof: => Z): Z

  def apply[Z](empty: => Z, el: (=> E) => Z, eof: => Z) =
    fold(empty, el, eof)

  def el: LazyOption[E] =
    apply(lazyNone[E], lazySome(_), lazyNone[E])

  def elOr(e: => E) =
    el.getOrElse(e)

  def isEmpty: Boolean =
    apply(true, _ => false, false)

  def isEl: Boolean =
    apply(false, _ => true, false)

  def isEof: Boolean =
    apply(false, _ => false, true)

  def map[X](f: (=> E) => X): Input[X] =
    fold(emptyInput, e => elInput(f(e)), eofInput)

  def flatMap[X](f: (=> E) => Input[X]): Input[X] =
    fold(emptyInput, e => f(e), eofInput)

  def filter(f: (=> E) => Boolean): Input[E] = 
    fold(emptyInput, e => if (f(e)) this else emptyInput, eofInput)

  def foreach(f: (=> E) => Unit) =
    fold((), e => f(e), ())

  def forall(p: (=> E) => Boolean): Boolean =
    fold(true, p, true)

  def exists(p: (=> E) => Boolean): Boolean =
    fold(false, p, false)

  abstract override def toString = fold("Empty", el => el.toString, "EOF")
}

object Input extends InputFunctions with InputInstances {
  def apply[E](e: => E): Input[E] =
    elInput(e)

  object Empty {
    def apply[E]: Input[E] = new Input[E] {
      def fold[Z](empty: => Z, el: (=> E) => Z, eof: => Z) = empty
    }

    def unapply[E](i: Input[E]): Boolean = i.fold(true, _ => false, false)
  }


  object Element {
    def apply[E](e: => E): Input[E] = new Input[E] {
      def fold[Z](empty: => Z, el: (=> E) => Z, eof: => Z) = el(e)
    }

    def unapply[E](i: Input[E]): Option[E] = i.fold(None, Some(_), None)
  }

  object Eof {
    def apply[E]: Input[E] = new Input[E] {
      def fold[Z](empty: => Z, el: (=> E) => Z, eof: => Z) = eof
    }

    def unapply[E](i: Input[E]): Boolean = i.fold(false, _ => false, true)
  }

}

trait InputInstances {
  import Input._

  implicit val input = new Traverse[Input] with MonadPlus[Input] with Each[Input] with Length[Input] {
     def length[A](fa: Input[A]): Int = fa.fold(
       empty = 0
       , el = _ => 1
       , eof = 0
     )
     def point[A](a: => A): Input[A] = elInput(a)
     def traverseImpl[G[_]: Applicative, A, B](fa: Input[A])(f: (A) => G[B]): G[Input[B]] = fa.fold(
       empty = Applicative[G].point(emptyInput[B])
       , el = x => Applicative[G].map(f(x))(b => elInput(b))
       , eof = Applicative[G].point(eofInput[B])
     )
    override def foldRight[A, B](fa: Input[A], z: => B)(f: (A, => B) => B): B = fa.fold(
       empty = z
       , el = a => f(a, z)
       , eof = z
     )
     def each[A](fa: Input[A])(f: (A) => Unit) = fa foreach (a => f(a))
     def plus[A](a: Input[A], b: => Input[A]): Input[A] = a.fold(
       empty = b
       , el = _ => a
       , eof = b
     )
     def bind[A, B](fa: Input[A])(f: (A) => Input[B]): Input[B] = fa flatMap (a => f(a))
     def empty[A]: Input[A] = emptyInput
   }

   implicit def inputMonoid[A](implicit A: Monoid[A]) = new Monoid[Input[A]] {
     def append(a1: Input[A], a2: => Input[A]): Input[A] = a1.fold(
       empty = a2.fold(
         empty = emptyInput
         , el = elInput
         , eof = eofInput
       )
       , el = xa => a2.fold(
         empty = elInput(xa)
         , el = ya => elInput(A.append(xa, ya))
         , eof = eofInput
       )
       , eof = eofInput
     )

     def zero: Input[A] = emptyInput
   }

   implicit def inputEqual[A](implicit A: Equal[A]) = new Equal[Input[A]] {
     def equal(a1: Input[A], a2: Input[A]): Boolean = a1.fold(
       empty = a2.isEmpty
       , el = a => a2.exists(z => A.equal(a, z))
       , eof = a2.isEmpty
     )
   }

   implicit def inputShow[A](implicit A: Show[A]) = new Show[Input[A]] {
     def show(f: Input[A]): List[Char] = f.fold(
       empty = "empty-input"
       , el = a => "el-input(" + A.shows(a) + ")"
       , eof = "eof-input"
     ).toList
   }
}

trait InputFunctions {
  def emptyInput[E]: Input[E] = Input.Empty[E]
  def elInput[E](e: => E): Input[E] = Input.Element(e)
  def eofInput[E]: Input[E] = Input.Eof[E]
}
