package scalaz
package data

import com.github.ghik.silencer.silent

import scala.{ Any, AnyRef, Int, Null, Some }
import prop.As
import scalaz.tc.{ instanceOf, Comonad, ComonadClass, Monad, MonadClass, Traversable, TraversableClass }

import scala.annotation.tailrec

final class Need[+A] private (private var thunk: Any) {
  def value: A = thunk match {
    case x: Need.Init[_] =>
      val r = x.run()
      thunk = r
      r.asInstanceOf[A]
    case _: Need.Thunk[_] =>
      Need.evalFast(new Need.BufRef, 0, this)
    case _ => thunk.asInstanceOf[A]
  }

  def map[B](f: A => B): Need[B] =
    flatMap(a => Need.now(f(a)))

  def flatMap[B](f: A => Need[B]): Need[B] =
    new Need(new Need.Bind(this, f))

  def flatten[B](implicit ev: A As Need[B]): Need[B] =
    ev.substCv(this).flatMap(x => x)
}
object Need {
  def now[A](a: A): Need[A] = new Need(a.asInstanceOf[AnyRef])

  def apply[A](a: => A): Need[A] = new Need(new Init(() => a))

  def later[A](a: () => A): Need[A] = new Need(new Init(a))

  def suspend[A](a: () => Need[A]): Need[A] =
    new Need(new Bind[Null, A](null, (_: Null) => a()))

  def unapply[A](arg: Need[A]): Some[() => A] =
    Some(() => arg.value)

  private sealed trait Thunk[+A]
  private final class Init[A](val run: () => A)                           extends Thunk[A]
  private final class Bind[A, B](val arg: Need[A], val run: A => Need[B]) extends Thunk[B]

  private type ThunkD[+A] = AnyRef
  private final case class BindL[A](node: Need[A], run: Any => Need[A]) extends ThunkD[A]
  private type BindR[A] = Need[A] // <: ThunkD[A]

  import java.util.{ ArrayList => Q }
  private final class BufRef(var buf: Q[ThunkD[Any]] = null)

  @silent
  private def evalFast[A](ref: BufRef, depth: Int, current: Need[A]): A =
    if (depth >= 512) evalSlow(ref, 0, current)
    else
      current.thunk match {
        case bind: Bind[Any, A] =>
          var arg: Any =
            if (bind.arg != null) evalFast(ref, depth + 1, bind.arg)
            else null

          val next: Need[A] = bind.run(arg)
          arg = null
          current.thunk = next.thunk

          val result: A = evalFast[A](ref, depth + 1, next)
          current.thunk = result
          result

        case init: Init[A] =>
          val result = init.run()
          current.thunk = result
          result

        case r => r.asInstanceOf[A]
      }

  @silent @tailrec private def evalSlow[A, Z](ref: BufRef, count: Int, current: Need[A]): Z =
    current.thunk match {
      case bind: Need.Bind[Any, A] =>
        if (bind.arg != null) {
          if (ref.buf == null) ref.buf = new Q()
          ref.buf.add(BindL(current, bind.run))
          evalSlow(ref, count + 1, bind.arg)
        } else {
          val next = bind.run(null)
          current.thunk = next.thunk
          ref.buf.add(current: BindR[Any])
          evalSlow(ref, count + 1, next)
        }

      case x: Need.Init[Any] =>
        val r = x.run()
        current.thunk = r
        evalSlow(ref, count, current)

      case r =>
        if (count > 0) {
          ref.buf.remove(ref.buf.size() - 1) match {
            case BindL(c, f) =>
              val next = f(r)
              c.thunk = next.thunk
              ref.buf.add(c: BindR[Any])
              evalSlow(ref, count, next)
            case c =>
              val c1 = c.asInstanceOf[BindR[_]]
              c1.thunk = r
              evalSlow(ref, count - 1, c1)
          }
        } else r.asInstanceOf[Z]
    }

  implicit val monad: Monad[Need] = instanceOf(
    new MonadClass[Need] {
      override def pure[A](a: A): Need[A]                               = Need.now(a)
      override def flatMap[A, B](ma: Need[A])(f: A => Need[B]): Need[B] = ma.flatMap(f)
    }
  )

  implicit val traversable: Traversable[Need] = instanceOf(
    new TraversableClass[Need] {
      override def map[A, B](ma: Need[A])(f: A => B): Need[B]                  = ma.map(f)
      override def foldLeft[A, B](fa: Need[A], z: B)(f: (B, A) => B): B        = f(z, fa.value)
      override def foldRight[A, B](fa: Need[A], z: => B)(f: (A, => B) => B): B = f(fa.value, z)
    }
  )

  implicit val comonad: Comonad[Need] = instanceOf(
    new ComonadClass[Need] {
      override def copoint[A](fa: Need[A]): A                          = fa.value
      override def map[A, B](ma: Need[A])(f: A => B): Need[B]          = ma.map(f)
      override def cobind[A, B](fa: Need[A])(f: Need[A] => B): Need[B] = Need(f(fa))
    }
  )
}
