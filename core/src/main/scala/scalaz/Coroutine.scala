package scalaz

sealed trait Trampoline[A] {
  import Trampoline._
  def map[B](f: A => B): Trampoline[B] =
    flatMap(a => More(() => Return(f(a))))
  def >>=[B](f: A => Trampoline[B]): Trampoline[B] = this match {
    case a >>= g => Trampoline.>>=(a, (x: Any) => Trampoline.>>=(g(x), f))
    case a => Trampoline.>>=(a, f)
  }
  def flatMap[B](f: A => Trampoline[B]): Trampoline[B] =
    this >>= f
  def bounce: Either[Trampoline[A], A] = this match {
    case Return(a) => Right(a)
    case More(t) => Left(t())
    case a >>= f => Left(a.bounce.fold(_ >>= f, f))
  }
  def run: A = {
    import scala.collection.mutable.ArrayStack
    var cur: Trampoline[_] = this
    var stack: ArrayStack[Any => Trampoline[A]] = new ArrayStack()
    var result: Option[A] = None
    while (result.isEmpty) {
      cur match {
        case Return(a) => if(stack.isEmpty) 
                          result = Some(a.asInstanceOf[A]) 
                        else
                          cur = stack.pop().apply(a)
        case More(t) => cur = t()
        case a >>= f => {
          cur = a
          stack push f.asInstanceOf[Any => Trampoline[A]]
        }
      }
    }
    result.get
  }

  /** Interleave two trampolines */
  def zipWith[B, C](tb: Trampoline[B], f: (A, B) => C): Trampoline[C] = {
    def bind(t1: Either[Trampoline[A], A], t2: Either[Trampoline[B], B]) = (t1, t2) match {
      case (Left(a), Left(b)) => a.zipWith(b, f)
      case (Left(a), Right(b)) => a.zipWith(Return(b), f)
      case (Right(a), Left(b)) => Return(a).zipWith(b, f)
      case (Right(a), Right(b)) => Return(f(a, b))
    }
    bind(this.bounce, tb.bounce)
  }

}

object Trampoline {
  def pause: Trampoline[Unit] = More(() => Return(()))

  case class Return[A](a: A) extends Trampoline[A]
  case class More[A](a: () => Trampoline[A]) extends Trampoline[A]
  case class >>=[A, B](a: Trampoline[A],
                       f: A => Trampoline[B]) extends Trampoline[B]
}
