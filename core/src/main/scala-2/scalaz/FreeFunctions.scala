package scalaz

import scala.annotation.tailrec
import Free._

trait FreeFunctions[S[_], A] { self: Free[S, A] =>

  /**
   * Re-associate any left-nested binds to the right, pull the first suspension to the top
   * and then pass the result to one of the callbacks.
   */
  @tailrec private[scalaz] final def foldStep[B](
    onReturn: A => B,
    onSuspend: S[A] => B,
    onGosub: ((S[X], X => Free[S, A]) forSome { type X }) => B
  ): B = this match {
    case Gosub(fz, f) => fz match {
      case Gosub(fy, g) => fy.flatMap(y => g(y).flatMap(f)).foldStep(onReturn, onSuspend, onGosub)
      case Suspend(sz) => onGosub((sz, f))
      case Return(z) => f(z).foldStep(onReturn, onSuspend, onGosub)
    }
    case Suspend(sa) => onSuspend(sa)
    case Return(a) => onReturn(a)
  }
}
