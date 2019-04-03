package scalaz
package syntax

final class ContTOps[M[_], A](private val self: M[A]) extends AnyVal {
  final def cps[R](implicit M: Bind[M]): ContT[R, M, A] =
    ContT((f: A => M[R]) => M.bind(self)(f))

  final def cps_(implicit M: Bind[M]): ContT[Unit, M, A] =
    cps[Unit]
}

trait ToContTOps {
  implicit def ToContTOps[M[_], A](ma: M[A]): ContTOps[M, A] = new ContTOps(ma)
}
