package scalaz
package syntax

final class TheseOps[A](val self: A) extends AnyVal {
  final def wrapThis[B]: A \&/ B =
    \&/.This(self)

  final def `this`[B]: A \&/ B =
    \&/.This(self)

  final def wrapThat[B]: B \&/ A =
    \&/.That(self)

  final def that[B]: B \&/ A =
    \&/.That(self)
}

final class ThesePairOps[A, B](val self: (A, B)) extends AnyVal {
  final def both: A \&/ B =
    \&/.Both(self._1, self._2)
}

trait ToTheseOps {
  implicit def ToTheseOps[A](a: A): TheseOps[A] = new TheseOps(a)
  implicit def ToThesePairOps[A, B](a: (A, B)): ThesePairOps[A, B] = new ThesePairOps(a)
}
