package scalaz.data

import scalaz.control.Inf

final class EnvT[A, T[_], X](val run: (Inf[A], Inf[T[X]])) extends AnyVal
