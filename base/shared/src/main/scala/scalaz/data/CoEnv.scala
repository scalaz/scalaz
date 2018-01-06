package scalaz.data

import scalaz.data.Disjunction.\/

final class CoEnv[E, F[_], A](val run: E \/ F[A]) extends AnyVal
