package scalaz.data

final class EnvT[A, T[_], X](val run: (A, T[X])) extends AnyVal
