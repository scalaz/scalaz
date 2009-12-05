package scalaz.concurrent

trait Strategy[A] {
  def apply(a: () => A): () => A
}

object Strategy {
  implicit def StrategyFrom[A](f: (() => A) => () => A): Strategy[A] = new Strategy[A] {
    def apply(a: () => A) = f(a)
  }

  implicit def StrategyTo[A](s: Strategy[A]): (() => A) => () => A = (a: () => A) => s(a)
}
