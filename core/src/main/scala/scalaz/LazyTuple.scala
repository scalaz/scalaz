package scalaz

trait LazyTuple2[A,B] {
  def _1: A
  def _2: B
}

trait LazyTuple3[A,B,C] {
  def _1: A
  def _2: B
  def _3: C
}

trait LazyTuples {
  type <&>[A,B] = LazyTuple2[A,B]

  def lazyTuple[A,B](a: => A, b: => B) = new (A <&> B) {
    def _1 = a
    def _2 = b
  }

  def lazyTuple[A,B,C](a: => A, b: => B, c: => C) = new LazyTuple3[A,B,C] {
    def _1 = a
    def _2 = b
    def _3 = c
  }
}
