package scalaz

sealed trait LazyTuple2[A, B] {
  def _1: A

  def _2: B
}

sealed trait LazyTuple3[A, B, C] {
  def _1: A

  def _2: B

  def _3: C
}

sealed trait LazyTuple4[A, B, C, D] {
  def _1: A

  def _2: B

  def _3: C

  def _4: D
}

object LazyTuple extends LazyTuples

trait LazyTuples {
  type :&:[A, B] = LazyTuple2[A, B]

  def lazyTuple2[A, B](a: => A, b: => B): (A :&: B) = new (A :&: B) {
    def _1 = a

    def _2 = b
  }

  def lazyTuple3[A, B, C](a: => A, b: => B, c: => C): LazyTuple3[A, B, C] = new LazyTuple3[A, B, C] {
    def _1 = a

    def _2 = b

    def _3 = c
  }

  def lazyTuple4[A, B, C, D](a: => A, b: => B, c: => C, d: => D): LazyTuple4[A, B, C, D] = new LazyTuple4[A, B, C, D] {
    def _1 = a

    def _2 = b

    def _3 = c

    def _4 = d
  }
}
