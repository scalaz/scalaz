package scalaz.test

sealed trait Shrink[A] {
  def shrink(a: A): Stream[A]

  def xmap[B](f: A => B, g: B => A) = Shrink.shrink[B](b => shrink(g(b)) map f)
}

object Shrink {
  def shrink[A](f: A => Stream[A]) = new Shrink[A] {
    def shrink(a: A) = f(a)
  }

  import S._

  implicit val LongShrink: Shrink[Long] = shrink(i => if(i == 0L) Stream.empty else {
    val is = Stream.cons(0L, i.iterate[Stream](_ / 2)) takeWhile(_ != 0L) map (i - _)
    if(i < 0L) Stream.cons(-i, is) else is
  })

  implicit val IntShrink: Shrink[Int] = LongShrink xmap (_.toInt, _.toLong)
}