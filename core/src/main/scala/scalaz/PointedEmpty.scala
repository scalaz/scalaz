package scalaz

trait PointedEmpty[F[_]] {
  val pointed: Pointed[F]
  val empty: Empty[F]

  def point[A](a: => A): F[A] =
    pointed.point(a)

  def e[A]: F[A] =
    empty.empty[A]
}

object PointedEmpty extends PointedEmptys

trait PointedEmptys {
  def pointedEmpty[F[_]](implicit p: Pointed[F], ee: Empty[F]): PointedEmpty[F] = new PointedEmpty[F] {
    val pointed = p
    val empty = ee
  }

  implicit val OptionPointedEmpty: PointedEmpty[Option] =
    pointedEmpty

  implicit val ListPointedEmpty: PointedEmpty[List] =
    pointedEmpty

  implicit val StreamPointedEmpty: PointedEmpty[Stream] =
    pointedEmpty
}