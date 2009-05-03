package scalaz

trait Copointed[C[_]] extends Functor[C] with Copure[C]

object Copointed {
  def copointed[C[_]](implicit t: Functor[C], c: Copure[C]) = new Copointed[C] {
    def fmap[A, B](a: C[A], f: A => B) = t.fmap(a, f)
    def copure[A](a: C[A]): A = c.copure(a)
  }

  implicit val IdentityCopointed = copointed[Identity]

  implicit val NonEmptyListCopointed = copointed[NonEmptyList]

  implicit val Tuple1Copointed = copointed[Tuple1]
  
  implicit def Tuple2Copointed[R] = copointed[PartialApply1Of2[Tuple2, R]#Apply]

  implicit val Function0Copointed = copointed[Function0]

  implicit val ZipperCopointed = copointed[Zipper]

  implicit val TreeCopointed = copointed[Tree]
}
