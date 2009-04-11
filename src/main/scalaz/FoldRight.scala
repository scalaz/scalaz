package scalaz

trait FoldRight[F[_]] {
  def foldRight[A, B](t: F[A], b: B, f: (A, => B) => B): B
}

object FoldRight {
  implicit val IdentityFoldRight = new FoldRight[Identity] {
    def foldRight[A, B](t: Identity[A], b: B, f: (A, => B) => B) = f(t.value, b)
  }
}
