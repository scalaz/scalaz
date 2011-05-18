package scalaz

trait Id[F[_, _]] {
  def id[A]: F[A, A]
}

object Id extends Ids

trait Ids {

  implicit val Function1Id: Id[Function1] = new Id[Function1] {
    def id[A] = a => a
  }

  implicit val PartialFunctionId: Id[PartialFunction] = new Id[PartialFunction] {
    def id[A] = {
      case a => a
    }
  }

  implicit val `<:<_Id` : Id[<:<] = new Id[<:<] {
    def id[A] = implicitly[A <:< A]
  }

  implicit val `=:=_Id` : Id[=:=] = new Id[=:=] {
    def id[A] = implicitly[A =:= A]
  }

}
