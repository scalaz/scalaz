package scalaz

trait ShowLike[F]  { self =>
  ////
  def show(f: F): List[Char]

  // derived functions

  ////
  val showSyntax = new scalaz.syntax.ShowSyntax[F] {}
}

////
/**
 *
 */
////
trait Show[F] extends ShowLike[F] {
  self  =>


}

object Show {
  def apply[F](implicit F: Show[F]): Show[F] = F

  ////

  ////
}

trait ShowInstance[F] extends Show[F]
