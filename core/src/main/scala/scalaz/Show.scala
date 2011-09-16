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
trait Show[F] extends ShowLike[F]

trait ShowInstance[F] extends Show[F]
