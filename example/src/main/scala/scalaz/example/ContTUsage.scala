package scalaz.example

import scalaz._
import scalaz.syntax.all._

object ContTUsage extends App {

  def syntaxUsage[M[_]: Bind, R, A, B, C](ma: M[A], cmb: ContT[R, M, B], f: (A, B) => M[C]): ContT[R, M, C] = for {
    a <- ma.cps[R]
    b <- cmb
    c <- f(a, b).cps[R]
  } yield c

}
