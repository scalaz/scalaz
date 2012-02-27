package scalaz

import CoStateT._

sealed trait PLens[A, B] {
  import PLens._
  import CoStateT._

  def run(a: A): Option[CoState[B, A]]

  def compose[C](that: PLens[C, A]): PLens[C, B] =
    plens(a => for {
      s <- that run a
      t <- run(s.pos)
    } yield coState(x => s.put(t.put(x)), t.pos))

}

object PLens extends PLensFunctions with PLensInstances {
  def apply[A, B](r: A => Option[CoState[B, A]]): PLens[A, B] =
    plens(r)
}

trait PLensInstances {
  import PLens._

  implicit def plensCategory: Category[PLens] = new Category[PLens] {
    def compose[A, B, C](f: PLens[B, C], g: PLens[A, B]): PLens[A, C] =
      f compose g
    def id[A]: PLens[A, A] =
      plensId[A]
  }
}

trait PLensFunctions {

  import CoStateT._

  /** The eye-patch operator, an alias for `PLens` */
  type @-?[A, B] =
  PLens[A, B]

  def plens[A, B](r: A => Option[CoState[B, A]]): PLens[A, B] = new PLens[A, B] {
    def run(a: A) = r(a)
  }

  /** The identity partial lens for a given object */
  def plensId[A]: PLens[A, A] =
    implicitly[Category[Lens]].id.partial
}