package scalaz
package iteratee

import data.LazyOption, LazyOption._


/**The input to an iteratee. **/
sealed trait Input[E] {
  def apply[Z](empty: => Z, el: (=> E) => Z, eof: => Z): Z

  def el: LazyOption[E] =
    apply(lazyNone[E], lazySome(_), lazyNone[E])

  def elOr(e: => E) =
    el.getOrElse(e)
}

object Input extends Inputs {
  def apply[E](e: => E): Input[E] =
    elInput(e)
}

trait Inputs {
  def emptyInput[E]: Input[E] = new Input[E] {
    def apply[Z](empty: => Z, el: (=> E) => Z, eof: => Z) =
      empty
  }

  def elInput[E](e: => E): Input[E] = new Input[E] {
    def apply[Z](empty: => Z, el: (=> E) => Z, eof: => Z) =
      el(e)
  }

  def eofInput[E]: Input[E] = new Input[E] {
    def apply[Z](empty: => Z, el: (=> E) => Z, eof: => Z) =
      eof
  }
}
