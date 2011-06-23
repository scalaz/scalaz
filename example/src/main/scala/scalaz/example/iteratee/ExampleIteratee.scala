package scalaz.example
package iteratee

// todo iteratee package is about to undergo significant change
object ExampleIteratee {
  def main(args: Array[String]) = run

  /*
  import scalaz._, iteratee._
  import Scalaz._
  import IterVT._

  implicit val StreamEnumerator = new Enumerator[Stream] {
    def apply[E, A](e: Stream[E], i: IterV[E, A]): IterV[E, A] = e match {
      case Stream() => i
      case x #:: xs => i.fold(done = (_, _) => i, cont = k => apply(xs, k(El(x))))
    }
  }
  */

  val list = collect[Int, List]
  val reverse = reversed[Int, List](ListReducer)
  val repeatHead = repeat[Int, Option[Int], List](head)

  def run {
    /*
    head(Stream(1, 2, 3)).run assert_=== Some(1)
    length(Stream(10, 20, 30)).run assert_=== 3
    peek(Stream(1, 2, 3)).run assert_=== Some(1)
    head(Stream[Int]()).run assert_=== none[Int]
    list(Stream(1, 2, 3)).run assert_=== List(1, 2, 3)
    repeatHead(Stream(1, 2, 3)).run assert_=== List(Some(1), Some(2), Some(3))
    reverse(Stream(1, 2, 3)).run assert_=== List(3, 2, 1)

    // As a monad
    val m1 = head[Int] >>= ((b:Option[Int]) => head[Int] map (b2 => (b <|*|> b2)))
    m1(Stream(1,2,3)).run assert_=== Some(1 -> 2)
    */

  }
}
