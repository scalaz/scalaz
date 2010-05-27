package scalaz.example

object ExampleIteratee {
  def main(args: Array[String]) = run

  import scalaz._
  import Scalaz._
  import Iteratee._

  implicit val StreamEnumerator = new Enumerator[Stream] {
    def apply[E, A](e: Stream[E], i: Iteratee[E, A]): Iteratee[E, A] = e match {
      case Stream() => i
      case x #:: xs => i.fold(done = (_, _) => i, cont = k => apply(xs, k(El(x))))
    }
  }

  def run {
    head(Stream(1, 2, 3)).run.join assert_≟ Some(1)
    length(Stream(10, 20, 30)).run assert_≟ Some(3)
    peek(Stream(1, 2, 3)).run.join assert_≟ Some(1)
    head(Stream[Int]()).run.join assert_≟ none[Int]
  }
}
