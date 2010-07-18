package scalaz.example

object ExampleIteratee {
  def main(args: Array[String]) = run

  import scalaz._
  import Scalaz._
  import IterV._

  implicit val StreamEnumerator = new Enumerator[Stream] {
    def apply[E, A](e: Stream[E], i: IterV[E, A]): IterV[E, A] = e match {
      case Stream() => i
      case x #:: xs => i.fold(done = (_, _) => i, cont = k => apply(xs, k(El(x))))
    }
  }

  def run {
    head(Stream(1, 2, 3)).run assert_≟ Some(1)
    length(Stream(10, 20, 30)).run assert_≟ 3
    peek(Stream(1, 2, 3)).run assert_≟ Some(1)
    head(Stream[Int]()).run assert_≟ none[Int]
    
    // As a monad
    val m1 = head[Int] >>= ((b:Option[Int]) => head[Int] map (b2 => (b <|*|> b2)))
    m1(Stream(1,2,3)).run assert_≟ Some(1 -> 2)
    
    
  }
}
