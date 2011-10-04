package scalaz.example

object ExampleIteratee {
  def main(args: Array[String]) = run

  import scalaz._
  import Scalaz._
  import IterV._

  implicit val StreamEnumerator = new Enumerator[Stream] {
    @annotation.tailrec def apply[E, A](e: Stream[E], i: IterV[E, A]): IterV[E, A] = {
       val next: Option[(Stream[E], IterV[E, A])] = e match {
         case Stream() => None
         case x #:: xs => i.fold(done = (_, _) => None, cont = k => some((xs, k(El(x)))))
       }
       next match {
         case None => i
         case Some((es, is)) => apply(es, is)
       }
    }
  }

  val list = collect[Int, List]
  val reverse = reversed[Int, List](ListReducer)
  val repeatHead = repeat[Int, Option[Int], List](head)

  def run {
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
    
    
  }
}
