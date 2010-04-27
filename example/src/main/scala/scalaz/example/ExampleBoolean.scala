package scalaz.example

object ExampleBoolean {
  def main(args: Array[String]) = {
    run
  }

  import scalaz._
  import Scalaz._

  def run {
    true ?? List(1, 2, 3) assert_≟ List(1, 2, 3)
    false ?? List(1, 2, 3) assert_≟ List()

    true !? List(1, 2, 3) assert_≟ List()
    false !? List(1, 2, 3) assert_≟ List(1, 2, 3)

    {
      var i = 0
      true ! {i += 1}
      true when {i += 2}
      true unless {i += 4}
      i assert_≟ 3
    }
    {
      var i = 0
      false ! {i += 1}
      false when {i += 2}
      false unless {i += 4}
      i assert_≟ 4
    }
    true option 1 assert_≟ some(1)
    false option 1 assert_≟ none[Int]

    true ? 1 | 0 assert_≟ 1
    false ? 1 | 0 assert_≟ 0

    true either 1 or 0 assert_≟ 1.left
    false either 1 or 0 assert_≟ 0.right

    true.guard[List](1) assert_≟ List(1)
    false.guard[List](1) assert_≟ nil[Int]

    true.prevent[List](1) assert_≟ nil[Int]
    false.prevent[List](1) assert_≟ List(1)
  }
}