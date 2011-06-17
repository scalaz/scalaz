package scalaz.example

import scalaz._, Scalaz._

object ExampleAscii {
  def main(args: Array[String]) = run

  def run {
    val is = List(1, 2, 3)

    // Pointed
    1.η[List] assert_=== 1.point[List]

    // Functor map
    (is ∘ (1 +)) assert_=== (is map (1 +))

    // Contravariant map
    case class IntWrap(i: Int)
    val intShow = implicitly[Show[Int]]
    (intShow ∙ ((_: IntWrap).i)).show(IntWrap(0)) assert_=== (intShow contramap ((_: IntWrap).i)).show(IntWrap(0))

    // Functor bind. Caution: '∗' is Unicode, not Ascii '*'!
    (is >>= (i => List(i, i))) assert_=== (is flatMap (i => List(i, i)))
    (is >>= (i => List(i, i))) assert_=== (is >>= (i => List(i, i)))
    (is >|> List(0, 1)) assert_=== (is >|> List(0, 1))

    // Monadic join
    val ll = List(List(1))
    ll.μ assert_=== ll.join

    // Equal
    (1 ≟ 1) assert_=== (1 === 1)
    (1 ≠ 0) assert_=== (1 /== 0)

    // Semigroup append
    (1 ⊹ 1) assert_=== (1 |+| 1)

    // Dual
    ("1".σ ⊹ "2".σ).value assert_=== ("1".dual |+| "2".dual).value

    // Applicative functor
    (is ⊛ is).tupled assert_=== (is |@| is).tupled

    // Traversable traverse
    (is ↦ (_.some)) assert_=== (is traverse (_.some))

    // Foldable count, sum, any, all
    is.∃(_ < 0) assert_=== is.any(_ < 0)
    is.∀(_ < 0) assert_=== is.all(_ < 0)

    // TODO >>> <<<

  }
}