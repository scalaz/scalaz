package scalaz.example

import scalaz._
import Scalaz._

object ExampleZipper {
  def main(args: Array[String]) = run

  def run {
    val path = nel('usr, 'local, 'scala, 'bin)

    val z1: Zipper[Symbol] = path.toZipper
    z1.toStream assert_≟ path.list.toStream

    z1.focus assert_≟ 'usr
    z1.lefts assert_≟ Stream()
    z1.rights assert_≟ Stream('local, 'scala, 'bin)

    val z2: Option[Zipper[Symbol]] = z1.move(2)
    z2 ∘ (_.focus) assert_≟ 'scala.some
    z2 ∘ (_.lefts) assert_≟ Stream('local, 'usr).some
    z2 ∘ (_.rights) assert_≟ Stream('bin).some

    (z1.next ∗ (_.next) ∗ (_.previous)) assert_≟ z1.move(1)

    z1.previous assert_≟ none[Zipper[Symbol]]
  }
}