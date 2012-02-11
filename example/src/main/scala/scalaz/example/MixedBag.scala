package scalaz.example

import scalaz._

object MixedBag extends App {
  monoid()
  traverseBigList()
  traverseBigStream()
  tree()
  kleisiArrow()
  dListExample()

  def monoid() {
    import std.anyVal._
    import std.option._

    import syntax.monoid._

    1 |+| 2
    1 mappend 2
    some(1) |+| some(2)
    some(1) |+| mzero[Option[Int]]

    intInstance.semigroupSyntax.mappend(1, 2)
  }

  def traverseBigList() {
    import std.option._
    import std.list._
    import syntax.traverse._

    //    val xs: Option[List[Int]] = (1 to 100000 toList).traverse(x => some(x * 2))
    //    println(xs map (_ take 10))
    ()
  }

  def traverseBigStream() {
    import std.option._
    import std.stream._
    import syntax.traverse._

    (1 to 100000 toStream).traverse(x => none[Int])
    //    (1 to 100000 toStream).traverse(x => some(x * 2))
    ()
  }

  def tree() {
    import std.string._
    import syntax.semigroup._
    import syntax.equal._
    import syntax.tree._
    import syntax.traverse._
    import std.stream._

    val tree: Tree[Int] = 1.node(2.node(3.leaf), 4.leaf, 5.leaf)
    val r = tree.foldRight(".")((i, s) => i.toString |+| s)
    r assert_=== "12345."
    val f = tree.flatten.foldMap(_.toString)
    f assert_=== "12345"
    val m = tree.foldMap(_.toString)
    m assert_=== "12345"
  }

  def kleisiArrow() {
    import Kleisli._
    import std.option._
    import syntax.compose._

    val f = kleisli((i: Int) => some(i))
    f map (i => i * 2) map (x => println(x)) run 3

    val K = Kleisli.kleisliArrId[Option] // or, Arr[({type λ[α, β]=Kleisli[Option, α, β]})#λ]
    f >>> K.arr(i => i * 2) >>> K.arr(x => println(x)) run 3
  }

  def dListExample() {
    import DList._
    import syntax.monad._
    import syntax.writer._
    import WriterT._
    import Free._

    type Pair[+A] = (A, A)
    type Tree[A] = Free[Pair, A]

    implicit val pairFunctor: Functor[Pair] = new Functor[Pair] {
      def map[A, B](as: Pair[A])(f: A => B) =
        f(as._1) -> f(as._2)
    }

    def leaf[A](a: A): Tree[A] = Return(a)
    def node[A](l: Tree[A], r: Tree[A]): Tree[A] = Suspend[Pair, A](l -> r)

    def flattenWriter[A](t: Tree[A]): DList[A] = {
      def flatten(t: Tree[A]): Writer[DList[A], Unit] = t.resume match {
        case Right(a)     => DList(a).tell
        case Left((x, y)) => flatten(x) >> flatten(y)
      }
      flatten(t).run._1
    }

    flattenWriter(node(node(leaf(1), leaf(3)), leaf(2))).toList
  }

  def zipper() {
    import scalaz.std.list

    val fileName = "abc.txt"

    val oldExtensionAndNewName: Option[(String, String)] = for {
      zipper <- list.toZipper(fileName.toList)

      // previousC from the first position rotates the focus to the last element
      zipperAtLast = zipper.previousC

      // focus on the first preceding character, if found, that `== '.'`
      zipperAtDot <- zipper.findPrevious(_ == '.')

      // Zipper#rights contains the elements to the right of the focus
      oldExtension = zipperAtDot.rights.mkString

      // Change the extension.
      changedExtZipper = zipperAtDot.copy(rights = "log".toStream)

      // Convert the Zipper back to a string
      newFileName = changedExtZipper.toStream.mkString
    } yield (oldExtension, newFileName)
  }
}
