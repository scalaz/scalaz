package scalaz.effect

import org.specs2.Specification

import scalaz._
import Scalaz._

class IORefSpec extends Specification with RTS {

  def is = "IORefSpec".title ^ s2"""
   Create a new IORef with a specified value and check if:
      `read` returns the current value.                                                        $e1
      `write` puts the new value correctly.                                                    $e2
      `modify` changes the value and returns the updated value.                                $e3
      `modifyFold` changes the value and returns another value computed from the modification. $e4
      `writeLater` puts a new value.                                                           $e5
      `tryWrite` returns
         true and puts a new value to IORef.                                                   $e6
         false and abort if there is a concurrent modification of the value by other fibers.   $e7
      `compareAndSet` returns
        true if the previous value and the current value have the same reference.              $e8
        false if the previous value and the current value have a different reference.          $e9
    """

  def e1 = forall(Data.values) { v =>
    unsafePerformIO(
      for {
        ref   <- IORef(v)
        value <- ref.read[Void]
      } yield value must beTheSameAs(v)
    )
  }

  def e2 = forall(Data.tuples) {
    case (current, update) =>
      unsafePerformIO(
        for {
          ref   <- IORef(current)
          _     <- ref.write(update)
          value <- ref.read[Void]
        } yield value must beTheSameAs(update)
      )
  }

  def e3 = forall(Data.tuples) {
    case (current, update) =>
      unsafePerformIO(
        for {
          ref   <- IORef(current)
          value <- ref.modify[Void](_ => update)
        } yield value must beTheSameAs(update)
      )
  }

  def e4 = forall(Data.tuples) {
    case (current, update) =>
      unsafePerformIO(
        for {
          ref   <- IORef(current)
          r     <- ref.modifyFold(_ => ("hello", update))
          value <- ref.read[Void]
        } yield (r must beTheSameAs("hello")) and (value must beTheSameAs(update))
      )
  }

  def e5 = forall(Data.tuples) {
    case (current, update) =>
      unsafePerformIO(
        for {
          ref   <- IORef(current)
          _     <- ref.writeLater(update)
          value <- ref.read[Void]
        } yield value must beTheSameAs(update)
      )
  }

  def e6 = forall(Data.tuples) {
    case (current, update) =>
      unsafePerformIO(
        for {
          ref     <- IORef(current)
          success <- ref.tryWrite(update)
          value   <- ref.read[Void]
        } yield (success must beTrue) and (value must beTheSameAs(update))
      )
  }

  def e7 = {

    def tryWriteUntilFalse(ref: IORef[Int], update: Int): IO[Void, Boolean] =
      ref.tryWrite(update).flatMap(success => if (!success) IO.point(success) else tryWriteUntilFalse(ref, update))

    unsafePerformIO(
      for {
        ref     <- IORef[Void, Int](0)
        f1      <- ref.write[Void](1).forever[Unit].fork[Void]
        f2      <- ref.write[Void](2).forever[Unit].fork[Void]
        success <- tryWriteUntilFalse(ref, 3)
        value   <- ref.read[Void]
        _       <- (f1 <> f2).interrupt[Void](new Error("Terminated fiber"))
      } yield (success must beFalse) and (value must be_!=(3))
    )

  }

  def e8 = forall(Data.tuples) {
    case (current, update) =>
      unsafePerformIO(
        for {
          ref     <- IORef(current)
          success <- ref.compareAndSet[Void](current, update)
          value   <- ref.read
        } yield (success must beTrue) and (value must beTheSameAs(update))
      )
  }

  def e9 = forall(Data.tuples) {
    case (current, update) =>
      unsafePerformIO(
        for {
          ref     <- IORef(current)
          success <- ref.compareAndSet[Void](update, current)
          value   <- ref.read
        } yield (success must beFalse) and (value must beTheSameAs(current))
      )
  }

  object Data {
    private case class Foo()
    val values = List("hello", new Object, Foo())
    val tuples = Seq(
      "hello"       -> "hi",
      new Object    -> new Object,
      Foo()         -> Foo(),
      List(1, 3, 5) -> List(2, 4, 6)
    )
  }
}
