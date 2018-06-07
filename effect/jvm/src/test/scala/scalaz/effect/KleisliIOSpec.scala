package scalaz
package effect

import org.specs2.Specification
import scalaz.effect.KleisliIO._
import scala.Predef.genericArrayOps
import scala.{ Array, Either, Left, List, Right }

class KleisliIOSpec extends Specification with RTS {
  def is = "KleisliIOSpec".title ^ s2"""
   Check if the functions in `KleisliIO` work correctly
     `lift` lifts from A => B into effectful function $e1
     `identity` returns the identity of the input without modification $e2
     `>>>` is a symbolic operator of `andThen`which does a Backwards composition of effectul functions $e3
     `<<<` is a symbolic operator of `compose` which compses two effectful functions $e4
     `zipWith` zips the output of two effectful functions $e5
     `&&&` zips the output of two effectful functions and returns a tuple of their result $e6
     `|||` computes two effectful functions left and right from from an Either input $e7
     `first` returns a tuple: the output on the first element and input on the second element $e8
     `second` returns a tuple: the input on the first element and output on the second element $e9
     `left` takes an Either as input and computes it if it is Left otherwise returns the same value of the input $e10
     `right`takes an Either as input and computes it if it is Right otherwise returns the same value of the input   $e11
     `asEffect` returns the input value $e12
     `test` check a condition and returns an Either output: Left if the condition is true otherwise false $e13
     `ifThenElse` check an impure condition if it is true then computes an effectful function `then0` else computes `else0` $e14a
     `ifThenElse` check a pure condition if it is true then computes an effectful function `then0` else computes `else0` $e14b
     `whileDo` take a condition and run the body until the condition will be  false with impure function $e15a
     `whileDo` take a condition and run the body until the condition will be  false with pure function $e15b
     `_1` extracts out the first element of a tupe $e16
     `_2` extracts out the second element of a tupe $e17
     `fail` returns a failure  $e18a
     `impure` can translate an Exception to an error  $e18b
    """

  def e1 =
    unsafePerformIO(
      for {
        v <- lift[Void, Int, Int](_ + 1)(4)
      } yield v must_=== 5
    )

  def e2 =
    unsafePerformIO(
      for {
        v <- identity[Void, Int](1)
      } yield v must_=== 1
    )

  def e3 =
    unsafePerformIO(
      for {
        v <- (lift[Void, Int, Int](_ + 1) >>> lift[Void, Int, Int](_ * 2))(6)
      } yield v must_=== 14
    )

  def e4 =
    unsafePerformIO(
      for {
        v <- (lift[Void, Int, Int](_ + 1) <<< lift[Void, Int, Int](_ * 2))(6)
      } yield v must_=== 13
    )

  def e5 =
    unsafePerformIO(
      for {
        v <- point[Void, Int, Int](1).zipWith[Int, Int](point[Void, Int, Int](2))((a, b) => a + b)(1)
      } yield v must_=== 3
    )

  def e6 =
    unsafePerformIO(
      for {
        v <- (lift[Void, Int, Int](_ + 1) &&& lift[Void, Int, Int](_ * 2))(6)
      } yield (v._1 must_=== 7) and (v._2 must_=== 12)
    )

  def e7 =
    unsafePerformIO(
      for {
        l <- (lift[Void, Int, Int](_ + 1) ||| lift[Void, Int, Int](_ * 2))(Left(25))
        r <- (lift[Void, List[Int], Int](_.sum) ||| lift[Void, List[Int], Int](_.size))(Right(List(1, 3, 5, 2, 8)))
      } yield (l must_=== 26) and (r must_=== 5)
    )

  def e8 =
    unsafePerformIO(
      for {
        v <- lift[Void, Int, Int](_ * 2).first(100)
      } yield (v._1 must_=== 200) and (v._2 must_=== 100)
    )

  def e9 =
    unsafePerformIO(
      for {
        v <- lift[Void, Int, Int](_ * 2).second(100)
      } yield (v._1 must_=== 100) and (v._2 must_=== 200)
    )
  def e10 =
    unsafePerformIO(
      for {
        v1 <- lift[Void, Int, Int](_ * 2).left[Int](Left(6))
        v2 <- point[Void, Int, Int](1).left[String](Right("hi"))
      } yield (v1 must beLeft(12)) and (v2 must beRight("hi"))
    )

  def e11 =
    unsafePerformIO(
      for {
        v1 <- lift[Void, Int, Int](_ * 2).right[String](Left("no value"))
        v2 <- lift[Void, Int, Int](_ * 2).right[Int](Right(7))
      } yield (v1 must beLeft("no value")) and (v2 must beRight(14))
    )

  def e12 =
    unsafePerformIO(
      for {
        v <- lift[Void, Int, Int](_ * 2).asEffect(56)
      } yield v must_=== 56
    )

  def e13 =
    unsafePerformIO(
      for {
        v1 <- test(lift[Void, Array[Int], Boolean](_.sum > 10))(Array(1, 2, 5))
        v2 <- test(lift[Void, Array[Int], Boolean](_.sum > 10))(Array(1, 2, 5, 6))
      } yield (v1 must beRight(Array(1, 2, 5))) and (v2 must beLeft(Array(1, 2, 5, 6)))
    )

  def e14a =
    unsafePerformIO(
      for {
        v1 <- ifThenElse(lift[Void, Int, Boolean](_ > 0))(point[Void, Int, String]("is positive"))(
               point[Void, Int, String]("is negative")
             )(-1)
        v2 <- ifThenElse(lift[Void, Int, Boolean](_ > 0))(point[Void, Int, String]("is positive"))(
               point[Void, Int, String]("is negative")
             )(1)
      } yield (v1 must_=== "is negative") and (v2 must_=== "is positive")
    )

  def e14b =
    unsafePerformIO(
      for {
        v1 <- ifThenElse(pure[Void, Int, Boolean](a => IO.now(a > 0)))(point[Void, Int, String]("is positive"))(
               point[Void, Int, String]("is negative")
             )(-1)
        v2 <- ifThenElse(pure[Void, Int, Boolean](a => IO.now(a > 0)))(point[Void, Int, String]("is positive"))(
               point[Void, Int, String]("is negative")
             )(1)
      } yield (v1 must_=== "is negative") and (v2 must_=== "is positive")
    )

  def e15a =
    unsafePerformIO(
      for {
        v <- whileDo[Void, Int](lift[Void, Int, Boolean](_ < 10))(lift[Void, Int, Int](_ + 1))(1)
      } yield v must_=== 10
    )

  def e15b =
    unsafePerformIO(
      for {
        v <- whileDo[Void, Int](pure[Void, Int, Boolean](a => IO.now[Void, Boolean](a < 10)))(
              pure[Void, Int, Int](a => IO.sync[Void, Int](a + 1))
            )(1)
      } yield v must_=== 10
    )

  def e16 =
    unsafePerformIO(
      for {
        v <- _1[Void, Int, String]((1, "hi"))
      } yield v must_=== 1
    )

  def e17 =
    unsafePerformIO(
      for {
        v <- _2[Void, Int, String]((2, "hola"))
      } yield v must_=== "hola"
    )

  def e18a =
    unsafePerformIO(
      for {
        a <- fail[String, Int, Int]("error")(1).attempt
      } yield a must_=== -\/("error")
    )

  def e18b =
    unsafePerformIO(
      for {
        a <- impure[String, Int, Int] { case _: Throwable => "error" }(_ => throw new Exception)(9).attempt
      } yield a must_=== -\/("error")
    )
}
