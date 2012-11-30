package scalaz
package std

import std.AllInstances._
import scalaz.scalacheck.ScalazProperties._
import scalaz.scalacheck.ScalazArbitrary._

class FunctionTest extends Spec {
  type A = Int
  type B = Int
  type C = Int
  type D = Int
  type E = Int
  type F = Int
  type G = Int
  type H = Int
  type R = Int
  type X = Int
  type Z = Int

  checkAll("Function0", equal.laws[() => Int])

  implicit def EqualFunction0 = Equal.equalBy[() => Int, Int](_.apply())
  implicit def EqualFunction1 = Equal.equalBy[Int => Int, Int](_.apply(0))
  implicit def EqualFunction2 = Equal.equalBy[(Int, Int) => Int, Int](_.apply(0, 0))
  implicit def EqualFunction3 = Equal.equalBy[(Int, Int, Int) => Int, Int](_.apply(0, 0, 0))
  implicit def EqualFunction4 = Equal.equalBy[(Int, Int, Int, Int) => Int, Int](_.apply(0, 0, 0, 0))
  implicit def EqualFunction5 = Equal.equalBy[(Int, Int, Int, Int, Int) => Int, Int](_.apply(0, 0, 0, 0, 0))

  checkAll("Function0", group.laws[Int => Int])

  checkAll("Function0", monad.laws[Function0])
  checkAll("Function1", monad.laws[({type λ[α] = (B) => α})#λ])
  checkAll("Function2", monad.laws[({type λ[α] = (B, C) => α})#λ])
  checkAll("Function3", monad.laws[({type λ[α] = (B, C, D) => α})#λ])
  checkAll("Function4", monad.laws[({type λ[α] = (B, C, D, E) => α})#λ])
  checkAll("Function5", monad.laws[({type λ[α] = (B, C, D, E, F) => α})#λ])

  checkAll("Function0", traverse.laws[Function0])

  checkAll("Function1", category.laws[Function1])

  checkAll("Function1", comonad.laws[({type λ[α]=(Int => α)})#λ])

  // Likely could be made to cover all the FunctionN types.
  "Function0 map eagerness" ! prop{(number: Int) =>
    var modifiableNumber: Int = number
    val methodCall: () => Int = () => modifiableNumber
    val mappedCall: () => Int = Monad[Function0].map(methodCall)(_ + 3)
    modifiableNumber += 1
    mappedCall() must be_===(number + 4)
  }

  // Likely could be made to cover all the FunctionN types.
  "Function0 bind eagerness" ! prop{(number: Int) =>
    var modifiableNumber: Int = number
    val methodCall: () => Int = () => modifiableNumber
    val mappedCall = Monad[Function0].bind(methodCall)((value: Int) => () => value + 3)
    modifiableNumber += 1
    mappedCall() must be_===(number + 4)
  }


  object instances {
    def equal[A, R: Equal] = Equal[() => R]
    def semigroup[A, R: Semigroup] = Semigroup[A => R]
    def monoid[A, R: Monoid] = Semigroup[A => R]
    def group[A, R: Group] = Group[A => R]
    def comonad[A: Monoid, R] = Comonad[({type λ[α]=A => α})#λ]

    // checking absence of ambiguity
    def semigroup[A, R: Group] = Semigroup[A => R]
    def monoid[A, R: Group] = Semigroup[A => R]
  }
}
