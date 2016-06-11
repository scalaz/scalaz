package scalaz
package std

import std.AllInstances._
import std.AllFunctions.fix
import scalaz.scalacheck.ScalazProperties._
import scalaz.scalacheck.ScalazArbitrary._
import org.scalacheck.Prop.forAll

object FunctionTest extends SpecLite {
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

  checkAll("Function1", monoid.laws[Int => Int])

  checkAll("Function0", monad.laws[Function0])
  checkAll("Function1", monad.laws[(B) => ?])
  checkAll("Function2", monad.laws[(B, C) => ?])
  checkAll("Function3", monad.laws[(B, C, D) => ?])
  checkAll("Function4", monad.laws[(B, C, D, E) => ?])
  checkAll("Function5", monad.laws[(B, C, D, E, F) => ?])

  checkAll("Function0", traverse.laws[Function0])

  checkAll("Function1", contravariant.laws[(?) => B])

  checkAll("Function1", category.laws[Function1])

  checkAll("Function1", comonad.laws[Int => ?])

  checkAll("Function1", zip.laws[Int => ?])

  // Likely could be made to cover all the FunctionN types.
  "Function0 map eagerness" ! forAll{(number: Int) =>
    var modifiableNumber: Int = number
    val methodCall: () => Int = () => modifiableNumber
    val mappedCall: () => Int = Monad[Function0].map(methodCall)(_ + 3)
    modifiableNumber += 1
    mappedCall() must_===(number + 4)
  }

  // Likely could be made to cover all the FunctionN types.
  "Function0 bind eagerness" ! forAll{(number: Int) =>
    var modifiableNumber: Int = number
    val methodCall: () => Int = () => modifiableNumber
    val mappedCall = Monad[Function0].bind(methodCall)((value: Int) => () => value + 3)
    modifiableNumber += 1
    mappedCall() must_===(number + 4)
  }

  "fix" ! forAll{(n: Int) =>
    fix[Int](_ => n) must_===(n)
    (fix[Stream[Int]](ns => n #:: (2*n) #:: ns).take(4).toList
      must_===(List(n, 2*n, n, 2*n)))
  }

  object instances {
    def equal[A, R: Equal] = Equal[() => R]
    def semigroup[A, R: Semigroup] = Semigroup[A => R]
    def monad0 = Monad[() => ?]
    def traverse0 = Traverse[Function0]
    def bindRec0 = BindRec[Function0]
    def comonad0 = Comonad[Function0]
    def distributive0 = Distributive[Function0]

    // these aren't working atm.
    //    def monadByName[A] = Monad[(=> A) => ?]
    //    def zipByName[A] = Zip[(=> A) => ?]
    //    def unzipByName[A] = Unzip[(=> A) => ?]
    //    def distributiveByName[A] = Distributive[(=> A) => ?]

    def monad1[A] = Monad[A => ?]
    def comonad1[A: Monoid] = Comonad[A => ?]
    def bindRec1[A] = BindRec[A => ?]
    def zip1[A] = Zip[A => ?]
    def unzip1[A] = Unzip[A => ?]
    def distributive1[A] = Distributive[A => ?]
    def contravariant1[A] = Contravariant[? => A]
    def arrow1 = Arrow[Function1]
    def choice1 = Choice[Function1]
    def proChoice1 = ProChoice[Function1]
    def monoid1[A, R: Monoid] = Monoid[A => R]

    def monad2[A, B] = Monad[(A, B) => ?]
    def bindRec2[A, B] = BindRec[(A, B) => ?]
    def monad3[A, B, C] = Monad[(A, B, C) => ?]
    def bindRec3[A, B, C] = BindRec[(A, B, C) => ?]
    def monad4[A, B, C, D] = Monad[(A, B, C, D) => ?]
    def bindRec4[A, B, C, D] = BindRec[(A, B, C, D) => ?]
    def monad5[A, B, C, D, E] = Monad[(A, B, C, D, E) => ?]
    def bindRec5[A, B, C, D, E] = BindRec[(A, B, C, D, E) => ?]
    def monad6[A, B, C, D, E, F] = Monad[(A, B, C, D, E, F) => ?]
    def bindRec6[A, B, C, D, E, F] = BindRec[(A, B, C, D, E, F) => ?]
    def monad7[A, B, C, D, E, F, G] = Monad[(A, B, C, D, E, F, G) => ?]
    def bindRec7[A, B, C, D, E, F, G] = BindRec[(A, B, C, D, E, F, G) => ?]
    def monad8[A, B, C, D, E, F, G, H] = Monad[(A, B, C, D, E, F, G, H) => ?]
    def bindRec8[A, B, C, D, E, F, G, H] = BindRec[(A, B, C, D, E, F, G, H) => ?]

    // checking absence of ambiguity
    def semigroup[A, R: Monoid] = Semigroup[A => R]
  }
}
