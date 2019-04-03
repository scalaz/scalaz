package scalaz.example

import scalaz.{Endo,IList,Zip}
import scalaz.std.function._
import scalaz.std.anyVal._
import scalaz.syntax.monoid._
import scalaz.syntax.foldable._

object EndoUsage extends App {
  // The scala Endo class is a class which wraps functions from A ⇒ A
  // for some A. This class exists in order to supply some special
  // typeclass instances, since functions where the domain and the
  // codomain are the same type have some special properties.

  // there already exists a Monoid instance for any Function1 where
  // there exists a monoid for the codomain, the append function of
  // this monoid returns a function which for a given input, returns
  // the resut of applying each function to this input and append the
  // two results to each other:
  val f1: Int ⇒ Int = _ + 1
  val f2: Int ⇒ Int = _ * 10

  assert(f1(1) == 2)
  assert(f2(1) == 10)

  val f3 = f1 |+| f2

  assert(f3(1) == f1(1) + f2(1))
  assert(f3(10) == f1(10) + f2(10))

  // for an endofunctor, we can alternatively represent the monoid
  // append operation as function composition:

  val ef3 = Endo(f1) |+| Endo(f2)

  assert(ef3(1) == (f1 compose f2)(1))
  assert(ef3(10) == (f1 compose f2)(10))

  // having this monoid allows us to take a Foldable full of
  // endomorphisms for some type, and squash that down into a single
  // function:

  case class Person(first: String, last: String, age: Int)

  val lowercaseFirst: Endo[Person] = Endo(p ⇒ p.copy(first=p.first.toLowerCase))
  val lowercaseLast: Endo[Person] = Endo(p ⇒ p.copy(last=p.last.toLowerCase))
  val trimFirst: Endo[Person] = Endo(p ⇒ p.copy(first=p.first.trim))
  val trimLast: Endo[Person] = Endo(p ⇒ p.copy(last=p.last.trim))
  val birthday: Endo[Person] = Endo(p ⇒ p.copy(age = p.age+1))
  val endos: IList[Endo[Person]] = IList(lowercaseFirst, lowercaseLast, trimFirst, trimLast, birthday)

  val stewBefore = Person(" Stew ", "O'Connor", 70)
  assert(endos.suml.apply(stewBefore) == Person("stew", "o'connor", 71))

  // endofunctors can be zipped together to get back an endofunctor that operates on tuples:
  val ints = IList(1,2,3)
  val strings = IList("a","b","c")

  val plus1: Endo[Int] = Endo(_ + 1)
  val toUpper: Endo[String] = Endo(_.toUpperCase)

  val foo = (ints zip strings) map Zip[Endo].zip(plus1, toUpper).run
  assert(foo == IList((2,"A"),(3,"B"),(4,"C")))
}

