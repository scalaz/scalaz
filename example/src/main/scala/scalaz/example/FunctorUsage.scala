package scalaz
package example

import std.option._
import std.list._
import std.map._
import std.anyVal._
import std.string._
import std.tuple._
import syntax.equal._
import scalaz.concurrent.Task
import syntax.functor._

/**
  * A Functor is a ubiquitous typeclass involving type constructors of
  * kind * → *, which is another way of saying types that have a
  * single type variable. Examples might be Option, List, Future.
  *
  * The Functor category involves a single operation, named `map`:
  *
  * def map[A, B](fa: F[A])(f: A => B): F[B]
  *
  * This method takes a Function from A => B and turns an F[A] into an F[B]
  */
object FunctorUsage extends App {

  val len: String => Int = _.length

  //
  // map
  //

  // Option is a functor which always returns a Some with the function
  // applied when the Option value is a Some.
  assert(Functor[Option].map(Some("adsf"))(len) === Some(4))
  // When the Option is a None, it always returns None
  assert(Functor[Option].map(None)(len) === None)

  // List is a functor which applies the function to each element of
  // the list.
  assert(Functor[List].map(List("qwer", "adsfg"))(len) === List(4,5))

  //
  // lift
  //

  // We can use the Funtor to "lift" a function to operate on the Functor type:
  val lenOption: Option[String] => Option[Int] = Functor[Option].lift(len)
  assert(lenOption(Some("abcd")) === Some(4))

  //
  // strength
  //

  // Functors in scalaz all come equipped with tensorial strenth! does
  // that sound exciting? It's not that exciting, it means that we get
  // two additional derived functions which allow us to turn the
  // contained values into tuples:
  assert(Functor[List].strengthL("a", List(1,2,3)) === List("a" -> 1, "a" -> 2, "a" -> 3))
  assert(Functor[List].strengthR(List(1,2,3), "a") === List(1 -> "a", 2 -> "a", 3 -> "a"))

  // there is syntax for the strength functions
  assert(List(1,2,3).strengthL("a") === List("a" -> 1, "a" -> 2, "a" -> 3))
  assert(List(1,2,3).strengthR("a") === List(1 -> "a", 2 -> "a", 3 -> "a"))

  //
  // fproduct
  //

  // Functor provides a fproduct function which pairs a value with the
  // result of applying a function to that value.
  val source = List("a", "aa", "b", "ccccc")
  val result = Map("a" -> 1, "aa" -> 2, "b" ->  1, "ccccc" -> 5)

  assert(source.fproduct(len).toMap === result)


  //
  // void
  //

  // We can "void" a functor, which will change any F[A] into a F[Unit]
  assert(Functor[Option].void(Some(1)) === Some(()))

  // You might wonder why such a thing would ever be useful, it will
  // become useful when we have functors that control side-effects
  // here's a bit of a contrived example to show where we might want
  // to void a functor.

  // pretend this is our database
  var database = Map("abc" → 1,
                     "aaa" → 2,
                     "qqq" → 3)

  // Return a Task which removes items from our database and returns the number of items deleted
  def del(f: String => Boolean): Task[Int] = Task.delay {
    val (count, db) = database.foldRight(0 → List.empty[(String,Int)]) {
      case ((k,_),(d,r)) if f(k) => (d+1, r)
      case (i,(d,r)) => (d, i::r)
    }
    database = db.toMap
    count
  }

  // This is a task which will delete two of the three items in our database,
  val delTask = del(_.startsWith("a"))

  // it hasn't run yet
  assert(database.size === 3)

  // but perhaps we don't care about the number of items that were
  // deleted, we really just want to execute the side-effects, and get
  // a Task[Unit]
  val voidTask: Task[Unit] = Functor[Task].void(delTask)

  // There is syntax for void.
  val voidTask2: Task[Unit] = delTask.void

  // Running the task returns a Unit.
  assert(voidTask.unsafePerformSync === (()))

  // And now our database is smaller
  assert(database.size === 1)

  //
  // Composition
  //

  // Functors compose! Given any Functor F[_] and any Functor G[_] we
  // can compose the two Functors to create a new Functor on F[G[_]]:
  val listOpt = Functor[List] compose Functor[Option]
  assert(listOpt.map(List(Some(1), None, Some(3)))(_ + 1) === List(Some(2), None, Some(4)))
}
