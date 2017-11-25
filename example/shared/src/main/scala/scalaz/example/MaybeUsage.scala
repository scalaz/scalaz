package scalaz.example

import scalaz.Prelude._

object MaybeUsage extends App {
  // creation
  val x = just(1)
  val y = empty[Int]

  // pattern matching
  x match {
    case Just(i) => println(s"Just $i")
    case Empty() => println(""" ¯\_(ツ)_/¯ """)
  }

  // monad instance and monad ops
  x flatMap { _ => y }

  // Maybe syntax
  val five: Maybe[Int] = 5.just
  val foo: Maybe[String] = "foo".just
}
