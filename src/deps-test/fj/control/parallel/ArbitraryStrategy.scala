package fj.control.parallel

import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary
import org.scalacheck.Gen.value
import Strategy.executorStrategy
import java.util.concurrent.Executors.newFixedThreadPool

object ArbitraryStrategy {
  implicit def arbitraryStrategy[A]: Arbitrary[Strategy[A]] =
    Arbitrary(value(executorStrategy[A](newFixedThreadPool(1))))
}
