package scalaz
package concurrent

import std.AllInstances._

class ActorTest extends testlib.Spec {
  "code executes async" in {
    var called = false
    implicit val strategy = new Strategy {
      def apply[A](a: => A): () => A = {
        called = true
        () => a
      }
    }
    val actor = Actor[Int](a => a)
    actor ! 0

    called must be_===(true)
  }

}
