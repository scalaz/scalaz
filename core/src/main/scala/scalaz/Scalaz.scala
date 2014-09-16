package scalaz

import scala.concurrent.Future

object Scalaz
  extends StateFunctions        // Functions related to the state monad
  with syntax.ToTypeClassOps    // syntax associated with type classes
  with syntax.ToDataOps         // syntax associated with Scalaz data structures
  with std.AllInstances         // Type class instances for the standard library types
  with std.AllFunctions         // Functions related to standard library types
  with syntax.std.ToAllStdOps   // syntax associated with standard library types
  with IdInstances {            // Identity type and instances

  implicit def newFutureInstance(implicit ec: scala.concurrent.ExecutionContext): Cobind[Future] with MonadError[({type λ[α,β] = Future[β]})#λ, Throwable] with Catchable[Future] =
    new scalaz.std.FutureInstance
}
