package scalaz
package std

import scala.concurrent.Future

trait AllInstances
  extends AnyValInstances with FunctionInstances with ListInstances with MapInstances
  with OptionInstances with SetInstances with StringInstances with StreamInstances
  with TupleInstances with VectorInstances with FutureInstances
  with EitherInstances with PartialFunctionInstances with TypeConstraintInstances
  with scalaz.std.math.BigDecimalInstances with scalaz.std.math.BigInts
  with scalaz.std.math.OrderingInstances
  with scalaz.std.util.parsing.combinator.Parsers
  with scalaz.std.java.util.MapInstances
  with scalaz.std.java.math.BigIntegerInstances
  with scalaz.std.java.EnumInstances
  with scalaz.std.java.util.concurrent.CallableInstances
  with NodeSeqInstances

object AllInstances extends AllInstances {
  implicit def newFutureInstance(implicit ec: scala.concurrent.ExecutionContext): Cobind[Future] with MonadError[({type λ[α,β] = Future[β]})#λ, Throwable] with Catchable[Future] =
    new FutureInstance
}
