package scalaz.std

trait AllInstances
  extends AnyValInstances with FunctionInstances with ListInstances with MapInstances
  with OptionInstances with SetInstances with StringInstances with StreamInstances
  with TupleInstances with VectorInstances with FutureInstances with ArrayInstances
  with EitherInstances with PartialFunctionInstances with TypeConstraintInstances
  with scalaz.std.math.BigDecimalInstances with scalaz.std.math.BigInts
  with scalaz.std.math.OrderingInstances
  with scalaz.std.java.util.MapInstances
  with scalaz.std.java.math.BigDecimalInstances
  with scalaz.std.java.math.BigIntegerInstances
  with scalaz.std.java.EnumInstances
  with scalaz.std.java.util.concurrent.CallableInstances
  with scalaz.std.java.TimeInstances

object AllInstances extends AllInstances
