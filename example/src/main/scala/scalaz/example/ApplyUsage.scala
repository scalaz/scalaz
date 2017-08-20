package scalaz.example

object ApplyUsage extends App {
  import scalaz.Apply
  import scalaz.std.option._
  import scalaz.std.list._
  import scalaz.std.string._
  import scalaz.std.anyVal._
  import scalaz.std.vector._
  import scalaz.std.tuple._
  import scalaz.syntax.equal._
  import scalaz.syntax.std.option._

  // Apply extends the (hopefully familar) Functor Typeclass by adding
  // a method named "ap" which is similar to "map" from Functor in
  // that it applies a function to values in a context, however with
  // ap, the function is also in the same context. Here are some
  // examples, contrasted with map

  val intToString: Int => String = _.toString
  val double: Int => Int = _ * 2
  val addTwo: Int => Int = _ + 2

  // map
  assert(Apply[Option].map(1.some)(intToString) === "1".some)
  assert(Apply[Option].map(1.some)(double) === 2.some)
  assert(Apply[Option].map(none)(double) === none)

  // ap
  assert(Apply[Option].ap(1.some)(some(intToString)) === "1".some)
  assert(Apply[Option].ap(1.some)(some(double)) === 2.some)
  assert(Apply[Option].ap(none)(some(double)) === none)
  assert(Apply[Option].ap(1.some)(none[Int => Int]) === none[Int])
  assert(Apply[Option].ap(none)(none[Int => Int]) === none[Int])
  assert(Apply[List].ap(List(1,2,3))(List(double, addTwo)) === List(2,4,6,3,4,5))

  // from these two methods (map and ap) we are able to derive some
  // very useful methods which allow us to "lift" a function of
  // multiple arguments into a context. There are methods named
  // by how many parameters they take
  val add2 = ((_:Int) + (_:Int))
  val add3 = ((_:Int) + (_:Int) + (_:Int))
  val add4 = ((_:Int) + (_:Int) + (_:Int) + (_:Int))
  assert(Apply[Option].apply2(some(1), some(2))(add2) === some(3))
  assert(Apply[Option].apply3(some(1), some(2), some(3))(add3) === some(6))
  assert(Apply[Option].apply4(some(1), some(2), some(3), some(4))(add4) === some(10))

  // the effects from the context we are operating on are carried
  // through the computation, so, for example, in the case of the
  // Option Apply instance here, if any of the arguments are None, the
  // result of the entire computation is None:
  assert(Apply[Option].apply3(some(1), none, some(3))(add3) === None)

  // tuple2, tuple3 etc, will construct tuples from values from the
  // provided contexts
  assert(Apply[List].tuple3(List(1,2,3), List("a", "b"), List(())) ===
           List((1,"a",()),(1,"b",()),(2,"a",()),
                (2,"b",()),(3,"a",()),(3,"b",())))


  // There some helpful syntax available for the Apply typeclass:
  import scalaz.syntax.apply._

  // <*> is syntax for the "ap" method
  val plus1: Int => Int = _ + 1
  val plus2: Int => Int = _ + 2
  assert(List(1,2,3) <*> List(plus1, plus2) === List(2,3,4,3,4,5))

  // |@| is refered to as "applicative builder", it allows you to
  // evaluate a function of multiple arguments in a context, similar
  // to apply2, apply3, apply4, etc:
  assert((some(1) |@| some(2) |@| some(3))(_ + _ + _) === Some(6))
  assert((some(1) |@| none[Int] |@| some(3))(_ + _ + _) === None)

  def add8(a: Int, b: Int, c: Int, d: Int, e: Int, f: Int, g: Int, h: Int) = a+b+c+d+e+f+g+h
  val someOf8Options = (1.some |@| 2.some |@| 3.some |@| 4.some |@|
                        5.some |@| 6.some |@| 7.some |@| 8.some)(add8 _)
  assert(someOf8Options === 36.some)

  // the applicative builder created by |@| also has a "tupled" method
  // which will tuple the arguments
  assert((List(1,2,3) |@| List("a","b","c")).tupled ===
           List(1 -> "a", 1 -> "b", 1 -> "c",
                2 -> "a", 2 -> "b", 2 -> "c",
                3 -> "a", 3 -> "b", 3 -> "c"))


  // there are ^, ^^, ^^^, etc methods which correspond respectively
  // to apply2, apply, apply4, etc.
  assert(^(1.some, 2.some)(_ + _) === 3.some)
  assert(^^^(1.some, 2.some, 3.some, 4.some)(_ + _ + _ + _) === 10.some)
  assert(^^^(1.some, 2.some, none[Int], 4.some)(_ + _ + _ + _) === none)

  // sometimes we will want to apply a function in a context in order
  // to preserve the effects of running the function in a context, but
  // are not interested in the result of the function, for this there
  // are two combinators, *> which discards the value on the left, and
  // <* which discards the value on the right.

  // as an example we'll use "Writer", which a context which performs
  // a computation while accumulating a "side-value". Very commonly,
  // writer is used to emit log messages along with the value being
  // computed.

  import scalaz.{Writer,DList}
  import scalaz.syntax.writer._
  type Logged[A] = Writer[DList[String], A]

  // log a message, return no results (hence Unit)
  def log(message: String): Logged[Unit] = DList(message).tell

  // log that we are adding, and return the results of adding x and y
  def compute(x: Int, y: Int): Logged[Int] =
    log("adding " + x + " and " + y) as (x+y)

  // we log a message "begin", we add two numbers, we log "end",
  // neither calls to "log" compute a value, they are only evaluated
  // for the "effect" of logging a message, so we can use *> and <* to
  // discard the computed value (which in this case is Unit), while
  // preserving the value computed in the call to "compute"
  def addAndLog(x: Int, y: Int): Logged[Int] =
    log("begin") *> compute(x,y) <* log("end")

  val (written,result) = addAndLog(1,2).run
  assert(written === DList("begin", "adding 1 and 2", "end"))
  assert(result === 3)

  val (written2,result2) = addAndLog(1, 10).run
  assert(written2 === DList("begin", "adding 1 and 10", "end"))
  assert(result2 === 11)

  // Apply instances can be composed, which allows us to lift a
  // function into a computation in multiple nested contexts, while
  // applying the effects of all contexts:
  val applyVLO = Apply[Vector] compose Apply[List] compose Apply[Option]

  val deepResult =
    applyVLO.apply2(Vector(List(1.some, none[Int]),
                           List(2.some, 3.some)),
                    Vector(List("a".some, "b".some, "c".some)))(_.toString + _)

  val expectedDeep = Vector(List(Some("1a"), Some("1b"), Some("1c"),
                                 None, None, None),
                            List(Some("2a"), Some("2b"), Some("2c"),
                                 Some("3a"), Some("3b"), Some("3c")))

  assert(deepResult === expectedDeep)

}
