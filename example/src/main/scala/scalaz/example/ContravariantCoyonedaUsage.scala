package scalaz.example

import scalaz.{\/, ContravariantCoyoneda => CtCoyo, Order}
import scalaz.std.anyVal._
import scalaz.std.function._
import scalaz.std.list._
import scalaz.std.option._
import scalaz.std.string._
import scalaz.std.tuple._
import scalaz.syntax.arrow._
import scalaz.syntax.id._
import scalaz.syntax.monoid._
import scalaz.syntax.order._
import scalaz.syntax.traverse._
import scalaz.syntax.std.option._
import scalaz.syntax.std.string._

object ContravariantCoyonedaUsage extends App {
  // Suppose I have some unstructured data.
  val unstructuredData: List[Vector[String]] =
    List(Vector("Zürich", "807", "383,708"),
         Vector("東京", "1868", "13,185,502"),
         Vector("Brisbane", "1824-09-13", "2,189,878"),
         Vector("München", "1158", "1,388,308"),
         Vector("Boston", "1630-09-07", "636,479"))

  // Or, really, maybe it has some structure.  That's not important.
  // What matters is, I want to sort the data according to various
  // rules.

  def numerically1: Order[String] =
    Order.order((a, b) => parseCommaNum(a) ?|? parseCommaNum(b))

  // Which is a silly way to write this.  Let's try again:

  def numerically2: Order[String] =
    Order orderBy parseCommaNum

  // Which is a shorthand way of writing

  def numerically3: Order[String] =
    Order[Long \/ String] contramap parseCommaNum

  // All of them have the same behavior: to compare two strings, do
  // `parseCommaNum' on them, and compare the results.  The
  // interesting thing that `numerically3' reveals is that this is
  // just applying the ordinary contravariant functor for `Order'.
  //
  // We'll call `parseCommaNum' a "sort key" function.  Here's that,
  // and the other two we'll be using for this example.

  def parseCommaNum(s: String): Long \/ String =
    ("""-?[0-9,]+""".r findFirstIn s
       flatMap (_.filter(_ != ',').parseLong.toOption)) <\/ s

  def caseInsensitively(s: String): String =
    s.toUpperCase.toLowerCase

  def parseDate(s: String): (Int, Option[(Int, Int)]) \/ String =
    (for {
       grps <- """([0-9]+)-([0-9]+)-([0-9]+)""".r findFirstMatchIn s
       List(y, m, d) <- grps.subgroups.traverse(_.parseInt.toOption)
     } yield (y, Some((m, d))))
      .orElse(for {
                n <- """[0-9]+""".r findFirstIn s
                yi <- n.parseInt.toOption
              } yield (yi, None)) <\/ s

  // With sort keys in hand, we can produced contramapped `Order's all
  // on the argument type, String, that will compare two values by
  // applying the sort key function to each argument and comparing the
  // result.

  def caseInsensitivelyOrd: Order[String] =
    Order orderBy caseInsensitively

  def dateOrd: Order[String] =
    Order orderBy parseDate

  // Schwartzian transform
  // ---------------------
  //
  // The problem with using these `Order[String]'s directly is that
  // the "sort key" function gets applied twice for each *comparison*
  // between two elements, rather than once for each element.  A
  // typical sort can compare a given element many times, wasting the
  // work of calling the sort key function repeatedly.
  //
  // For simple functions, this doesn't matter.  For complex sort key
  // functions, it can make a great deal of difference.  So we use the
  // Schwartzian transform:

  def schwartzian[A, B](xs: List[A])(f: A => B)(implicit B: Order[B]): List[A] =
    xs.map(a => (a, f(a)))
      .sortBy(_._2)(B.toScalaOrdering)
      .map(_._1)

  def nonschwartzian[A, B](xs: List[A])(f: A => B)(implicit B: Order[B]): List[A] =
    xs.sorted(Order.orderBy(f).toScalaOrdering)

  // The above two functions are guaranteed to return the same result for
  // pure 'f', but `schwartzian' may be faster for complex `f'.  By
  // the simple expedient of separating the sort key from the Order
  // passed to the real sort algorithm, we guarantee that the sort key
  // function will only be called once per element.
  //
  // More at [1].
  //
  // Simple sort key usage
  // ---------------------
  //
  // It's straightforward enough to use our sort key functions to sort
  // by particular elements of the data above.

  val byDirectSorts: List[List[Vector[String]]] =
    List(schwartzian(unstructuredData)(v => caseInsensitively(v(0))),
         schwartzian(unstructuredData)(v => parseDate(v(1))),
         schwartzian(unstructuredData)(v => parseCommaNum(v(2))))

  // Something interesting happens when you try to abstract over the
  // sort key function, though:

  /*
  val byFunListSorts = for {
    (f, i) <- List((caseInsensitively _, 0),
                  (parseDate _, 1),
                  (parseCommaNum _, 2))
  } yield schwartzian(unstructuredData)(v => f(v(i)))

…/example/src/main/scala/scalaz/example/ContravariantCoyonedaUsage.scala:125: could not find implicit value for parameter B: scalaz.Order[java.io.Serializable]
  } yield schwartzian(unstructuredData)(v => f(v(i)))
                                       ^
   */

  // Fails to compile with the given type error.  That's because the
  // type of that list of sort key functions and indexes is:

  val untypedSortKeys: List[(String => java.io.Serializable, Int)] =
    List((caseInsensitively _, 0),
         (parseDate _, 1),
         (parseCommaNum _, 2))

  // The problem is that the return type for each sort key is different,
  // and is an absolutely essential part of the sort process.  Check
  // the type of `schwartzian' again: we absolutely must have an
  // `Order' that relates the return type of the sort function.  For
  // example, to use the `parseDate' sort key, we must compute and
  // apply an `Order[(Int, Option[(Int, Int)]) \/ String]'.
  //
  // The standard way to solve this is to fuse the sort key into its
  // result's ordering.  That's what happens here:

  val byOrdListSorts: List[List[Vector[String]]] = for {
    (ord, i) <- List((caseInsensitivelyOrd, 0),
                    (dateOrd, 1),
                    (numerically2, 2))
  } yield unstructuredData.sortBy(v => v(i))(ord.toScalaOrdering)

  // We can no longer use `schwartzian', though, because there is no
  // way to separate the sort key from the underlying Order.
  //
  // Enter Contravariant Coyoneda
  // ----------------------------
  //
  // Recall that sort keys are applied by contramap, as seen in the
  // definition of `numerically3'.  What if we represented that
  // contramap call as a type?  `ContravariantCoyoneda' can do that
  // for us.

  val numerically4: CtCoyo[Order, String] =
    CtCoyo(Order[Long \/ String])(parseCommaNum)

  // This separates the sort key and the underlying order, but the
  // sort key's result type no longer appears!  It's been made
  // *existential*, and the main feature of the
  // `ContravariantCoyoneda' structure, for our purposes, is that it
  // remembers that the output type of the function is exactly the
  // type of the `Order', *even though it's forgotten what that type
  // is*.
  //
  // That's enough to call `schwartzian': `schwartzian' doesn't care
  // about *what* the `B' type argument is, just that it gets
  // arguments that line up for it!
  //
  // First, for convenience:

  val CCOrder = CtCoyo.by[Order]

  // But see `numerically4' for the full version of the below CCOrder
  // calls.

  val decomposedSortKeys: List[(CtCoyo[Order, String], Int)] =
    List((CCOrder(caseInsensitively), 0),
         (CCOrder(parseDate), 1),
         (numerically4, 2))

  // Now we're ready.

  val bySchwartzianListSorts: List[List[Vector[String]]] = for {
    (ccord, i) <- decomposedSortKeys
  } yield schwartzian(unstructuredData)(v => ccord.k(v(i)))(ccord.fi)

  // But we know that for each `schwartzian' call, the result type of
  // the lambda we give it changes.  So how does the `B' type argument
  // get picked?

  val bySchwartzianListSortsTP: List[List[Vector[String]]] = for {
    (ccord, i) <- decomposedSortKeys
  } yield (schwartzian[Vector[String], ccord.I]
             (unstructuredData)(v => ccord.k(v(i)))(ccord.fi))

  // `I' is the "pivot", how the function `k' result type and `fi'
  // order type relate to each other.  As seen above, this existential
  // type member is usually inferred as well as normal Scala types,
  // but you'll see it in type errors, and of course, can refer to it
  // as in `bySchwartzianListSortsTP' if you find it can't be
  // inferred.
  //
  // Sure, `I' is "really" changing for each call.  But, again, no one
  // cares that the `B' type parameter changes for each call, as long
  // as it's consistent between the 2nd and 3rd args in a given call.
  //
  // A sort specification algebra
  // ----------------------------
  //
  // Now we can abstract over the type of the sort key output, there
  // are some interesting things we could try with data
  // representation.  For example, a set of data rules about how
  // records are to be sorted.

  sealed abstract class SortType
  object SortType {
    case object CI extends SortType
    case object Dateish extends SortType
    case object Num extends SortType
  }

  type SortSpec = List[(SortType, Int)]

  // With a sample specification that sorts the columns left-to-right,
  // with the sort key associations we've been using.

  val mainLtoRsort: SortSpec =
    List((SortType.CI, 0), (SortType.Dateish, 1), (SortType.Num, 2))

  // It's simple enough to "interpret" each `SortType' to a
  // contravariant Coyoneda Order.

  def sortTypeOrd(s: SortType): CtCoyo[Order, String] = s match {
    case SortType.CI => CCOrder(caseInsensitively)
    case SortType.Dateish => CCOrder(parseDate)
    case SortType.Num => CCOrder(parseCommaNum) // like numerically4
  }

  // And then, similarly to `bySchwartzianListSorts', to combine one
  // of these `k's with a Vector lookup to produce a sort of records.

  def recItemOrd(i: Int, o: CtCoyo[Order, String])
      : CtCoyo[Order, Vector[String]] =
    o contramap (v => v(i))

  // Now what?  A `SortSpec' has several such values in it; how do we
  // combine them?
  //
  // Products
  // --------
  //
  // We're going to produce multiple values of type `CtCoyo[Order,
  // Vector[String]]', and we have to combine them in some way to
  // produce a final value of the same type.  We can take advantage of
  // a few things we know about `Order' itself:
  //
  // 1. There's a polymorphic Order product: `[A, B](Order[A],
  //    Order[B]): Order[(A, B)]'.
  //
  // 2. There's an Order[Unit].
  //
  // 3. The Order[Unit] is an identity for the product operator:
  //    `Order[(A, Unit)]' and `Order[(Unit, A)]' have the same sort
  //    behavior as the underlying Order[A].
  //
  // Given that, we can produce a contravariant Coyoneda order product
  // function that feeds the value under consideration to both
  // underlying functions, and the results to both orders, but then
  // forgets that the whole thing happened, without forgetting the new
  // type relationships.  And we can produce a base case, too.

  /** Forget the value, treat all as equal.  Note that this is exactly
    * the desired behavior for an empty `SortSpec'.
    */
  def unitOrd[A]: CtCoyo.Aux[Order, A, Unit] = CCOrder(a => ())

  // I use the `Aux' type to illustrate what we know about the `I'
  // type member in each case.  We'll drop it entirely when using
  // these functions.

  def ordFanout[A](l: CtCoyo[Order, A], r: CtCoyo[Order, A])
      : CtCoyo.Aux[Order, A, (l.I, r.I)] = {
    implicit val lfi: Order[l.I] = l.fi
    implicit val rfi: Order[r.I] = r.fi
    CCOrder(l.k &&& r.k)
  }

  // [1] https://en.wikipedia.org/wiki/Schwartzian_transform
}
