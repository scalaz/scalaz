package scalaz.example

import scalaz.{\/, ContravariantCoyoneda => CtCoyo, Monoid, Order}
import scalaz.std.anyVal._
import scalaz.std.function._
import scalaz.std.list._
import scalaz.std.option._
import scalaz.std.string._
import scalaz.std.tuple._
import scalaz.std.vector._
import scalaz.syntax.arrow._
import scalaz.syntax.monoid._
import scalaz.syntax.order._
import scalaz.syntax.show._
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

  // Or, really, maybe it has some structure.  That’s not important.
  // What matters is, I want to sort the data according to various
  // rules.

  def numerically1: Order[String] =
    Order.order((a, b) => parseCommaNum(a) ?|? parseCommaNum(b))

  // Which is a silly way to write this.  Let’s try again:

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
  // We’ll call `parseCommaNum' a “sort key” function.  Here’s that,
  // and the other two we’ll be using for this example.

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
  // on the argument type, `String', that will compare two values by
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
  // the “sort key” function gets applied twice for each *comparison*
  // between two elements, rather than once for each element.  A
  // typical sort can compare a given element many times, wasting the
  // work of calling the sort key function repeatedly.
  //
  // For simple functions, this doesn’t matter.  For complex sort key
  // functions, it can make a great deal of difference.  So we use the
  // Schwartzian transform:

  def schwartzian[A, B](xs: List[A])(f: A => B)(implicit B: Order[B]): List[A] =
    xs.map(a => (a, f(a)))
      .sortBy(_._2)(B.toScalaOrdering)
      .map(_._1)

  def nonschwartzian[A, B](xs: List[A])(f: A => B)(implicit B: Order[B]): List[A] =
    xs.sorted(Order.orderBy(f).toScalaOrdering)

  // The above two functions are guaranteed to return the same result for
  // pure `f', but `schwartzian' may be faster for complex `f'.  By
  // the simple expedient of separating the sort key from the Order
  // passed to the real sort algorithm, we guarantee that the sort key
  // function will only be called once per element.
  //
  // More at [1].
  //
  // Simple sort key usage
  // ---------------------
  //
  // It’s straightforward enough to use our sort key functions to sort
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

  // Fails to compile with the given type error.  That’s because the
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
  // result’s ordering.  That’s what happens here:

  val byOrdListSorts: List[List[Vector[String]]] = for {
    (ord, i) <- List((caseInsensitivelyOrd, 0),
                     (dateOrd, 1),
                     (numerically2, 2))
  } yield unstructuredData.sortBy(v => v(i))(ord.toScalaOrdering)

  // We can no longer use `schwartzian', though, because there is no
  // way to separate the sort key from the underlying Order.  We could
  // combine them all into a single ADT, but that would be closed, it
  // would be inconvenient to build a correct Order instance, and the
  // complication turns out to be unnecessary.  Contravariant co-Yoneda
  // is here to simplify our lives.
  //
  // Enter Contravariant co-Yoneda
  // -----------------------------
  //
  // Recall that sort keys are applied by contramap, as seen in the
  // definition of `numerically3'.  What if we represented that
  // contramap call as a type?  `ContravariantCoyoneda' can do that
  // for us.

  val numerically4: CtCoyo[Order, String] =
    CtCoyo(Order[Long \/ String])(parseCommaNum)

  // This separates the sort key and the underlying order, but the
  // sort key’s result type no longer appears!  It’s been made
  // *existential*, and the main feature of the
  // `ContravariantCoyoneda' structure, for our purposes, is that it
  // remembers that the output type of the function is exactly the
  // type of the `Order', *even though it’s forgotten what that type
  // is*.
  //
  // That’s enough to call `schwartzian': `schwartzian' doesn’t care
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

  // Now we’re ready.

  val bySchwartzianListSorts: List[List[Vector[String]]] = for {
    (ccord, i) <- decomposedSortKeys
  } yield schwartzian(unstructuredData)(v => ccord.k(v(i)))(ccord.fi)

  // But we know that for each `schwartzian' call, the result type of
  // the lambda we give it changes; for `caseInsensitively', it’s
  // `String', but for `parseCommaNum', it’s `Long \/ String', and so
  // on.  So how does the `B' type argument get picked?

  val bySchwartzianListSortsTP: List[List[Vector[String]]] = for {
    (ccord, i) <- decomposedSortKeys
  } yield (schwartzian[Vector[String], ccord.I]
             (unstructuredData)(v => ccord.k(v(i)))(ccord.fi))

  // `I' is the “pivot”, how the function `k' result type and `fi'
  // order type relate to each other.  As seen above, this existential
  // type member is usually inferred as well as normal Scala types,
  // but you’ll see it in type errors, and of course, can refer to it
  // with syntax like `someVar.I', as in `bySchwartzianListSortsTP',
  // if you find it can’t be inferred.
  //
  // Sure, `I' is “really” changing for each call.  But, again, no one
  // cares that the `B' type parameter changes for each call, as long
  // as it’s consistent between the 2nd and 3rd args in a given call.
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
  // with the sort key associations we’ve been using.

  val mainLtoRsort: SortSpec =
    List((SortType.CI, 0), (SortType.Dateish, 1), (SortType.Num, 2))

  // It’s simple enough to “interpret” each `SortType' to a
  // contravariant co-Yoneda Order.

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
  // We’re going to produce multiple values of type
  // `CtCoyo[Order, Vector[String]]', and we have to combine them in
  // some way to produce a final value of the same type.  We can take
  // advantage of a few things we know about `Order' itself:
  //
  // 1. There’s a polymorphic Order product, O_×:
  //    `[A, B](Order[A], Order[B]): Order[(A, B)]'.
  //
  // 2. There’s an Order[Unit], O_∅.
  //
  // 3. O_∅ is a left and right identity for
  //    O_×; i.e. `Order[(A, Unit)]' and `Order[(Unit, A)]',
  //    constructed from O_∅ and O_×, have the same sort behavior as
  //    the underlying Order[A].
  //
  // Given that, we can produce a contravariant co-Yoneda order
  // product function that feeds the value under consideration to both
  // underlying functions, and the results to both orders, but then
  // forgets that the whole thing happened, without forgetting the new
  // type relationships.  And we can produce a base case, too.

  /** Forget the value, treat all as equal.  Note that this is exactly
    * the desired behavior for an empty `SortSpec'.
    */
  def unitOrd[A]: CtCoyo.Aux[Order, A, Unit] = CCOrder(a => ())

  // I use the `Aux' type to illustrate what we know about the `I'
  // type member in each case.  We’ll drop it entirely when using
  // these functions.

  def ordFanout[A](l: CtCoyo[Order, A], r: CtCoyo[Order, A])
      : CtCoyo.Aux[Order, A, (l.I, r.I)] = {
    implicit val lfi: Order[l.I] = l.fi
    implicit val rfi: Order[r.I] = r.fi
    CCOrder(l.k &&& r.k)
  }

  // Now we have a base case, and an induction step.  I think we’re
  // ready.

  def sortSpecOrd(s: SortSpec): CtCoyo[Order, Vector[String]] =
    s.foldRight(unitOrd: CtCoyo[Order, Vector[String]]){(sti, acc) =>
      val (st, i) = sti
      ordFanout(recItemOrd(i, sortTypeOrd(st)), acc)
    }

  def sortDataBy(xs: List[Vector[String]], o: SortSpec)
      : List[Vector[String]] = {
    val coyo = sortSpecOrd(o)
    schwartzian(xs)(coyo.k)(coyo.fi)
  }

  val sortedBySpec: List[Vector[String]] =
    sortDataBy(unstructuredData, mainLtoRsort)

  println("sortedBySpec: " |+| sortedBySpec.shows)

  val sortedByNonCity: List[Vector[String]] =
    sortDataBy(unstructuredData, mainLtoRsort.tail)

  println("sortedByNonCity: " |+| sortedByNonCity.shows)

  // Hold on.  The types are changing again.  We started with an I of
  // `Unit', then had an I = `(someI, Unit)', then an I = `(anotherI,
  // (someI, Unit))'.  How come this works at all?
  //
  // Induction steps
  // ---------------
  //
  // The step function passed to the above fold is a logical induction
  // step.  It calls no functions that actually care about what any
  // `I' is; `ordFanout' only cares that it gets two functions and two
  // Orders, and each function’s return type matches up with its
  // associated order!  `ContravariantCoyoneda' acts as a bit of
  // scaffolding to maintain the proof as we perform each inductive
  // step.  The only place we know the type is in the `sortTypeOrd'
  // function body, and we throw it away before returning.
  //
  // So we end up using an arbitrary nesting of tuples in the ultimate
  // `I' that results, but it doesn’t matter, because each step of the
  // code that results knows exactly enough type information for this
  // to be sound.  If you’re wondering, here’s the type that got
  // erased for `mainLtoRsort':
  //
  // (String, ((Int, Option[(Int, Int)]) \/ String,
  //           (Long \/ String, Unit)))
  //
  // Let’s check out the `I's that must have been used for
  // `sortedBySpec' and `sortedByNonCity', via the unsafe `toString'
  // method.

  val mainLtoRcoyo: CtCoyo[Order, Vector[String]] =
    sortSpecOrd(mainLtoRsort)

  val mainLtoRtailcoyo: CtCoyo[Order, Vector[String]] =
    sortSpecOrd(mainLtoRsort.tail)

  println("list of mainLtoRcoyo.I: " |+|
            unstructuredData.map(r => mainLtoRcoyo.k(r).toString).shows)
  println("list of mainLtoRtailcoyo.I: " |+|
            unstructuredData.map(r => mainLtoRtailcoyo.k(r).toString).shows)

  // Digression: the monoid
  // ----------------------
  //
  // Something interesting about `sortSpecOrd': it’s completely
  // coincidental that I wrote it with a right fold.  It works just as
  // well with a left fold.

  def sortSpecOrdL(s: SortSpec): CtCoyo[Order, Vector[String]] =
    s.foldLeft(unitOrd: CtCoyo[Order, Vector[String]]){(acc, sti) =>
      val (st, i) = sti
      ordFanout(acc, recItemOrd(i, sortTypeOrd(st)))
    }

  // What does `I' look like then?  Let’s use the unsafe `toString'
  // again to see.

  val mainLtoRcoyoL: CtCoyo[Order, Vector[String]] =
    sortSpecOrdL(mainLtoRsort)

  println("list of mainLtoRcoyoL.I: " |+|
            unstructuredData.map(r => mainLtoRcoyoL.k(r).toString).shows)

  // Now the type looks like this:
  //
  // (((Unit, String),
  //   (Int, Option[(Int, Int)]) \/ String),
  //  Long \/ String)
  //
  // Yet, the resulting order is the same.  Compare to
  // `sortedByNonCity', which used the right-fold.

  val sortedByNonCityL: List[Vector[String]] = {
    val coyo = sortSpecOrdL(mainLtoRsort.tail)
    schwartzian(unstructuredData)(coyo.k)(coyo.fi)
  }

  println("sortedByNonCityL: " |+| sortedByNonCity.shows)

  // Our three properties of Order listed under “Products” now come
  // into play, in addition to a fourth.
  //
  // 4. Where a, b, and c are existential types, the
  //    `Order[((a, b), c)]' and `Order[(a, (b, c))]' as constructed
  //    with O_× are indistinguishable, i.e. O_× is associative.
  //
  // That `unitOrd' doesn’t change behavior of the sort, combined with
  // the 4th property, means that we can build a lawful monoid from
  // those two functions.

  implicit def ctCoyoOrdMonoid[A]: Monoid[CtCoyo[Order, A]] =
    Monoid instance (ordFanout(_, _), unitOrd)

  // And now we can build one more version of `sortSpecOrd' that has
  // cleaned up quite nicely.

  def sortSpecOrdF(s: SortSpec): CtCoyo[Order, Vector[String]] =
    s.foldMap{case (st, i) => recItemOrd(i, sortTypeOrd(st))}

  // “But how can this follow the monoid laws; it isn’t associative
  // because `I' changes depending on the order of the fold!”  Well,
  // you’re not allowed to care about that under the rules of
  // parametricity [2], just like you’re not allowed to test stack
  // depth in a function and claim that the changing results means the
  // functor identity law is violated for `Function1'.  It’s *some*
  // `I', and that’s all you get.  Nothing that actually knows how to
  // work with `I' in this result cares about the grouping of
  // combination, so it’s a monoid.
  //
  // How well does this generalize to `F's other than `Order'?  I
  // think the presence of a universally-quantified product satisfying
  // the 3rd and 4th properties above gives you a fanout-style
  // `Monoid', no problem.
  //
  // Knowing more about `I'
  // ----------------------
  //
  // Contravariant co-Yoneda isn’t *for* Order.  That’s just the
  // example I’ve shown here.  `F' is abstract for a reason.
  //
  // Let’s consider some kind of distributed arrangement for sorting:
  // we cut data into slices, deliver them – and the sort spec — to
  // each node, *they* do the transition to `I' and sort their
  // components, and we get it all back and merge the results.
  //
  // There are various type-safe binary format libraries, such as
  // scodec [3] and f0 [4].  Let’s simulate one of those.  Never mind
  // that this is phantom; assume that serialization and
  // deserialization methods are defined:

  trait Binfmt[A] {
    def describe: String
  }

  object Binfmt {
    def apply[A](s: String): Binfmt[A] = new Binfmt[A] {val describe = s}

    implicit val descLong: Binfmt[Long] = Binfmt("<Long>")
    implicit val descInt: Binfmt[Int] = Binfmt("<Int>")
    implicit val descString: Binfmt[String] = Binfmt("<String>")
    implicit val descUnit: Binfmt[Unit] = Binfmt("")
    implicit def descOption[A](implicit a: Binfmt[A])
        : Binfmt[Option[A]] = Binfmt("?<" |+| a.describe |+| ">")
    implicit def desc_\/[A, B](implicit a: Binfmt[A], b: Binfmt[B])
        : Binfmt[A \/ B] = Binfmt("<" |+| a.describe |+| "\\/"
                                      |+| b.describe |+| ">")
    implicit def desc2Tuple[A, B](implicit a: Binfmt[A], b: Binfmt[B])
        : Binfmt[(A, B)] = Binfmt(a.describe |+| b.describe)
  }

  // A bidirectional formatter, unlike `Order', does not have a
  // `Contravariant' instance.  Whether your `F' is contravariant is
  // irrelevant; contravariant co-Yoneda does all the work.
  //
  // Previously, we proved at every step that we had a function and an
  // Order that lined up.  Now, we need a function, Order, *and*
  // Binfmt that line up.  We didn’t make the previous functions
  // general enough, so let’s revisit them to include `Binfmt' as
  // simply as possible.  Using the `F' abstraction, you can make the
  // idea of which Binfmt typeclass abstract in your own designs; this
  // works well for an “open world” style `SortSpec', though our
  // example is closed.

  type BinOrd[A] = (Binfmt[A], Order[A])

  def CCBinOrd[A, B](f: A => B)(implicit b: Binfmt[B], o: Order[B])
      : CtCoyo.Aux[BinOrd, A, B] =
    CtCoyo[BinOrd, A, B]((b, o))(f)

  def sortTypeBinOrd(s: SortType): CtCoyo[BinOrd, String] = s match {
    case SortType.CI => CCBinOrd(caseInsensitively)
    case SortType.Dateish => CCBinOrd(parseDate)
    case SortType.Num => CCBinOrd(parseCommaNum)
  }

  // It turns out that recItemOrd is completely abstract:

  def recItem[F[_]](i: Int, o: CtCoyo[F, String])
      : CtCoyo[F, Vector[String]] =
    o contramap (v => v(i))

  // And, again, with a general notion of the underlying product
  // function and identity you could probably produce the monoid
  // generically, but for now:

  def unitBinOrd[A]: CtCoyo.Aux[BinOrd, A, Unit] = CCBinOrd(a => ())

  def binOrdFanout[A](l: CtCoyo[BinOrd, A], r: CtCoyo[BinOrd, A])
      : CtCoyo.Aux[BinOrd, A, (l.I, r.I)] = {
    implicit val (lfb, lfo) = l.fi
    implicit val (rfb, rfo) = r.fi
    CCBinOrd(l.k &&& r.k)
  }

  // I’ve chosen the `Binfmt' instances with a bit of care to preserve
  // their monoid-hood.  That isn’t a requirement for you to do this
  // kind of abstracting, though.

  implicit def ctCoyoBinOrdMonoid[A]: Monoid[CtCoyo[BinOrd, A]] =
    Monoid instance (binOrdFanout(_, _), unitBinOrd)

  def sortSpecBinOrdF(s: SortSpec): CtCoyo[BinOrd, Vector[String]] =
    s.foldMap{case (st, i) => recItem[BinOrd](i, sortTypeBinOrd(st))}

  // The drawback here is that I can’t just build a separate stack
  // willynilly for `Binfmt'.  I have to prove at each step that *the
  // same* `I' is used for the `Binfmt' and the `Order' for this to be
  // useful.  Once again, an idea of a fold step that can be fused
  // with the `Order' construction safely can be abstracted out here.

  val (binfmtdesc, finalsort) = {
    val bo = sortSpecBinOrdF(mainLtoRsort)
    (bo.fi._1.describe,
     schwartzian(unstructuredData)(bo.k)(bo.fi._2))
  }

  // For your edification, the binfmtdesc is:
  //
  //   <String><<Int>?<<Int><Int>>\/<String>><<Long>\/<String>>
  //
  // But you should just sbt "example/runMain
  // scalaz.example.ContravariantCoyonedaUsage" to see this and the
  // other prints in action.

  println("binfmtdesc: " |+| binfmtdesc)
  println("finalsort: " |+| finalsort.shows)

  // [1] https://en.wikipedia.org/wiki/Schwartzian_transform
  // [2] http://failex.blogspot.com/2013/06/fake-theorems-for-free.html
  // [3] https://github.com/scodec/scodec
  // [4] https://github.com/joshcough/f0
}
