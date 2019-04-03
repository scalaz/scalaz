package scalaz
package example

import Scalaz._

object IListUsage extends App {

  // Construct using elements
  val ns = IList(1, 2, 3)

  // Construct using cells
  val ns2 = ICons(1, ICons(2, ICons(3, INil())))
  val ns3 = 1 :: 2 :: 3 :: INil()

  // Construct from something else
  val ns4 = IList.fromList(List(1,3,4))
  val ns5 = IList.fromOption(Some(2))

  // Empty IList
  val e1 = INil[String]()
  val e2 = IList.empty[String]

  // IList is invariant; these won't compile
  // "abc" :: ns
  // e1 ++ ns

  // You can widen explicitly if you want to accomplish the above
  val any = "abc" :: ns.widen[Any]

  // List operations are generally the same as stdlib List
  val rev = ns.reverse
  val sum = ns.foldLeft(0)(_ + _)

  // But they are all total
  val tail = ns.tailOption
  val prod = ns.reduceLeftOption(_ * _)

  // Destructure with uncons
  val s1 = ns.uncons("empty", (h, t) => "head is %s and tail is %s".format(h, t))

  // Destructure with matching
  val s2 = ns match {
    case INil() => "empty"
    case ICons(h, t) => "head is %s and tail is %s".format(h, t)
  }

  // Same typeclass instances as List
  val xprod = (IList(1, 2) |@| IList(true, false)).tupled
  val unit  = 33.point[IList]
  val less  = IList(1,2,3) < IList(1,2,4)
  val trav  = IList(1, 2, 3).traverse(_.some)

  // Turn into something more familiar
  val lst = ns.toList
  val vec = ns.toVector
  val str = ns.toStream

  // Or less familiar
  val estr = ns.toEphemeralStream
  val zmap = ns.map(n => (n, "x" * n)).toMap // Int ==>> String

}
