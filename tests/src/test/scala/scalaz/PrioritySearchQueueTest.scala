package scalaz

import org.scalacheck.Arbitrary
import org.scalacheck.Prop.forAll
import scala.annotation.tailrec
import scalaz.scalacheck.ScalaCheckBinding._
import scalaz.scalacheck.ScalazArbitrary._
import scalaz.std.anyVal._
import scalaz.std.list._
import scalaz.std.set._
import scalaz.syntax.foldable._

object PrioritySearchQueueTest extends SpecLite {
  case class Entry(priority: Int, key: Int)
  object Entry {
    implicit val arb: Arbitrary[Entry] =
      Apply[Arbitrary].apply2(implicitly[Arbitrary[Int]], implicitly[Arbitrary[Int]])(Entry(_, _))

    implicit val order: Order[Entry] =
      Order[Int].contramap(_.key)

    implicit val show: Show[Entry] =
      Show.shows[Entry](e => s"(${e.priority}, ${e.key})")
  }

  private def emptyQueue: PrioritySearchQueue[Entry, Int, Int] =
    PrioritySearchQueue.empty(_.priority, _.key)

  private def queueFrom[F[_]: Foldable](entries: F[Entry]): PrioritySearchQueue[Entry, Int, Int] =
    entries.foldLeft(emptyQueue)((acc, e) => acc.insert(e)._1)

  "contains everything that was inserted" ! forAll { (entries: List[Entry]) =>
    val queue = queueFrom(entries)
    for (e <- entries) {
      assert(queue.containsKey(e.key))
    }
  }

  "contains exactly the inserted elements" ! forAll { (entries: IList[Entry]) =>
    val queue = queueFrom(entries)
    queue.toUnsortedList.toSet must_=== entries.distinct.toList.toSet
  }

  "does not contain a key after it has been removed" ! forAll { (entries: IList[Entry]) =>
    val distinctEntries = entries.distinct.toList
    var queue = queueFrom(distinctEntries)
    for (e <- distinctEntries) {
      val (q, removed) = queue.removeByKey(e.key)
      removed must_=== Maybe.just(e)
      assert(!q.containsKey(e.key))
      queue = q
    }
    assert(queue.isEmpty)
  }

  "unchanged after removing an absent element" ! forAll { (entries: List[Entry]) =>
    val queue = queueFrom(entries)
    for (e <- entries) {
      val (q, _) = queue.removeByKey(e.key)
      q.removeByKey(e.key)._1.toUnsortedIList must_=== q.toUnsortedIList
    }
  }

  "deleteMin removes minimum" ! forAll { (keys: List[Int]) =>
    // entries with the same priority and different keys
    val entries = keys.distinct.map(Entry(1, _))

    @tailrec def go(queue: PrioritySearchQueue[Entry, Int, Int]): Unit =
      queue.minimum match {
        case Maybe.Just(min) =>
          val queue1 = queue.deleteMin
          assert(!queue1.containsKey(min.key))
          go(queue1)
        case Maybe.Empty() =>
      }

    go(queueFrom(entries))
  }

  "iterated deleteMin produces a sequence sorted by priority" ! forAll { (entries: IList[Entry]) =>
    import scala.collection.mutable.Buffer

    @tailrec
    def toSortedList(acc: Buffer[Entry], queue: PrioritySearchQueue[Entry, Int, Int]): List[Entry] =
      queue.minimum match {
        case Maybe.Just(entry) => toSortedList(acc += entry, queue.deleteMin)
        case Maybe.Empty() => acc.toList
      }

    val distinctEntries = entries.distinct
    val sorted = toSortedList(Buffer.empty[Entry], queueFrom(distinctEntries))
    sorted must_=== distinctEntries.toList.sortBy(e => (e.priority, e.key))
  }

  "deleteLt" ! forAll { (entries: IList[Entry]) =>
    val distinctEntries = entries.distinct
    val queue = queueFrom(distinctEntries)
    val pivot = distinctEntries.headMaybe.map(_.priority).getOrElse(0)
    val retained = queue.deleteLt(pivot)
    assert(retained.toUnsortedList.forall(_.priority >= pivot))
    retained.size must_=== distinctEntries.count(_.priority >= pivot)
  }

  "deleteLte" ! forAll { (entries: IList[Entry]) =>
    val distinctEntries = entries.distinct
    val queue = queueFrom(distinctEntries)
    val pivot = distinctEntries.headMaybe.map(_.priority).getOrElse(0)
    val retained = queue.deleteLte(pivot)
    assert(retained.toUnsortedList.forall(_.priority > pivot))
    retained.size must_=== distinctEntries.count(_.priority > pivot)
  }

  "splitBeforePrio" ! forAll { (entries: IList[Entry]) =>
    val distinctEntries = entries.distinct
    val queue = queueFrom(distinctEntries)
    val pivot = distinctEntries.headMaybe.map(_.priority).getOrElse(0)
    val (left, right) = queue.splitBeforePrio(pivot)
    assert(left.toUnsortedList.forall(_.priority < pivot))
    left.size must_=== distinctEntries.count(_.priority < pivot)
    right.size must_=== distinctEntries.length - left.size
  }

  "splitAfterPrio" ! forAll { (entries: IList[Entry]) =>
    val distinctEntries = entries.distinct
    val queue = queueFrom(distinctEntries)
    val pivot = distinctEntries.headMaybe.map(_.priority).getOrElse(0)
    val (left, right) = queue.splitAfterPrio(pivot)
    assert(left.toUnsortedList.forall(_.priority <= pivot))
    left.size must_=== distinctEntries.count(_.priority <= pivot)
    right.size must_=== distinctEntries.length - left.size
  }
}
