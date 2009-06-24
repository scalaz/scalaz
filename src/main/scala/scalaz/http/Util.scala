package scalaz.http

import collection.immutable.{Map, HashMap}
import Scalaz._
import java.net.URLDecoder.decode

/**
 * Utility functions not specific to any particular context within HTTP.
 *
 * @author <a href="mailto:code@tmorris.net">Tony Morris</a>
 * @version $LastChangedRevision < br >
 *          $LastChangedDate$<br>
 *          $LastChangedBy$
 */
object Util {
  /**
   * Splits the given argument by ampersand (<code>&</code>) then each list again by the equals sign (<code>=</code>).
   * e.g. <code>parameters("a=b&c=d&e=f") == [("a", "b"), ("c", "d"), ("e", "f")]</code> 
   */
  def parameters(p: List[Char]): List[(List[Char], List[Char])] = {
    val x = p.selectSplit(_ != '&')
    // TODO Get !- back in scalaz and use that
    //    x map (_ !- (_ == '=') match {
    //      case (a, b) => {
    //        (a, decode(b.mkString).drop(1).toList)
    //      }
    //    })

    x map (chars => {
      val pre = chars takeWhile (_ != '=')
      val post = chars dropWhile (_ != '=')
      (pre, decode(post.mkString).drop(1).toList)
    })
  }

  /**
   * Encodes the given parameters by inserting an ampersand (<code>&</code>) between each pair and an equals sign
   * (<code>=</code>) with each pair.
   */
  def encodeParameters(p: (List[Char], List[Char])*): String =
    p.toList map {case (k, v) => k ::: '=' :: v} intersperse List('&') flatMap (x => x) mkString

  /**
   * Encodes the given parameters by inserting an ampersand (<code>&</code>) between each pair and an equals sign
   * (<code>=</code>) with each pair.
   */
  def encode(p: (String, String)*): String = encodeParameters(p map {case (k, v) => (k.toList, v.toList)}: _*)

  /**
   * Returns a map of key/value pairs where only the first value for the given key is available.
   */
  def mapHeads[K, C](p: Map[K, NonEmptyList[C]]) = p.transform((h, v) => v.head)

  trait AsHashMap[T[_], SM[_]] {
    def apply[K, V](kvs: T[(K, V)])(implicit f: FoldLeft[T], s: Semigroup[SM[V]], md: Monad[SM]): Map[K, SM[V]]
  }

  /**
   * Indexes the given sequence of key/value pairs using an empty hash-map. The key/value pair type constructor must
   * support fold-left and the index is created in constant space.
   */
  def asHashMap[T[_], SM[_]] = new AsHashMap[T, SM] {
    def apply[K, V](kvs: T[(K, V)])(implicit f: FoldLeft[T], s: Semigroup[SM[V]], md: Monad[SM]) =
      asMap[T, SM](new HashMap[K, SM[V]], kvs)
  }

  /**
   * Indexes the given sequence of key/value pairs using the given map. The key/value pair type constructor must support
   * fold-left and the index is created in constant space.
   */
  def asMap[T[_], SM[_]] = new {
    def apply[K, V](e: Map[K, SM[V]], kvs: T[(K, V)])
                   (implicit f: FoldLeft[T], s: Semigroup[SM[V]], md: Monad[SM]): Map[K, SM[V]] =
      f.foldLeft[Map[K, SM[V]], (K, V)](kvs, e, (m, kv) => m + ((kv._1, m.get(kv._1) match {
        case None => md.pure(kv._2)
        case Some(vv) => s.append(md.pure(kv._2), vv)
      })))
  }

  object Nel {
    /**
     * Creates a list from a potential non-empty list.
     */
    implicit def OptionNonEmptyListList[A](as: Option[NonEmptyList[A]]): List[A] = as match {
      case None => Nil
      case Some(x) => x.head :: x.tail
    }

    /**
     * Creates a list from the given non-empty list.
     */
    implicit def NonEmptyListList[A](xs: NonEmptyList[A]): List[A] = xs.list

    /**
     * Creates a potential non-empty list. If the given list is empty, then <code>None</code>.
     */
    implicit def NonEmptyListOptionList[A](as: List[A]): Option[NonEmptyList[A]] = as match {
      case Nil => None
      case a :: as => Some(NonEmptyList.nel(a, as))
    }

    /**
     * Creates a string from the given non-empty list.
     */
    implicit def NonEmptyListString[A](xs: NonEmptyList[Char]): String = scala.List.toString(xs.toList)
  }

  object Digits {
    /**
     * Converts the given long value to a sequence of digits.
     */
    def longDigits[T[_]](n: Long)(implicit f: FoldLeft[T], p: Pure[T], e : Empty[T], plus : Plus[T], m : Monoid[T[Digit]]): T[Digit] = {
      val xxx = n.unfold[T]((b: Long) => if (b == 0) None else Some(b % 10L: Digit, b / 10L))
      // TODO this is just a reverse, which should get into scalaz4
      if (n == 0) p.pure(_0) else f.foldLeft[T[Digit], Digit](xxx, e.empty[Digit], (b, a) => plus.plus(p.pure(a), b))
    }

    /**
     * Converts the given sequence of digits to a long value.
     */
    def digitsLong[T[_]](ds: T[Digit])(implicit f: FoldLeft[T]) =
      f.foldLeft[Long, Digit](ds, 0L, (a, b) => a * 10L + b)
  }
}
