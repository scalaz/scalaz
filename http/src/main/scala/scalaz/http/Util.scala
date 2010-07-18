package scalaz
package http

import collection.immutable.{Map, HashMap}
import Scalaz._
import java.net.URLDecoder.decode
import io.Codec

/**
 * Utility functions not specific to any particular context within HTTP.
 *
 * @author <a href="mailto:code@tmorris.net">Tony Morris</a>
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
      (pre, decode(post.mkString, Codec.UTF8.name).drop(1).toList)
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
    def apply[K, V](kvs: T[(K, V)])(implicit f: Foldable[T], s: Semigroup[SM[V]], md: Monad[SM]): Map[K, SM[V]]
  }

  /**
   * Indexes the given sequence of key/value pairs using an empty hash-map. The key/value pair type constructor must
   * support fold-left and the index is created in constant space.
   */
  def asHashMap[T[_], SM[_]] = new AsHashMap[T, SM] {
    def apply[K, V](kvs: T[(K, V)])(implicit f: Foldable[T], s: Semigroup[SM[V]], md: Monad[SM]) =
      asMap(new HashMap[K, SM[V]], kvs)
  }

  /**
   * Indexes the given sequence of key/value pairs using the given map. The key/value pair type constructor must support
   * fold-left and the index is created in constant space.
   */
  def asMap[T[_], SM[_], K, V](e: Map[K, SM[V]], kvs: T[(K, V)])
                   (implicit f: Foldable[T], s: Semigroup[SM[V]], md: Monad[SM]): Map[K, SM[V]] =
      f.foldLeft[(K, V), Map[K, SM[V]]](kvs, e, (m, kv) => m + ((kv._1, m.get(kv._1) match {
        case None => md.pure(kv._2)
        case Some(vv) => s.append(md.pure(kv._2), vv)
      })))

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
     * Creates a string from the given non-empty list.
     */
    implicit def NonEmptyListString[A](xs: NonEmptyList[Char]): String = xs.list.mkString
  }

  object Digits {
    /**
     * Converts the given long value to a sequence of digits.
     */
    def longDigits[T[_]](n: Long)(implicit f: Foldable[T], p: Pure[T], e : Empty[T], plus : Plus[T], m : Monoid[T[Digit]]): T[Digit] = {
      val xxx = n.unfold((b: Long) => if (b == 0) None else Some(b % 10L: Digit, b / 10L))
      // TODO this is just a reverse, which should get into scalaz4
      if (n == 0) p.pure(Digit._0) else f.foldLeft[Digit, T[Digit]](xxx, e.empty[Digit], (b, a) => plus.plus(p.pure(a), b))
    }

    /**
     * Converts the given sequence of digits to a long value.
     */
    def digitsLong[T[_]](ds: T[Digit])(implicit f: Foldable[T]) =
      f.foldLeft[Digit, Long](ds, 0L, (a, b) => a * 10L + b)
  }
}
