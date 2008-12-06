/*-------------------------------------------------------------------------*\
**  ScalaCheck                                                             **
**  Copyright (c) 2007-2008 Rickard Nilsson. All rights reserved.          **
**  http://www.scalacheck.org                                              **
**                                                                         **
**  This software is released under the terms of the Revised BSD License.  **
**  There is NO WARRANTY. See the file LICENSE for the full text.          **
\*-------------------------------------------------------------------------*/

package org.scalacheck.util

trait Builder[C[_], T] {
  def +=(x: T)
  def finalise: C[T]
}

trait Buildable[C[_]] {
  def builder[T]: Builder[C,T]
  def fromIterable[T](it: Iterable[T]): C[T] = {
    val b = builder[T]
    val elems = it.elements
    while(elems.hasNext) b += elems.next
    b.finalise
  }
}

object Buildable {

  import scala.collection._
  import java.util.ArrayList

  implicit object buildableList extends Buildable[List] {
    def builder[T] = new Builder[List,T] {
      val buf = new scala.collection.mutable.ListBuffer[T]
      def +=(x: T) = buf += x
      def finalise = buf.toList
    }
  }

  implicit object buildableStream extends Buildable[Stream] {
    def builder[T] = new Builder[Stream,T] {
      val buf = new scala.collection.mutable.ListBuffer[T]
      def +=(x: T) = buf += x
      def finalise = Stream.fromIterator(buf.elements)
    }
  }

  implicit object buildableArray extends Buildable[Array] {
    def builder[T] = new Builder[Array,T] {
      val buf = new scala.collection.mutable.ArrayBuffer[T]
      def +=(x: T) = buf += x
      def finalise = {
        val arr = new Array[T](buf.size)
        buf.copyToArray(arr, 0)
        arr
      }
    }
  }

  implicit object buildableSet extends Buildable[Set] {
    def builder[T] = new Builder[Set,T] {
      val buf = mutable.Set.empty[T]
      def +=(x: T) = buf += x
      def finalise = buf
    }
  }

  implicit object buildableArrayList extends Buildable[ArrayList] {
    def builder[T] = new Builder[ArrayList,T] {
      val al = new ArrayList[T]
      def +=(x: T) = al.add(x)
      def finalise = al
    }
  }

}
