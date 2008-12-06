/*-------------------------------------------------------------------------*\
**  ScalaCheck                                                             **
**  Copyright (c) 2007-2008 Rickard Nilsson. All rights reserved.          **
**  http://www.scalacheck.org                                              **
**                                                                         **
**  This software is released under the terms of the Revised BSD License.  **
**  There is NO WARRANTY. See the file LICENSE for the full text.          **
\*-------------------------------------------------------------------------*/

package org.scalacheck

import util.Buildable

sealed abstract class Shrink[T] {
  def shrink(x: T): Stream[T]
}

object Shrink {

  import Stream.{cons, empty}
  import scala.collection._
  import java.util.ArrayList

  /** Shrink instance of container */
  private def shrinkContainer[C[_],T](implicit v: C[T] => Collection[T], s: Shrink[T],
    b: Buildable[C]
  ): Shrink[C[T]] = Shrink { xs: C[T] =>

    def interleave(xs: Stream[Stream[T]], ys: Stream[Stream[T]]): Stream[Stream[T]] =
      (xs,ys) match {
        case (xs,ys) if xs.isEmpty => ys
        case (xs,ys) if ys.isEmpty => xs
        case (cons(x,xs),cons(y,ys)) => cons(x, cons(y, interleave(xs,ys)))
      }

    def removeChunks(n: Int, xs: Stream[T]): Stream[Stream[T]] =
      if(xs.isEmpty) empty
      else if(xs.tail.isEmpty) cons(empty, empty)
      else {
        val n1 = n / 2
        val n2 = n - n1
        lazy val xs1 = xs.take(n1)
        lazy val xs2 = xs.drop(n1)
        lazy val xs3 =
          for(ys1 <- removeChunks(n1,xs1) if !ys1.isEmpty) yield ys1 append xs2
        lazy val xs4 =
          for(ys2 <- removeChunks(n2,xs2) if !ys2.isEmpty) yield xs1 append ys2

        cons(xs1, cons(xs2, interleave(xs3,xs4)))
      }

    def shrinkOne(zs: Stream[T]): Stream[Stream[T]] =
      if(zs.isEmpty) empty
      else {
        val x = zs.head
        val xs = zs.tail
        (for(y <- shrink(x)) yield cons(y,xs)) append
        (for(ys <- shrinkOne(xs)) yield cons(x,ys))
      }

    val ys = v(xs)
    val zs = ys.toStream
    removeChunks(ys.size,zs).append(shrinkOne(zs)).map(b.fromIterable[T])

  }

  def apply[T](s: T => Stream[T]): Shrink[T] = new Shrink[T] {
    override def shrink(x: T) = s(x)
  }

  def shrink[T](x: T)(implicit s: Shrink[T]): Stream[T] = s.shrink(x)

  /** Default shrink instance */
  implicit def shrinkAny[T]: Shrink[T] = Shrink(x => empty)

  /** Shrink instance of integer */
  implicit lazy val shrinkInt: Shrink[Int] = Shrink { n =>

    def iterate[T](f: T => T, x: T): Stream[T] = {
      val y = f(x)
      cons(y, iterate(f,y))
    }

    if(n == 0) empty
    else {
      val ns = cons(0, iterate((_:Int)/2, n).takeWhile(_ != 0).map(n - _))
      if(n < 0) cons(-n,ns) else ns
    }
  }

  /** Shrink instance of String */
  implicit lazy val shrinkString: Shrink[String] = Shrink { s =>
    shrinkList[Char].shrink(s.toList).map(_.mkString)
  }

  /** Shrink instance of Option */
  implicit def shrinkOption[T](implicit s: Shrink[T]): Shrink[Option[T]] =
    Shrink {
      case None    => empty
      case Some(x) => cons(None, for(y <- shrink(x)) yield Some(y))
    }

  /** Shrink instance of List */
  implicit def shrinkList[T](implicit s: Shrink[T]): Shrink[List[T]] =
    shrinkContainer[List,T]

  /** Shrink instance of Stream */
  implicit def shrinkStream[T](implicit s: Shrink[T]): Shrink[Stream[T]] =
    shrinkContainer[Stream,T]

  /** Shrink instance of Array */
  implicit def shrinkArray[T](implicit s: Shrink[T]): Shrink[Array[T]] =
    shrinkContainer[Array,T]

  /** Shrink instance of Set */
  implicit def shrinkSet[T](implicit s: Shrink[T]): Shrink[Set[T]] =
    shrinkContainer[Set,T]

  /** Shrink instance of ArrayList */
  implicit def shrinkArrayList[T](implicit s: Shrink[T]): Shrink[ArrayList[T]] =
    shrinkContainer[ArrayList,T](al => new jcl.ArrayList(al), s, 
      Buildable.buildableArrayList)

  /** Shrink instance of 2-tuple */
  implicit def shrinkTuple2[T1,T2](implicit
    s1: Shrink[T1], s2: Shrink[T2]
  ): Shrink[(T1,T2)] =
    Shrink { case (t1,t2) =>
      (for(x1 <- shrink(t1)) yield (x1, t2)) append
      (for(x2 <- shrink(t2)) yield (t1, x2))
    }

  /** Shrink instance of 3-tuple */
  implicit def shrinkTuple3[T1,T2,T3](implicit
    s1: Shrink[T1], s2: Shrink[T2], s3: Shrink[T3]
  ): Shrink[(T1,T2,T3)] =
    Shrink { case (t1,t2,t3) =>
      (for(x1 <- shrink(t1)) yield (x1, t2, t3)) append
      (for(x2 <- shrink(t2)) yield (t1, x2, t3)) append
      (for(x3 <- shrink(t3)) yield (t1, t2, x3))
    }

  /** Shrink instance of 4-tuple */
  implicit def shrinkTuple4[T1,T2,T3,T4](implicit
    s1: Shrink[T1], s2: Shrink[T2], s3: Shrink[T3], s4: Shrink[T4]
  ): Shrink[(T1,T2,T3,T4)] =
    Shrink { case (t1,t2,t3,t4) =>
      (for(x1 <- shrink(t1)) yield (x1, t2, t3, t4)) append
      (for(x2 <- shrink(t2)) yield (t1, x2, t3, t4)) append
      (for(x3 <- shrink(t3)) yield (t1, t2, x3, t4)) append
      (for(x4 <- shrink(t4)) yield (t1, t2, t3, x4))
    }

  /** Shrink instance of 5-tuple */
  implicit def shrinkTuple5[T1,T2,T3,T4,T5](implicit
    s1: Shrink[T1], s2: Shrink[T2], s3: Shrink[T3], s4: Shrink[T4],
    s5: Shrink[T5]
  ): Shrink[(T1,T2,T3,T4,T5)] =
    Shrink { case (t1,t2,t3,t4,t5) =>
      (for(x1 <- shrink(t1)) yield (x1, t2, t3, t4, t5)) append
      (for(x2 <- shrink(t2)) yield (t1, x2, t3, t4, t5)) append
      (for(x3 <- shrink(t3)) yield (t1, t2, x3, t4, t5)) append
      (for(x4 <- shrink(t4)) yield (t1, t2, t3, x4, t5)) append
      (for(x5 <- shrink(t5)) yield (t1, t2, t3, t4, x5))
    }

  /** Shrink instance of 6-tuple */
  implicit def shrinkTuple6[T1,T2,T3,T4,T5,T6](implicit
    s1: Shrink[T1], s2: Shrink[T2], s3: Shrink[T3], s4: Shrink[T4],
    s5: Shrink[T5], s6: Shrink[T6]
  ): Shrink[(T1,T2,T3,T4,T5,T6)] =
    Shrink { case (t1,t2,t3,t4,t5,t6) =>
      (for(x1 <- shrink(t1)) yield (x1, t2, t3, t4, t5, t6)) append
      (for(x2 <- shrink(t2)) yield (t1, x2, t3, t4, t5, t6)) append
      (for(x3 <- shrink(t3)) yield (t1, t2, x3, t4, t5, t6)) append
      (for(x4 <- shrink(t4)) yield (t1, t2, t3, x4, t5, t6)) append
      (for(x5 <- shrink(t5)) yield (t1, t2, t3, t4, x5, t6)) append
      (for(x6 <- shrink(t6)) yield (t1, t2, t3, t4, t5, x6))
    }

  /** Shrink instance of 7-tuple */
  implicit def shrinkTuple7[T1,T2,T3,T4,T5,T6,T7](implicit
    s1: Shrink[T1], s2: Shrink[T2], s3: Shrink[T3], s4: Shrink[T4],
    s5: Shrink[T5], s6: Shrink[T6], s7: Shrink[T7]
  ): Shrink[(T1,T2,T3,T4,T5,T6,T7)] =
    Shrink { case (t1,t2,t3,t4,t5,t6,t7) =>
      (for(x1 <- shrink(t1)) yield (x1, t2, t3, t4, t5, t6, t7)) append
      (for(x2 <- shrink(t2)) yield (t1, x2, t3, t4, t5, t6, t7)) append
      (for(x3 <- shrink(t3)) yield (t1, t2, x3, t4, t5, t6, t7)) append
      (for(x4 <- shrink(t4)) yield (t1, t2, t3, x4, t5, t6, t7)) append
      (for(x5 <- shrink(t5)) yield (t1, t2, t3, t4, x5, t6, t7)) append
      (for(x6 <- shrink(t6)) yield (t1, t2, t3, t4, t5, x6, t7)) append
      (for(x7 <- shrink(t7)) yield (t1, t2, t3, t4, t5, t6, x7))
    }

  /** Shrink instance of 8-tuple */
  implicit def shrinkTuple8[T1,T2,T3,T4,T5,T6,T7,T8](implicit
    s1: Shrink[T1], s2: Shrink[T2], s3: Shrink[T3], s4: Shrink[T4],
    s5: Shrink[T5], s6: Shrink[T6], s7: Shrink[T7], s8: Shrink[T8]
  ): Shrink[(T1,T2,T3,T4,T5,T6,T7,T8)] =
    Shrink { case (t1,t2,t3,t4,t5,t6,t7,t8) =>
      (for(x1 <- shrink(t1)) yield (x1, t2, t3, t4, t5, t6, t7, t8)) append
      (for(x2 <- shrink(t2)) yield (t1, x2, t3, t4, t5, t6, t7, t8)) append
      (for(x3 <- shrink(t3)) yield (t1, t2, x3, t4, t5, t6, t7, t8)) append
      (for(x4 <- shrink(t4)) yield (t1, t2, t3, x4, t5, t6, t7, t8)) append
      (for(x5 <- shrink(t5)) yield (t1, t2, t3, t4, x5, t6, t7, t8)) append
      (for(x6 <- shrink(t6)) yield (t1, t2, t3, t4, t5, x6, t7, t8)) append
      (for(x7 <- shrink(t7)) yield (t1, t2, t3, t4, t5, t6, x7, t8)) append
      (for(x8 <- shrink(t8)) yield (t1, t2, t3, t4, t5, t6, t7, x8))
    }

  /** Shrink instance of 9-tuple */
  implicit def shrinkTuple9[T1,T2,T3,T4,T5,T6,T7,T8,T9](implicit
    s1: Shrink[T1], s2: Shrink[T2], s3: Shrink[T3], s4: Shrink[T4],
    s5: Shrink[T5], s6: Shrink[T6], s7: Shrink[T7], s8: Shrink[T8],
    s9: Shrink[T9]
  ): Shrink[(T1,T2,T3,T4,T5,T6,T7,T8,T9)] =
    Shrink { case (t1,t2,t3,t4,t5,t6,t7,t8,t9) =>
      (for(x1 <- shrink(t1)) yield (x1, t2, t3, t4, t5, t6, t7, t8, t9)) append
      (for(x2 <- shrink(t2)) yield (t1, x2, t3, t4, t5, t6, t7, t8, t9)) append
      (for(x3 <- shrink(t3)) yield (t1, t2, x3, t4, t5, t6, t7, t8, t9)) append
      (for(x4 <- shrink(t4)) yield (t1, t2, t3, x4, t5, t6, t7, t8, t9)) append
      (for(x5 <- shrink(t5)) yield (t1, t2, t3, t4, x5, t6, t7, t8, t9)) append
      (for(x6 <- shrink(t6)) yield (t1, t2, t3, t4, t5, x6, t7, t8, t9)) append
      (for(x7 <- shrink(t7)) yield (t1, t2, t3, t4, t5, t6, x7, t8, t9)) append
      (for(x8 <- shrink(t8)) yield (t1, t2, t3, t4, t5, t6, t7, x8, t9)) append
      (for(x9 <- shrink(t9)) yield (t1, t2, t3, t4, t5, t6, t7, t8, x9))
    }


  //// workarounds for Scala bug #298. ////

  implicit lazy val shrinkIntList: Shrink[List[Int]] = shrinkList[Int]
  implicit lazy val shrinkBooleanList: Shrink[List[Boolean]] = shrinkList[Boolean]
  implicit lazy val shrinkStringList: Shrink[List[String]] = shrinkList[String]
  implicit lazy val shrinkDoubleList: Shrink[List[Double]] = shrinkList[Double]

  implicit lazy val shrinkIntStream: Shrink[Stream[Int]] = shrinkStream[Int]
  implicit lazy val shrinkBooleanStream: Shrink[Stream[Boolean]] = shrinkStream[Boolean]
  implicit lazy val shrinkStringStream: Shrink[Stream[String]] = shrinkStream[String]
  implicit lazy val shrinkDoubleStream: Shrink[Stream[Double]] = shrinkStream[Double]

  implicit lazy val shrinkIntArray: Shrink[Array[Int]] = shrinkArray[Int]
  implicit lazy val shrinkBooleanArray: Shrink[Array[Boolean]] = shrinkArray[Boolean]
  implicit lazy val shrinkStringArray: Shrink[Array[String]] = shrinkArray[String]
  implicit lazy val shrinkDoubleArray: Shrink[Array[Double]] = shrinkArray[Double]

  implicit lazy val shrinkIntSet: Shrink[Set[Int]] = shrinkSet[Int]
  implicit lazy val shrinkBooleanSet: Shrink[Set[Boolean]] = shrinkSet[Boolean]
  implicit lazy val shrinkStringSet: Shrink[Set[String]] = shrinkSet[String]
  implicit lazy val shrinkDoubleSet: Shrink[Set[Double]] = shrinkSet[Double]

  implicit lazy val shrinkIntArrayList: Shrink[ArrayList[Int]] = shrinkArrayList[Int]
  implicit lazy val shrinkBooleanArrayList: Shrink[ArrayList[Boolean]] = shrinkArrayList[Boolean]
  implicit lazy val shrinkStringArrayList: Shrink[ArrayList[String]] = shrinkArrayList[String]
  implicit lazy val shrinkDoubleArrayList: Shrink[ArrayList[Double]] = shrinkArrayList[Double]

  implicit lazy val shrinkIntOption: Shrink[Option[Int]] = shrinkOption[Int]
  implicit lazy val shrinkBooleanOption: Shrink[Option[Boolean]] = shrinkOption[Boolean]
  implicit lazy val shrinkStringOption: Shrink[Option[String]] = shrinkOption[String]
  implicit lazy val shrinkDoubleOption: Shrink[Option[Double]] = shrinkOption[Double]

  implicit lazy val shrinkIntTuple2: Shrink[Tuple2[Int,Int]] = shrinkTuple2[Int,Int]
  implicit lazy val shrinkBooleanTuple2: Shrink[Tuple2[Boolean,Boolean]] = shrinkTuple2[Boolean,Boolean]
  implicit lazy val shrinkStringTuple2: Shrink[Tuple2[String,String]] = shrinkTuple2[String,String]
  implicit lazy val shrinkDoubleTuple2: Shrink[Tuple2[Double,Double]] = shrinkTuple2[Double,Double]

  implicit lazy val shrinkIntTuple3: Shrink[Tuple3[Int,Int,Int]] = shrinkTuple3[Int,Int,Int]
  implicit lazy val shrinkBooleanTuple3: Shrink[Tuple3[Boolean,Boolean,Boolean]] = shrinkTuple3[Boolean,Boolean,Boolean]
  implicit lazy val shrinkStringTuple3: Shrink[Tuple3[String,String,String]] = shrinkTuple3[String,String,String]
  implicit lazy val shrinkDoubleTuple3: Shrink[Tuple3[Double,Double,Double]] = shrinkTuple3[Double,Double,Double]

  implicit lazy val shrinkIntTuple4: Shrink[Tuple4[Int,Int,Int,Int]] = shrinkTuple4[Int,Int,Int,Int]
  implicit lazy val shrinkBooleanTuple4: Shrink[Tuple4[Boolean,Boolean,Boolean,Boolean]] = shrinkTuple4[Boolean,Boolean,Boolean,Boolean]
  implicit lazy val shrinkStringTuple4: Shrink[Tuple4[String,String,String,String]] = shrinkTuple4[String,String,String,String]
  implicit lazy val shrinkDoubleTuple4: Shrink[Tuple4[Double,Double,Double,Double]] = shrinkTuple4[Double,Double,Double,Double]
}
