package scalaz

import data._

sealed trait Order[A] {
  val order: A => A => Ordering

  def isGT: A => A => Boolean =
    a1 => a2 => order(a1)(a2) == GT

  def isLT: A => A => Boolean =
    a1 => a2 => order(a1)(a2) == LT

  def isEQ: A => A => Boolean =
    a1 => a2 => order(a1)(a2) == EQ

  def isLTE: A => A => Boolean =
    a1 => a2 => order(a1)(a2) != GT

  def isGTE: A => A => Boolean =
    a1 => a2 => order(a1)(a2) != LT

  def min(a1: A)(a2: A): A =
    if (order(a1)(a2) != GT)
      a1
    else a2

  def max(a1: A)(a2: A): A =
    if (order(a1)(a2) != LT)
      a1
    else a2

  def contramap[B](f: B => A): Order[B] =
    Order.order(b1 => b2 => Order.this.order(f(b1))(f(b2)))
}

object Order extends Orders

trait Orders {
  def order[A](f: A => A => Ordering): Order[A] = new Order[A] {
    val order = f
  }

  def orderC[A](f: (A, A) => Ordering): Order[A] =
    order(a1 => a2 => f(a1, a2))

  def orderI[A](f: (A, A) => Int): Order[A] =
    order(a1 => a2 => {
      val k = f(a1, a2)
      if (k < 0) LT
      else if (k > 0) GT
      else EQ
    })

  def orderBy[A, B: Order](f: A => B): Order[A] =
    implicitly[Order[B]] contramap f

  implicit def ScalaOrderingOrder[T: scala.Ordering]: Order[T] = orderC {
    (t1, t2) =>
      val k = implicitly[scala.Ordering[T]].compare(t1, t2)
      if (k < 0) LT
      else if (k > 0) GT
      else EQ
  }

  import Predef.{implicitly => i}

  implicit def StringOrder: Order[String] = ScalaOrderingOrder

  implicit def SymbolOrder: Order[Symbol] = orderBy(_.name)

  implicit def UnitOrder: Order[Unit] = ScalaOrderingOrder

  implicit def BooleanOrder: Order[Boolean] = ScalaOrderingOrder

  implicit def ByteOrder: Order[Byte] = ScalaOrderingOrder

  implicit def CharOrder: Order[Char] = ScalaOrderingOrder

  implicit def IntOrder: Order[Int] = ScalaOrderingOrder

  implicit def LongOrder: Order[Long] = ScalaOrderingOrder

  implicit def ShortOrder: Order[Short] = ScalaOrderingOrder

  implicit def FloatOrder: Order[Float] = ScalaOrderingOrder

  implicit def DoubleOrder: Order[Double] = ScalaOrderingOrder

  def UnpackOrder[T, R](implicit s: Order[R], u: ^*^[T, R]): Order[T] =
    implicitly[Order[R]] contramap (u.unpack(_))

  implicit def BigIntegerOrder: Order[java.math.BigInteger] = orderI(_ compareTo _)

  implicit def BigIntOrder: Order[BigInt] = ScalaOrderingOrder

  implicit def IterableOrder[CC[X] <: Iterable[X], A: Order]: Order[CC[A]] =
    order(a1 => a2 => {
      val i1 = a1.iterator
      val i2 = a2.iterator
      var b = true
      var r: Ordering = EQ

      while (i1.hasNext && i2.hasNext && b) {
        val a1 = i1.next
        val a2 = i2.next

        val o = implicitly[Order[A]].order(a1)(a2)
        if (o != EQ) {
          r = o
          b = false
        }
      }

      if (i1.hasNext)
        if (i2.hasNext)
          r
        else
          GT
      else
        LT
    })

  implicit def Function0Order[A: Order]: Order[Function0[A]] = orderBy(_.apply)

  implicit def Tuple1Order[A: Order]: Order[Tuple1[A]] = orderBy(_._1)

  implicit def Tuple2Order[A: Order, B: Order]: Order[(A, B)] =
    orderC {
      case ((a1, b1), (a2, b2)) =>
        implicitly[FoldMap[List]].foldU(List(
          implicitly[Order[A]].order(a1)(a2)
          , implicitly[Order[B]].order(b1)(b2)
        ))
    }

  implicit def Tuple3Order[A: Order, B: Order, C: Order]: Order[(A, B, C)] =
    orderC {
      case ((a1, b1, c1), (a2, b2, c2)) =>
        implicitly[FoldMap[List]].foldU(List(
          implicitly[Order[A]].order(a1)(a2)
          , implicitly[Order[B]].order(b1)(b2)
          , implicitly[Order[C]].order(c1)(c2)
        ))
    }

  implicit def Tuple4Order[A: Order, B: Order, C: Order, D: Order]: Order[(A, B, C, D)] =
    orderC {
      case ((a1, b1, c1, d1), (a2, b2, c2, d2)) =>
        implicitly[FoldMap[List]].foldU(List(
          implicitly[Order[A]].order(a1)(a2)
          , implicitly[Order[B]].order(b1)(b2)
          , implicitly[Order[C]].order(c1)(c2)
          , implicitly[Order[D]].order(d1)(d2)
        ))
    }

  implicit def Tuple5Order[A: Order, B: Order, C: Order, D: Order, E: Order]: Order[(A, B, C, D, E)] =
    orderC {
      case ((a1, b1, c1, d1, e1), (a2, b2, c2, d2, e2)) =>
        implicitly[FoldMap[List]].foldU(List(
          implicitly[Order[A]].order(a1)(a2)
          , implicitly[Order[B]].order(b1)(b2)
          , implicitly[Order[C]].order(c1)(c2)
          , implicitly[Order[D]].order(d1)(d2)
          , implicitly[Order[E]].order(e1)(e2)
        ))
    }

  implicit def Tuple6Order[A: Order, B: Order, C: Order, D: Order, E: Order, F: Order]: Order[(A, B, C, D, E, F)] =
    orderC {
      case ((a1, b1, c1, d1, e1, f1), (a2, b2, c2, d2, e2, f2)) =>
        implicitly[FoldMap[List]].foldU(List(
          implicitly[Order[A]].order(a1)(a2)
          , implicitly[Order[B]].order(b1)(b2)
          , implicitly[Order[C]].order(c1)(c2)
          , implicitly[Order[D]].order(d1)(d2)
          , implicitly[Order[E]].order(e1)(e2)
          , implicitly[Order[F]].order(f1)(f2)
        ))
    }

  implicit def Tuple7Order[A: Order, B: Order, C: Order, D: Order, E: Order, F: Order, G: Order]: Order[(A, B, C, D, E, F, G)] =
    orderC {
      case ((a1, b1, c1, d1, e1, f1, g1), (a2, b2, c2, d2, e2, f2, g2)) =>
        implicitly[FoldMap[List]].foldU(List(
          implicitly[Order[A]].order(a1)(a2)
          , implicitly[Order[B]].order(b1)(b2)
          , implicitly[Order[C]].order(c1)(c2)
          , implicitly[Order[D]].order(d1)(d2)
          , implicitly[Order[E]].order(e1)(e2)
          , implicitly[Order[F]].order(f1)(f2)
          , implicitly[Order[G]].order(g1)(g2)
        ))
    }

  implicit def OptionOrder[A: Order]: Order[Option[A]] =
    orderC {
      case (Some(x), Some(y)) => implicitly[Order[A]].order(x)(y)
      case (Some(_), None) => GT
      case (None, Some(_)) => LT
      case (None, None) => EQ
    }

  implicit def EitherOrder[A: Order, B: Order]: Order[Either[A, B]] =
    orderC {
      case (Left(x), Left(y)) => implicitly[Order[A]].order(x)(y)
      case (Right(x), Right(y)) => implicitly[Order[B]].order(x)(y)
      case (Left(_), Right(_)) => LT
      case (Right(_), Left(_)) => GT
    }

  implicit def SetOrder[CC[A] <: collection.Set[A], A: Order]: Order[CC[A]] =
    orderC((a1, a2) => {
      val i1 = a1.iterator
      val i2 = a2.iterator
      var b = true
      var r: Ordering = EQ

      while (i1.hasNext && i2.hasNext && b) {
        val a1 = i1.next
        val a2 = i2.next

        val o = implicitly[Order[A]].order(a1)(a2)
        if (o != EQ) {
          r = o
          b = false
        }
      }

      if (i1.hasNext)
        if (i2.hasNext)
          r
        else
          GT
      else
        LT
    })

  implicit def MapOrder[CC[K, V] <: collection.Map[K, V], A: Order, B: Order]: Order[CC[A, B]] =
    orderBy(_.toSet)

  import java.{lang => jl, util => ju}

  implicit def JavaIterableOrder[CC[X] <: jl.Iterable[X], A: Order]: Order[CC[A]] =
    orderC((a1, a2) => {
      val i1 = a1.iterator
      val i2 = a2.iterator
      var b = true
      var r: Ordering = EQ

      while (i1.hasNext && i2.hasNext && b) {
        val a1 = i1.next
        val a2 = i2.next

        val o = implicitly[Order[A]].order(a1)(a2)
        if (o != EQ) {
          r = o
          b = false
        }
      }

      if (i1.hasNext)
        if (i2.hasNext)
          r
        else
          GT
      else
        LT
    })

  implicit def JavaMapEntryOrder[K: Order, V: Order]: Order[java.util.Map.Entry[K, V]] =
    orderC {
      case (a1, a2) =>
        implicitly[FoldMap[List]].foldU(List(
          implicitly[Order[K]].order(a1.getKey)(a2.getKey)
          , implicitly[Order[V]].order(a1.getValue)(a2.getValue)
        ))
    }

  implicit def JavaMapOrder[K: Order, V: Order]: Order[ju.Map[K, V]] =
    orderBy(_.entrySet)

  implicit def CallableOrder[A: Order]: Order[java.util.concurrent.Callable[A]] =
    orderBy(_.call)
}