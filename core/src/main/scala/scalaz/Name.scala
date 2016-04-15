package scalaz

import scala.annotation.tailrec

/** Call by name */
sealed abstract class Name[+A] {
  def value: A
}

/** Call by need */
sealed abstract class Need[+A] extends Name[A]

/** Call by value */
final case class Value[+A](value: A) extends Need[A]

object Name {
  def apply[A](a: => A) = new Name[A] {
    def value = a
  }
  def unapply[A](v: Name[A]): Option[A] = Some(v.value)

  implicit val name: Monad[Name] with BindRec[Name] with Comonad[Name] with Distributive[Name] with Traverse1[Name] with Zip[Name] with Unzip[Name] with Align[Name] with Cozip[Name] =
    new Monad[Name] with BindRec[Name] with Comonad[Name] with Distributive[Name] with Traverse1[Name] with Zip[Name] with Unzip[Name] with Align[Name] with Cozip[Name] {
      override def foldMap1[A, B: Semigroup](fa: Name[A])(f: A => B) = f(fa.value)
      override def foldLeft[A, B](fa: Name[A], z: B)(f: (B, A) => B) = f(z, fa.value)
      override def foldRight[A, B](fa: Name[A], z: => B)(f: (A, => B) => B) = f(fa.value, z)
      def alignWith[A, B, C](f: A \&/ B => C) = (a, b) => Name(f(\&/.Both(a.value, b.value)))
      def cozip[A, B](x: Name[A \/ B]) = x.value.bimap(Name(_), Name(_))
      def foldMapRight1[A, B](fa: Name[A])(z: A => B)(f: (A, => B) => B) = z(fa.value)
      def traverse1Impl[G[_], A, B](fa: Name[A])(f: A => G[B])(implicit G: Apply[G]) = G.map(f(fa.value))(Name(_))
      def unzip[A, B](a: Name[(A, B)]) = (Name(a.value._1), Name(a.value._2))
      def zip[A, B](a: => Name[A], b: => Name[B]) = Name((a.value, b.value))
      def point[A](a: => A) = Name(a)
      override def map[A, B](fa: Name[A])(f: A => B) = Name(f(fa.value))
      override def ap[A, B](fa: => Name[A])(f: => Name[A => B]) =
        Name(f.value apply fa.value)
      def bind[A,B](v: Name[A])(f: A => Name[B]): Name[B] = Name(f(v.value).value)
      def cobind[A, B](fa: Name[A])(f: Name[A] => B): Name[B] = Name(f(fa))
      override def cojoin[A](a: Name[A]): Name[Name[A]] = Name(a)
      def copoint[A](p: Name[A]): A = p.value
      def distributeImpl[G[_], A, B](fa: G[A])(f: A => Name[B])(implicit G: Functor[G]) =
        Name(G.map(fa)(a => f(a).value))
      @tailrec
      def tailrecM[A, B](f: A => Name[A \/ B])(a: A): Name[B] =
        f(a).value match {
          case -\/(a0) => tailrecM(f)(a0)
          case \/-(b) => Name(b)
        }
    }
  implicit def nameEqual[A: Equal]: Equal[Name[A]] = new Equal[Name[A]] {
    def equal(a1: Name[A], a2: Name[A]): Boolean = Equal[A].equal(a1.value, a2.value)
  }
}

/** Synchronized, thread-safe version of `Need` */
object ThreadSafeNeed {
  def apply[A](a: => A): Need[A] = {
    new Need[A] {
      lazy val value: A = a
    }
  }
}

object Need {
  def apply[A](can: => A): Need[A] = {
    new Need[A] {
      private[this] var a: A = _
      private[this] var haz: Boolean = _
      def value = {
        if (!haz) {
          a = can
          haz = true
        }

        a
      }
    }
  }

  def unapply[A](x: Need[A]): Option[A] = Some(x.value)

  implicit val need: Monad[Need] with BindRec[Need] with Comonad[Need] with Distributive[Need] with Traverse1[Need] with Zip[Need] with Unzip[Need] with Align[Need] with Cozip[Need] =
    new Monad[Need] with BindRec[Need] with Comonad[Need] with Distributive[Need] with Traverse1[Need] with Zip[Need] with Unzip[Need] with Align[Need] with Cozip[Need] {
      override def foldMap1[A, B: Semigroup](fa: Need[A])(f: A => B) = f(fa.value)
      override def foldLeft[A, B](fa: Need[A], z: B)(f: (B, A) => B) = f(z, fa.value)
      override def foldRight[A, B](fa: Need[A], z: => B)(f: (A, => B) => B) = f(fa.value, z)
      def alignWith[A, B, C](f: A \&/ B => C) = (a, b) => Need(f(\&/.Both(a.value, b.value)))
      def cozip[A, B](x: Need[A \/ B]) = x.value.bimap(Need(_), Need(_))
      def foldMapRight1[A, B](fa: Need[A])(z: A => B)(f: (A, => B) => B) = z(fa.value)
      def traverse1Impl[G[_], A, B](fa: Need[A])(f: A => G[B])(implicit G: Apply[G]) = G.map(f(fa.value))(Need(_))
      def unzip[A, B](a: Need[(A, B)]) = (Need(a.value._1), Need(a.value._2))
      def zip[A, B](a: => Need[A], b: => Need[B]) = Need((a.value, b.value))
      def point[A](a: => A) = Need(a)
      override def map[A, B](fa: Need[A])(f: A => B) = Need(f(fa.value))
      override def ap[A, B](fa: => Need[A])(f: => Need[A => B]) =
        Need(f.value apply fa.value)
      def bind[A, B](v: Need[A])(f: A => Need[B]): Need[B] = Need(f(v.value).value)
      def cobind[A, B](fa: Need[A])(f: Need[A] => B): Need[B] = Need(f(fa))
      override def cojoin[A](a: Need[A]): Need[Need[A]] = Need(a)
      def copoint[A](p: Need[A]): A = p.value
      def distributeImpl[G[_], A, B](fa: G[A])(f: A => Need[B])(implicit G: Functor[G]) =
        Need(G.map(fa)(a => f(a).value))
      @tailrec
      def tailrecM[A, B](f: A => Need[A \/ B])(a: A): Need[B] =
        f(a).value match {
          case -\/(a0) => tailrecM(f)(a0)
          case \/-(b) => Need(b)
        }
    }
  implicit def needEqual[A: Equal]: Equal[Need[A]] = new Equal[Need[A]] {
    def equal(a1: Need[A], a2: Need[A]): Boolean = Equal[A].equal(a1.value, a2.value)
  }
}

object Value {
  implicit val value: Monad[Value] with BindRec[Value] with Comonad[Value] with Distributive[Value] with Traverse1[Value] with Zip[Value] with Unzip[Value] with Align[Value] with Cozip[Value] =
    new Monad[Value] with BindRec[Value] with Comonad[Value] with Distributive[Value] with Traverse1[Value] with Zip[Value] with Unzip[Value] with Align[Value] with Cozip[Value] {
      override def foldMap1[A, B: Semigroup](fa: Value[A])(f: A => B) = f(fa.value)
      override def foldLeft[A, B](fa: Value[A], z: B)(f: (B, A) => B) = f(z, fa.value)
      override def foldRight[A, B](fa: Value[A], z: => B)(f: (A, => B) => B) = f(fa.value, z)
      def alignWith[A, B, C](f: A \&/ B => C) = (a, b) => Value(f(\&/.Both(a.value, b.value)))
      def cozip[A, B](x: Value[A \/ B]) = x.value.bimap(Value(_), Value(_))
      def foldMapRight1[A, B](fa: Value[A])(z: A => B)(f: (A, => B) => B) = z(fa.value)
      def traverse1Impl[G[_], A, B](fa: Value[A])(f: A => G[B])(implicit G: Apply[G]) = G.map(f(fa.value))(Value(_))
      def unzip[A, B](a: Value[(A, B)]) = (Value(a.value._1), Value(a.value._2))
      def zip[A, B](a: => Value[A], b: => Value[B]) = Value((a.value, b.value))
      def point[A](a: => A) = Value(a)
      def bind[A, B](v: Value[A])(f: A => Value[B]): Value[B] = f(v.value)
      def cobind[A, B](fa: Value[A])(f: Value[A] => B): Value[B] = Value(f(fa))
      override def cojoin[A](a: Value[A]): Value[Value[A]] = Value(a)
      def copoint[A](p: Value[A]): A = p.value
      def distributeImpl[G[_], A, B](fa: G[A])(f: A => Value[B])(implicit G: Functor[G]) =
        Value(G.map(fa)(a => f(a).value))
      @tailrec
      def tailrecM[A, B](f: A => Value[A \/ B])(a: A): Value[B] =
        f(a).value match {
          case -\/(a0) => tailrecM(f)(a0)
          case \/-(b) => Value(b)
        }
    }
  implicit def valueEqual[A: Equal]: Equal[Value[A]] = new Equal[Value[A]] {
    def equal(a1: Value[A], a2: Value[A]): Boolean = Equal[A].equal(a1.value, a2.value)
  }
}
