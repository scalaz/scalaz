package scalaz

import scala.annotation.tailrec

/** Mixed into object `Id` in the package object [[scalaz]]. */
trait IdInstances {

  // Declaring here instead of the scalaz package object in order to avoid compiler crash in 2.9.2.

  /** The strict identity type constructor. Can be thought of as `Tuple1`, but with no
   *  runtime representation.
   */
  type Id[X] = X

  // TODO Review!
  type Identity[+X] = Need[X]

  val id: Traverse1[Id] with Monad[Id] with BindRec[Id] with Comonad[Id] with Distributive[Id] with Zip[Id] with Unzip[Id] with Align[Id] with Cozip[Id] with Optional[Id] =
    new Traverse1[Id] with Monad[Id] with BindRec[Id] with Comonad[Id] with Distributive[Id] with Zip[Id] with Unzip[Id] with Align[Id] with Cozip[Id] with Optional[Id] {
      def point[A](a: => A): A = a

      def bind[A, B](a: A)(f: A => B): B = f(a)

      def cobind[A, B](a: A)(f: A => B): B = f(a)

      override def cojoin[A](a: Id[A]): A = a

      def copoint[A](p: Id[A]): A = p

      def zip[A, B](a: => Id[A], b: => Id[B]): (A, B) = (a, b)

      def unzip[A, B](a: Id[(A, B)]): (A, B) = (a._1, a._2)

      def alignWith[A, B, C](f: A \&/ B => C) = (a, b) => f(\&/.Both(a, b))

      def cozip[A, B](a: Id[A \/ B]): (A \/ B) = a

      def traverse1Impl[G[_] : Apply, A, B](fa: Id[A])(f: A => G[B]): G[Id[B]] = f(fa)

      def distributeImpl[G[_] : Functor, A, B](fa: G[A])(f: A => Id[B]): Id[G[B]] = Functor[G].map(fa)(f)

      override def foldRight[A, B](fa: Id[A], z: => B)(f: (A, => B) => B): B = f(fa, z)

      override def foldMapRight1[A, B](fa: Id[A])(z: A => B)(f: (A, => B) => B): B = z(fa)

      // Overrides for efficiency.

      override def lift[A, B](f: A => B): Id[A] => Id[B] = f

      // `ffa: Id[Id[A]]`, gives, "cyclic aliasing or subtyping involving type Id", but `ffa: A` is identical.
      override def join[A](ffa: A) = ffa

      override def traverse[A, G[_], B](value: G[A])(f: A => Id[B])(implicit G: Traverse[G]): Id[G[B]] = G.map(value)(f)

      override def sequence[A, G[_] : Traverse](as: G[Id[A]]): Id[G[A]] = as

      override def ap[A, B](fa: => Id[A])(f: => Id[A => B]): Id[B] = f(fa)

      override def compose[G[_]](implicit G0: Applicative[G]): Applicative[G] = G0

      // TODO Fun compiler bug? "can't existentially abstract over parameterized type G"
      // override def product1[G[_]](implicit G0: Applicative[G]): Applicative[G] = G0

      override def pextract[B, A](a: Id[A])       : Id[B] \/ A = \/-(a)
      override def getOrElse[A](a: Id[A])(d: => A): A          = a
      override def isDefined[A](a: Id[A])         : Boolean    = true
      override def toOption[A](a: Id[A])          : Option[A]  = Some(a)
      override def toMaybe[A](a: Id[A])           : Maybe[A]   = Maybe.Just(a)

      @tailrec
      def tailrecM[A, B](f: A => A \/ B)(a: A): B =
        f(a) match {
          case -\/(a0) => tailrecM(f)(a0)
          case \/-(b) => b
        }
    }
}

object Id extends IdInstances
