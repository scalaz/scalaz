package scalaz

trait Bind[F[_]] {
  def bind[A, B](f: A => F[B]): F[A] => F[B]

  def deriving[G[_]](implicit n: ^**^[G, F]): Bind[G] =
    new Bind[G] {
      def bind[A, B](f: A => G[B]) =
        k => n.pack(Bind.this.bind((a: A) => n.unpack(f(a)))(n unpack k))
    }
}

object Bind extends Binds

trait Binds {
  implicit val OptionBind: Bind[Option] = new Bind[Option] {
    def bind[A, B](f: A => Option[B]) =
      _ flatMap f
  }

  implicit val ListBind: Bind[List] = new Bind[List] {
    def bind[A, B](f: A => List[B]) =
      _ flatMap f
  }

  implicit val StreamBind: Bind[Stream] = new Bind[Stream] {
    def bind[A, B](f: A => Stream[B]) =
      _ flatMap f
  }

  implicit def Tuple1Bind: Bind[Tuple1] = new Bind[Tuple1] {
    def bind[A, B](f: A => Tuple1[B]) =
      r => f(r._1)
  }

  implicit def Tuple2Bind[R: Semigroup]: Bind[({type λ[α]=(R, α)})#λ] = new Bind[({type λ[α]=(R, α)})#λ] {
    def bind[A, B](f: A => (R, B)) = r => {
      val (rr, b) = f(r._2)
      (implicitly[Semigroup[R]].append(r._1, rr), b)

    }
  }

  implicit def Tuple3Bind[R: Semigroup, S: Semigroup]: Bind[({type λ[α]=(R, S, α)})#λ] = new Bind[({type λ[α]=(R, S, α)})#λ] {
    def bind[A, B](f: A => (R, S, B)) = r => {
      val (rr, s, b) = f(r._3)
      (implicitly[Semigroup[R]].append(r._1, rr), implicitly[Semigroup[S]].append(r._2, s), b)
    }
  }

  implicit def Tuple4Bind[R: Semigroup, S: Semigroup, T: Semigroup]: Bind[({type λ[α]=(R, S, T, α)})#λ] = new Bind[({type λ[α]=(R, S, T, α)})#λ] {
    def bind[A, B](f: A => (R, S, T, B)) = r => {
      val (rr, s, t, b) = f(r._4)
      (implicitly[Semigroup[R]].append(r._1, rr), implicitly[Semigroup[S]].append(r._2, s), implicitly[Semigroup[T]].append(r._3, t), b)
    }
  }

  implicit def Tuple5Bind[R: Semigroup, S: Semigroup, T: Semigroup, U: Semigroup]: Bind[({type λ[α]=(R, S, T, U, α)})#λ] = new Bind[({type λ[α]=(R, S, T, U, α)})#λ] {
    def bind[A, B](f: A => (R, S, T, U, B)) = r => {
      val (rr, s, t, u, b) = f(r._5)
      (implicitly[Semigroup[R]].append(r._1, rr), implicitly[Semigroup[S]].append(r._2, s), implicitly[Semigroup[T]].append(r._3, t), implicitly[Semigroup[U]].append(r._4, u), b)
    }
  }

  implicit def Tuple6Bind[R: Semigroup, S: Semigroup, T: Semigroup, U: Semigroup, V: Semigroup]: Bind[({type λ[α]=(R, S, T, U, V, α)})#λ] = new Bind[({type λ[α]=(R, S, T, U, V, α)})#λ] {
    def bind[A, B](f: A => (R, S, T, U, V, B)) = r => {
      val (rr, s, t, u, v, b) = f(r._6)
      (implicitly[Semigroup[R]].append(r._1, rr), implicitly[Semigroup[S]].append(r._2, s), implicitly[Semigroup[T]].append(r._3, t), implicitly[Semigroup[U]].append(r._4, u), implicitly[Semigroup[V]].append(r._5, v), b)
    }
  }

  implicit def Tuple7Bind[R: Semigroup, S: Semigroup, T: Semigroup, U: Semigroup, V: Semigroup, W: Semigroup]: Bind[({type λ[α]=(R, S, T, U, V, W, α)})#λ] = new Bind[({type λ[α]=(R, S, T, U, V, W, α)})#λ] {
    def bind[A, B](f: A => (R, S, T, U, V, W, B)) = r => {
      val (rr, s, t, u, v, w, b) = f(r._7)
      (implicitly[Semigroup[R]].append(r._1, rr), implicitly[Semigroup[S]].append(r._2, s), implicitly[Semigroup[T]].append(r._3, t), implicitly[Semigroup[U]].append(r._4, u), implicitly[Semigroup[V]].append(r._5, v), implicitly[Semigroup[W]].append(r._6, w), b)
    }
  }

}
