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

  implicit def EitherLeftBind[X]: Bind[({type λ[α]=Either.LeftProjection[α, X]})#λ] = new Bind[({type λ[α]=Either.LeftProjection[α, X]})#λ] {
    def bind[A, B](f: A => Either.LeftProjection[B, X]) =
      _.flatMap(f(_).e).left
  }

  implicit def EitherRightBind[X]: Bind[({type λ[α]=Either.RightProjection[X, α]})#λ] = new Bind[({type λ[α]=Either.RightProjection[X, α]})#λ] {
    def bind[A, B](f: A => Either.RightProjection[X, B]) =
      _.flatMap(f(_).e).right
  }

  implicit def EitherBind[X]: Bind[({type λ[α]=Either[X, α]})#λ] = new Bind[({type λ[α]=Either[X, α]})#λ] {
    def bind[A, B](f: A => Either[X, B]) =
      _.fold(Left(_), f)
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

  implicit def Function0Bind: Bind[Function0] = new Bind[Function0] {
    def bind[A, B](f: A => (() => B)) = r => f(r.apply)
  }

  implicit def Function1Bind[R]: Bind[({type λ[α]=(R) => α})#λ] = new Bind[({type λ[α]=(R) => α})#λ] {
    def bind[A, B](f: A => R => B) = r => (t: R) => f(r(t))(t)
  }

  implicit def Function2Bind[R, S]: Bind[({type λ[α]=(R, S) => α})#λ] = new Bind[({type λ[α]=(R, S) => α})#λ] {
    def bind[A, B](f: A => (R, S) => B) = r => (t1: R, t2: S) => f(r(t1, t2))(t1, t2)
  }

  implicit def Function3Bind[R, S, T]: Bind[({type λ[α]=(R, S, T) => α})#λ] = new Bind[({type λ[α]=(R, S, T) => α})#λ] {
    def bind[A, B](f: A => (R, S, T) => B) = r => (t1: R, t2: S, t3: T) => f(r(t1, t2, t3))(t1, t2, t3)
  }

  implicit def Function4Bind[R, S, T, U]: Bind[({type λ[α]=(R, S, T, U) => α})#λ] = new Bind[({type λ[α]=(R, S, T, U) => α})#λ] {
    def bind[A, B](f: A => (R, S, T, U) => B) = r => (t1: R, t2: S, t3: T, t4: U) => f(r(t1, t2, t3, t4))(t1, t2, t3, t4)
  }

  implicit def Function5Bind[R, S, T, U, V]: Bind[({type λ[α]=(R, S, T, U, V) => α})#λ] = new Bind[({type λ[α]=(R, S, T, U, V) => α})#λ] {
    def bind[A, B](f: A => (R, S, T, U, V) => B) = r => (t1: R, t2: S, t3: T, t4: U, t5: V) => f(r(t1, t2, t3, t4, t5))(t1, t2, t3, t4, t5)
  }

  implicit def Function6Bind[R, S, T, U, V, W]: Bind[({type λ[α]=(R, S, T, U, V, W) => α})#λ] = new Bind[({type λ[α]=(R, S, T, U, V, W) => α})#λ] {
    def bind[A, B](f: A => (R, S, T, U, V, W) => B) = r => (t1: R, t2: S, t3: T, t4: U, t5: V, t6: W) => f(r(t1, t2, t3, t4, t5, t6))(t1, t2, t3, t4, t5, t6)
  }

}
