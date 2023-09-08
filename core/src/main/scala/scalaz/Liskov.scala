package scalaz

/**
 * Liskov substitutability: A better `<:<`
 *
 * `A <: B` holds whenever `A` could be used in any negative context that expects a `B`.
 * (e.g. if you could pass an `A` into any function that expects a `B`.)
 */
sealed abstract class Liskov[-A, +B] {
  def apply(a: A): B = Liskov.witness(this)(a)

  def substCo[F[+_]](p: F[A]): F[B]
  def substCt[F[-_]](p: F[B]): F[A]

  final def *[x[+_, +_], C, D](that: Liskov[C, D]): Liskov[A x C, B x D] = Liskov.lift2(this, that)

  final def andThen[C](that: Liskov[B, C]): Liskov[A, C] = Liskov.trans(that, this)

  final def compose[C](that: Liskov[C, A]): Liskov[C, B] = Liskov.trans(this, that)

  final def onF[X](fa: X => A): X => B = Liskov.co2_2[Function1, B, X, A](this)(fa)
}

sealed abstract class LiskovInstances {
  import Liskov._

  /** Subtyping forms a category */
  implicit val liskov: Category[<~<] = new Category[<~<] {
    def id[A]: (A <~< A) = refl[A]

    def compose[A, B, C](bc: B <~< C, ab: A <~< B): (A <~< C) = trans(bc, ab)
  }

  /** Lift Scala's subtyping relationship */
  implicit def isa[A, B >: A]: A <~< B = new (A <~< B) {
    def substCo[F[+ _]](p: F[A]): F[B] = p
    def substCt[F[- _]](p: F[B]): F[A] = p
  }
}

object Liskov extends LiskovInstances {

  /** A convenient type alias for Liskov */
  type <~<[-A, +B] = Liskov[A, B]

  /** A flipped alias, for those used to their arrows running left to right */
  type >~>[+B, -A] = Liskov[A, B]

  /** We can witness equality by using it to convert between types */
  implicit def witness[A, B](lt: A <~< B): A => B = {
    type f[-X] = X => B
    lt.substCt[f](identity)
  }

  /** Subtyping is reflexive */
  implicit def refl[A]: (A <~< A) = new (A <~< A) {
    def substCo[F[+ _]](p: F[A]) = p
    def substCt[F[- _]](p: F[A]) = p
  }

  private[scalaz] def fromLeibniz[A, B](ev: A === B): A <~< B =
    new (A <~< B) {
      def substCo[F[+ _]](p: F[A]) = ev.subst(p)
      def substCt[F[- _]](p: F[B]) = ev.flip.subst(p)
    }

  /** Subtyping is transitive */
  def trans[A, B, C](f: B <~< C, g: A <~< B): A <~< C =
    g.substCt[({type l[-α] = α <~< C})#l](f)

  /** We can lift subtyping into any covariant type constructor */
  def co[T[+_], A, A2](a: A <~< A2): (T[A] <~< T[A2]) =
    a.substCt[({type l[-α] = T[α] <~< T[A2]})#l](refl)

  def co2[T[+_, _], Z, A, B](a: A <~< Z): T[A, B] <~< T[Z, B] =
    a.substCt[({type l[-α] = T[α, B] <~< T[Z, B]})#l](refl)

  def co2_2[T[_, +_], Z, A, B](a: B <~< Z): T[A, B] <~< T[A, Z] =
    a.substCt[({type l[-α] = T[A, α] <~< T[A, Z]})#l](refl)

  def co3[T[+_, _, _], Z, A, B, C](a: A <~< Z): T[A, B, C] <~< T[Z, B, C] =
    a.substCt[({type l[-α] = T[α, B, C] <~< T[Z, B, C]})#l](refl)

  def co4[T[+_, _, _, _], Z, A, B, C, D](a: A <~< Z): T[A, B, C, D] <~< T[Z, B, C, D] =
    a.substCt[({type l[-α] = T[α, B, C, D] <~< T[Z, B, C, D]})#l](refl)

  /** lift2(a,b) = co1_2(a) compose co2_2(b) */
  def lift2[T[+_, +_], A, A2, B, B2](
    a: A <~< A2,
    b: B <~< B2
  ): (T[A, B] <~< T[A2, B2]) = {
    type a[-X] = T[X, B2] <~< T[A2, B2]
    type b[-X] = T[A, X] <~< T[A2, B2]
    b.substCt[b](a.substCt[a](refl))
  }

  /** lift3(a,b,c) = co1_3(a) compose co2_3(b) compose co3_3(c) */
  def lift3[T[+_, +_, +_], A, A2, B, B2, C, C2](
    a: A <~< A2,
    b: B <~< B2,
    c: C <~< C2
  ): (T[A, B, C] <~< T[A2, B2, C2]) = {
    type a[-X] = T[X, B2, C2] <~< T[A2, B2, C2]
    type b[-X] = T[A, X, C2] <~< T[A2, B2, C2]
    type c[-X] = T[A, B, X] <~< T[A2, B2, C2]
    c.substCt[c](b.substCt[b](a.substCt[a](refl)))
  }

  /** lift4(a,b,c,d) = co1_3(a) compose co2_3(b) compose co3_3(c) compose co4_4(d) */
  def lift4[T[+_, +_, +_, +_], A, A2, B, B2, C, C2, D, D2](
    a: A <~< A2,
    b: B <~< B2,
    c: C <~< C2,
    d: D <~< D2
  ): (T[A, B, C, D] <~< T[A2, B2, C2, D2]) = {
    type a[-X] = T[X, B2, C2, D2] <~< T[A2, B2, C2, D2]
    type b[-X] = T[A, X, C2, D2] <~< T[A2, B2, C2, D2]
    type c[-X] = T[A, B, X, D2] <~< T[A2, B2, C2, D2]
    type d[-X] = T[A, B, C, X] <~< T[A2, B2, C2, D2]
    d.substCt[d](c.substCt[c](b.substCt[b](a.substCt[a](refl))))
  }

  /** We can lift subtyping into any contravariant type constructor */
  def contra[T[-_], A, A2](a: A <~< A2): (T[A2] <~< T[A]) =
    a.substCt[({type l[-α] = T[A2] <~< T[α]})#l](refl)

  // binary
  def contra1_2[T[-_, _], Z, A, B](a: A <~< Z): (T[Z, B] <~< T[A, B]) =
    a.substCt[({type l[-α] = T[Z, B] <~< T[α, B]})#l](refl)

  def contra2_2[T[_, -_], Z, A, B](a: B <~< Z): (T[A, Z] <~< T[A, B]) =
    a.substCt[({type l[-α] = T[A, Z] <~< T[A, α]})#l](refl)

  // ternary
  def contra1_3[T[-_, _, _], Z, A, B, C](a: A <~< Z): (T[Z, B, C] <~< T[A, B, C]) =
    a.substCt[({type l[-α] = T[Z, B, C] <~< T[α, B, C]})#l](refl)

  def contra2_3[T[_, -_, _], Z, A, B, C](a: B <~< Z): (T[A, Z, C] <~< T[A, B, C]) =
    a.substCt[({type l[-α] = T[A, Z, C] <~< T[A, α, C]})#l](refl)

  def contra3_3[T[_, _, -_], Z, A, B, C](a: C <~< Z): (T[A, B, Z] <~< T[A, B, C]) =
    a.substCt[({type l[-α] = T[A, B, Z] <~< T[A, B, α]})#l](refl)

  def contra1_4[T[-_, _, _, _], Z, A, B, C, D](a: A <~< Z): (T[Z, B, C, D] <~< T[A, B, C, D]) =
    a.substCt[({type l[-α] = T[Z, B, C, D] <~< T[α, B, C, D]})#l](refl)

  def contra2_4[T[_, -_, _, _], Z, A, B, C, D](a: B <~< Z): (T[A, Z, C, D] <~< T[A, B, C, D]) =
    a.substCt[({type l[-α] = T[A, Z, C, D] <~< T[A, α, C, D]})#l](refl)

  def contra3_4[T[_, _, -_, _], Z, A, B, C, D](a: C <~< Z): (T[A, B, Z, D] <~< T[A, B, C, D]) =
    a.substCt[({type l[-α] = T[A, B, Z, D] <~< T[A, B, α, D]})#l](refl)

  def contra4_4[T[_, _, _, -_], Z, A, B, C, D](a: D <~< Z): (T[A, B, C, Z] <~< T[A, B, C, D]) =
    a.substCt[({type l[-α] = T[A, B, C, Z] <~< T[A, B, C, α]})#l](refl)

  /** Lift subtyping into a unary function-like type
   *  {{{
   *      liftF1(a,r) = contra1_2(a) compose co2_2(b)
   *  }}}
   */
  def liftF1[F[-_, +_], A, A2, R, R2](
    a: A <~< A2,
    r: R <~< R2
  ): (F[A2, R] <~< F[A, R2]) = {
    type a[-X] = F[A2, R2] <~< F[X, R2]
    type r[-X] = F[A2, X] <~< F[A, R2]
    r.substCt[r](a.substCt[a](refl))
  }

  /** Lift subtyping into a binary function-like type
   *  {{{
   *      liftF2(a,b,r) = contra1_3(a) compose contra2_3(b) compose co3_3(c)
   *  }}}
   */
  def liftF2[F[-_, -_, +_], A, A2, B, B2, R, R2](
    a: A <~< A2,
    b: B <~< B2,
    r: R <~< R2
  ): (F[A2, B2, R] <~< F[A, B, R2]) = {
    type a[-X] = F[A2, B2, R2] <~< F[X, B2, R2]
    type b[-X] = F[A2, B2, R2] <~< F[A, X, R2]
    type r[-X] = F[A2, B2, X] <~< F[A, B, R2]
    r.substCt[r](b.substCt[b](a.substCt[a](refl)))
  }

  /** Lift subtyping into a ternary function-like type
   *  {{{
   *      liftF3(a,b,c,r) = contra1_4(a) compose contra2_4(b) compose contra3_4(c) compose co3_4(d)
   *  }}}
   */
  def liftF3[F[-_, -_, -_, +_], A, A2, B, B2, C, C2, R, R2](
     a: A <~< A2,
     b: B <~< B2,
     c: C <~< C2,
     r: R <~< R2
  ): (F[A2, B2, C2, R] <~< F[A, B, C, R2]) = {
    type a[-X] = F[A2, B2, C2, R2] <~< F[X, B2, C2, R2]
    type b[-X] = F[A2, B2, C2, R2] <~< F[A, X, C2, R2]
    type c[-X] = F[A2, B2, C2, R2] <~< F[A, B, X, R2]
    type r[-X] = F[A2, B2, C2, X] <~< F[A, B, C, R2]
    r.substCt[r](c.substCt[c](b.substCt[b](a.substCt[a](refl))))
  }

  /** Lift subtyping into a 4-ary function-like type */
  def liftF4[F[-_, -_, -_, -_, +_], A, A2, B, B2, C, C2, D, D2, R, R2](
     a: A <~< A2,
     b: B <~< B2,
     c: C <~< C2,
     d: D <~< D2,
     r: R <~< R2
   ): (F[A2, B2, C2, D2, R] <~< F[A, B, C, D, R2]) = {
    type a[-X] = F[A2, B2, C2, D2, R2] <~< F[X, B2, C2, D2, R2]
    type b[-X] = F[A2, B2, C2, D2, R2] <~< F[A, X, C2, D2, R2]
    type c[-X] = F[A2, B2, C2, D2, R2] <~< F[A, B, X, D2, R2]
    type d[-X] = F[A2, B2, C2, D2, R2] <~< F[A, B, C, X, R2]
    type r[-X] = F[A2, B2, C2, D2, X] <~< F[A, B, C, D, R2]
    r.substCt[r](d.substCt[d](c.substCt[c](b.substCt[b](a.substCt[a](refl)))))
  }

  /** Unsafely force a claim that A is a subtype of B. */
  def force[A, B]: A <~< B =
    new (A <~< B) {
      def substCo[F[+ _]](p: F[A]) = p.asInstanceOf[F[B]]
      def substCt[F[- _]](p: F[B]) = p.asInstanceOf[F[A]]
    }

  def unco[F[_] : Injective, Z, A](
    a: F[A] <~< F[Z]
  ): (A <~< Z) = force[A, Z]

  def unco2_1[F[+_, _] : Injective2, Z, A, B](
    a: F[A, B] <~< F[Z, B]
  ): (A <~< Z) = force[A, Z]

  def unco2_2[F[_, +_] : Injective2, Z, A, B](
    a: F[A, B] <~< F[A, Z]
  ): (B <~< Z) = force[B, Z]

  def uncontra[F[-_] : Injective, Z, A](
    a: F[A] <~< F[Z]
  ): (Z <~< A) = force[Z, A]

  def uncontra2_1[F[-_, _] : Injective2, Z, A, B](
    a: F[A, B] <~< F[Z, B]
  ): (Z <~< A) = force[Z, A]

  def uncontra2_2[F[_, -_] : Injective2, Z, A, B](
    a: F[A, B] <~< F[A, Z]
  ): (Z <~< B) = force[Z, B]
}
