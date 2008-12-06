package fjs.test

import fj.test.Rand

sealed abstract class Property {
  val p: fj.test.Property

  def &&&(q: Property): Property = p and q

  def |||(q: Property): Property = p or q

  def >>(q: Property): Property = p sequence q

  def unary_+ = p check

  def unary_+(minSize: Int, maxSize: Int) = p check (minSize, maxSize)
  def apply(minSuccessful: Int, maxDiscarded: Int, minSize: Int, maxSize: Int) = p check (minSuccessful, maxDiscarded, minSize, maxSize)
  def apply(r: Rand) = p check r
  def apply(r: Rand, minSize: Int, maxSize: Int) = p check (r, minSize, maxSize)
  def apply(r: Rand, minSuccessful: Int, maxDiscarded: Int, minSize: Int, maxSize: Int) = p check (r, minSuccessful, maxDiscarded, minSize, maxSize)
}

import fjs.F._
import fjs.F2._
import fjs.F3._
import fjs.F4._
import fjs.F5._
import fjs.F6._
import fjs.F7._
import fjs.F8._
import Arbitrary._

object Property {
  type Prop = fj.test.Property
  type Shr[A] = fj.test.Shrink[A]

  implicit def prop[A](f: A => Prop)(implicit aa: Arbitrary[A]): Prop =
    fj.test.Property.property(aa, f)

  def property[A](f: A => Prop)(implicit aa: Arbitrary[A], sa: Shr[A]): Prop =
    fj.test.Property.property(aa, sa, f)

  def prop[A, B](f: (A, B) => Prop)(implicit aa: Arbitrary[A], ab: Arbitrary[B]): Prop =
    fj.test.Property.property(aa, ab, f)

  def property[A, B](f: (A, B) => Prop)(implicit aa: Arbitrary[A], ab: Arbitrary[B], sa: Shr[A], sb: Shr[B]): Prop =
    fj.test.Property.property(aa, ab, sa, sb, f)

  def prop[A, B, C](f: (A, B, C) => Prop)(implicit aa: Arbitrary[A], ab: Arbitrary[B], ac: Arbitrary[C]): Prop =
    fj.test.Property.property(aa, ab, ac, f)

  def property[A, B, C](f: (A, B, C) => Prop)(implicit aa: Arbitrary[A], ab: Arbitrary[B], ac: Arbitrary[C], sa: Shr[A], sb: Shr[B], sc: Shr[C]): Prop =
    fj.test.Property.property(aa, ab, ac, sa, sb, sc, f)

  def prop[A, B, C, D](f: (A, B, C, D) => Prop)(implicit aa: Arbitrary[A], ab: Arbitrary[B], ac: Arbitrary[C], ad: Arbitrary[D]): Prop =
    fj.test.Property.property(aa, ab, ac, ad, f)

  def property[A, B, C, D](f: (A, B, C, D) => Prop)(implicit aa: Arbitrary[A], ab: Arbitrary[B], ac: Arbitrary[C], ad: Arbitrary[D], sa: Shr[A], sb: Shr[B], sc: Shr[C], sd: Shr[D]): Prop =
    fj.test.Property.property(aa, ab, ac, ad, sa, sb, sc, sd, f)

  def prop[A, B, C, D, E](f: (A, B, C, D, E) => Prop)(implicit aa: Arbitrary[A], ab: Arbitrary[B], ac: Arbitrary[C], ad: Arbitrary[D], ae: Arbitrary[E]): Prop =
    fj.test.Property.property(aa, ab, ac, ad, ae, f)

  def property[A, B, C, D, E](f: (A, B, C, D, E) => Prop)(implicit aa: Arbitrary[A], ab: Arbitrary[B], ac: Arbitrary[C], ad: Arbitrary[D], ae: Arbitrary[E], sa: Shr[A], sb: Shr[B], sc: Shr[C], sd: Shr[D], se: Shr[E]): Prop =
    fj.test.Property.property(aa, ab, ac, ad, ae, sa, sb, sc, sd, se, f)

  def prop[A, B, C, D, E, F$](f: (A, B, C, D, E, F$) => Prop)(implicit aa: Arbitrary[A], ab: Arbitrary[B], ac: Arbitrary[C], ad: Arbitrary[D], ae: Arbitrary[E], af: Arbitrary[F$]): Prop =
    fj.test.Property.property(aa, ab, ac, ad, ae, af, f)

  def property[A, B, C, D, E, F$](f: (A, B, C, D, E, F$) => Prop)(implicit aa: Arbitrary[A], ab: Arbitrary[B], ac: Arbitrary[C], ad: Arbitrary[D], ae: Arbitrary[E], af: Arbitrary[F$], sa: Shr[A], sb: Shr[B], sc: Shr[C], sd: Shr[D], se: Shr[E], sf: Shr[F$]): Prop =
    fj.test.Property.property(aa, ab, ac, ad, ae, af, sa, sb, sc, sd, se, sf, f)

  def prop[A, B, C, D, E, F$, G](f: (A, B, C, D, E, F$, G) => Prop)(implicit aa: Arbitrary[A], ab: Arbitrary[B], ac: Arbitrary[C], ad: Arbitrary[D], ae: Arbitrary[E], af: Arbitrary[F$], ag: Arbitrary[G]): Prop =
    fj.test.Property.property(aa, ab, ac, ad, ae, af, ag, f)

  def property[A, B, C, D, E, F$, G](f: (A, B, C, D, E, F$, G) => Prop)(implicit aa: Arbitrary[A], ab: Arbitrary[B], ac: Arbitrary[C], ad: Arbitrary[D], ae: Arbitrary[E], af: Arbitrary[F$], ag: Arbitrary[G], sa: Shr[A], sb: Shr[B], sc: Shr[C], sd: Shr[D], se: Shr[E], sf: Shr[F$], sg: Shr[G]): Prop =
    fj.test.Property.property(aa, ab, ac, ad, ae, af, ag, sa, sb, sc, sd, se, sf, sg, f)

  def prop[A, B, C, D, E, F$, G, H](f: (A, B, C, D, E, F$, G, H) => Prop)(implicit aa: Arbitrary[A], ab: Arbitrary[B], ac: Arbitrary[C], ad: Arbitrary[D], ae: Arbitrary[E], af: Arbitrary[F$], ag: Arbitrary[G], ah: Arbitrary[H]): Prop =
    fj.test.Property.property(aa, ab, ac, ad, ae, af, ag, ah, f)

  def property[A, B, C, D, E, F$, G, H](f: (A, B, C, D, E, F$, G, H) => Prop)(implicit aa: Arbitrary[A], ab: Arbitrary[B], ac: Arbitrary[C], ad: Arbitrary[D], ae: Arbitrary[E], af: Arbitrary[F$], ag: Arbitrary[G], ah: Arbitrary[H], sa: Shr[A], sb: Shr[B], sc: Shr[C], sd: Shr[D], se: Shr[E], sf: Shr[F$], sg: Shr[G], sh: Shr[H]): Prop =
    fj.test.Property.property(aa, ab, ac, ad, ae, af, ag, ah, sa, sb, sc, sd, se, sf, sg, sh, f)

  implicit def Boolean_Property(b: Boolean) = fj.test.Property.prop(b)
  
  implicit def Property_SProperty(pp: fj.test.Property): Property = new Property {
    val p = pp
  }

  implicit def SProperty_Property(p: Property): fj.test.Property = p.p
}
