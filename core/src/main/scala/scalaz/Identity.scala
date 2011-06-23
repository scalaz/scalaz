package scalaz

sealed trait Identity[A] {
  def value: A

  def map[B](f: A => B): Identity[B] = new Identity[B] {
    def value = f(Identity.this.value)
  }
}

object Identity extends Identitys {
  def apply[A]: (=> A) => Identity[A] =
    id[A]
}

trait Identitys {
  def id[A]: (=> A) => Identity[A] = v => new Identity[A] {
    def value = v
  }

  implicit def IdentityEqual[A: Equal]: Equal[Identity[A]] =
    Equal.equalBy(_.value)

  implicit def IdentityOrder[A: Order]: Order[Identity[A]] =
    Order.orderBy(_.value)

  implicit def IdentityShow[A: Show]: Show[Identity[A]] =
    Show.showBy(_.value)

  implicit val IdentityFunctor: Functor[Identity] = new Functor[Identity] {
    def fmap[A, B](f: A => B) = a => id(f(a.value))
  }

  implicit val IdentityPointed: Pointed[Identity] = new Pointed[Identity] {
    def point[A](a: => A) = id(a)
  }

  implicit val IdentityPointedFunctor: PointedFunctor[Identity] =
    PointedFunctor.pointedFunctor[Identity]

  implicit val IdentityBind: Bind[Identity] = new Bind[Identity] {
    def bind[A, B](f: A => Identity[B]) = a => id(f(a.value).value)
  }

  implicit val IdentityMonad: Monad[Identity] =
    Monad.monadBP[Identity]

  implicit val IdentityApplic: Applic[Identity] = IdentityMonad.applic

  implicit val IdentityApplicFunctor: ApplicFunctor[Identity] = IdentityMonad.applicFunctor

  implicit val IdentityApplicative: Applicative[Identity] = IdentityMonad.applicative

  implicit val IdentityJoin: Join[Identity] = IdentityMonad.join

  implicit def IdenitytCoPointed: CoPointed[Identity] = new CoPointed[Identity] {
    def coPoint[A] = a => a.value
  }

  implicit def IdentityCoJoin: CoJoin[Identity] = new CoJoin[Identity] {
    def coJoin[A] = a => Identity.id(a)
  }


}
