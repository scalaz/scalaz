package scalaz.test

sealed trait Gen[+A] {
  val f: (Int, Rand) => A

  def apply(sz: Int)(implicit r: Rand) = f(sz, r)

  import S._
  
  def arbitrary = Arbitrary.arbitrary(this)

  def filter(f: A => Boolean): Gen[A] = Gen.gen((sz, rd) => {
    var a: A = this(sz)(rd)

    while(!f(a)) {
      a = this(sz)(rd)
    }
    
    a
  })

  def resize(sz: Int) = Gen.gen((_, r) => apply(sz)(r))
  
  def |||[AA >: A](g: => Gen[AA]): Gen[AA] =
    (0 >--> 1) >>= (n => (n == 0) ? g | this)

  def replicate[M[_], AA >: A](n: Int)(implicit p: Pure[M], m: Monoid[M[AA]]): Gen[M[AA]] =
    for(k <- this) yield (k: AA).replicate[M](n)

  def many[M[_], AA >: A](implicit p: Pure[M], m: Monoid[M[AA]]): Gen[M[AA]] = Gen.sized(sz => for(n <- 0 >--> sz; c <- replicate[M, AA](n)) yield c)

  def many1[M[_], AA >: A](implicit p: Pure[M], m: Monoid[M[AA]]): Gen[M[AA]] = Gen.sized(sz => for(n <- 1 >--> sz; c <- replicate[M, AA](n)) yield c)

  def forall[AA >:A](implicit s: Shrink[AA], t: Testable[AA]) = Property.property(Gen.gen((sz, r) => {
    def failed(o: Option[(AA, Status)]) = o exists (_._2.failed)

    def first(as: Stream[AA], shrinks: Int): Option[(AA, Status)] = {
      val rs = as map (a => {
        val k = (try {
          t test a
        } catch {
          case t => Property.property(Status.exception(Nil, t).gen)
        }).gen(sz)(r)

        if(k.isUndecided) None
        else Some((a, k))
      })

      if(rs.isEmpty) None
      else rs.find(failed(_))| rs.head
    }

    var x = first(Stream(this(sz)(r)), 0)

    (if(failed(x)) {
      var or: Option[Status] = null
      var shrinks = 0

      do {
        shrinks = shrinks + 1
        or = x map (_._2)
        x = first(s shrink x.get._1, shrinks)
      } while(failed(x))
      
      or
    } else
      x.map(_._2)) | Status.undecided
  }))
}

object Gen {
  def gen[A](k: (Int, Rand) => A) = new Gen[A] {
    val f = k
  }

  def gens[A](k: Int => A) = gen((sz, _) => k(sz))

  def genr[A](k: Rand => A) = gen((_, r) => k(r))

  import S._

  def fail[T]: Gen[T] = gen((_, _) => error("Failing generator"))

  def parameterised[A](f: (Int, Rand) => Gen[A]) = gen(f >>= (_.f))

  def sized[A](f: Int => Gen[A]): Gen[A] = parameterised((sz, _) => f(sz))

  def randomised[A](f: Rand => Gen[A]): Gen[A] = parameterised((_, r) => f(r))

  def frequency[A](k: (Int, Gen[A])*): Gen[A] = {
    def pick(n: Int, l: List[(Int, Gen[A])]): Gen[A] = l match {
      case Nil => fail
      case (a, b) :: k => if(n <= a) b else pick(n - a, k)
    }

    for(n <- 1 >--> (k.map(_._1) :\ 0)(_ + _);
        z <- pick(n, k.toList)) yield z
  }

  def freq[A](k: (Int, A)*): Gen[A] = frequency(k map (_ :-> (_.gen)): _*)

  def oneOf[A](k: Gen[A]*) = if(k.isEmpty) fail else for(i <- 0 >--> k.length - 1;
                                                           x <- k(i)) yield x
   
  implicit val GenFunctor: Functor[Gen] = new Functor[Gen] {
    def fmap[A, B](r: Gen[A], f: A => B) = gen((sz, rd) => f(r(sz)(rd)))
  }

  implicit val GenBind: Bind[Gen] = new Bind[Gen] {
    def bind[A, B](a: Gen[A], f: A => Gen[B]) = gen((sz, rd) =>
      f(a(sz)(rd))(sz)(rd))
  }

  implicit val GenPure: Pure[Gen] = new Pure[Gen] {
    def pure[A](a: => A) = gen((_, _) => a)
  }

  implicit val GenApply: Apply[Gen] = new Apply[Gen] {
    def apply[A, B](f: Gen[A => B], a: Gen[A]) = gen((sz, rd) =>
      f(sz)(rd)(a(sz)(rd)))
  }

  implicit val GenApplicative: Applicative[Gen] = Applicative.applicative[Gen]

  implicit val GenMonad: Monad[Gen] = Monad.monad[Gen]
}