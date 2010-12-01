package scalaz

sealed trait State[S, +A] {
  def apply(s: S): (S, A)

  import Scalaz._
  
  def map[B](f: A => B): State[S, B] = state(apply(_) match {
    case (s, a) => (s, f(a))
  })

  def flatMap[B](f: A => State[S, B]): State[S, B] = state(apply(_) match {
    case (s, a) => f(a)(s)
  })

  def !(s: S): A = apply(s)._2

  def ~>(s: S): S = apply(s)._1

  def withs(f: S => S): State[S, A] = state(f andThen (apply(_)))
}

/**
 * State monad transformer
 **/
sealed trait StateT[M[_], S, A] {
  def apply(s: S): M[(S, A)]

  import Scalaz._

  def !(s: S)(implicit m: Functor[M]): M[A] = apply(s) map (_._2)
  def ~>(s: S)(implicit m: Functor[M]): M[S] = apply(s) map (_._1)
  def maps[N[_], B](f: M[(S, A)] => N[(S, B)]): StateT[N, S, B] = 
    stateT((apply(_)) map f)
  def withs(f: S => S) =
    stateT(f map (apply(_)))
  def map[B](f: A => B)(implicit m: Functor[M]): StateT[M, S, B] =
    stateT(s => apply(s) map (_ :-> f))
  def flatMap[B](f: A => StateT[M, S, B])(implicit m: Bind[M]): StateT[M, S, B] =
    stateT[M,S,B](s => apply(s) >>= ((x: (S, A)) => x match { case (sp, a) => f(a)(sp) }))
}

trait States {
  def state[S, A](f: S => (S, A)): State[S, A] = new State[S, A] {
    def apply(s: S) = f(s)
  }

  def stateT[M[_], S, A](f: S => M[(S, A)]): StateT[M, S, A] =
    new StateT[M, S, A] {
      def apply(s: S) = f(s)
    }

  def init[S]: State[S, S] = state[S, S](s => (s, s))

  def modify[S](f: S => S) = init[S] flatMap (s => state(_ => (f(s), ())))

  def put[S](s: S) = state[S, Unit](_ => (s, ()))

  def gets[S,A](f: S => A): State[S, A] = 
    for (s <- init) yield f(s)
}
