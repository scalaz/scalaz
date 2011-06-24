package scalaz

/**
 * This is an attempt at an alternative to DList, to provide an immutable list with
 * efficient cons and snoc.
 *
 * The structure operates similar to DList, but rather then store each difference
 * on the stack we utilise a cons list to store all functions. In order to maintain
 * efficient snoc we maintain two lists of changes, appends and prepends.
 *
 * How it works:
 *
 * A traditional DList is able to perform each difference function on the seed in turn. As the last
 * change sits at the top of the stack, it has access to the seed first, this aspect is what allows
 * both efficient cons and snoc, the last difference can either choose to act on the seed for snoc,
 * or delegate to the next change and act on the response for cons. This process comes at a cost,
 * of each operation chewing through a stack frame. In scala (or rather on the JVM) this cost makes
 * DList infeasible - afaik.
 *
 * This structure stores a list of differences, as a function of List[A] => List[A],
 * the eventual evaluation takes a seed list, and applys each difference in turn. In effect we have
 * moved the size cost from the stack to memory where the list of functions is stored. The naive
 * implementation (storing a single list of differences) means we can have efficient snoc with no
 * stack overhead, but cons either has the same stack issue or loses its efficiency.
 *
 * To solve for both snoc and cons, we utilise two lists: a list for differences that (for efficient
 * operation) require application after the current set of differences have been applied - referred
 * to as 'post' operations, an example of which is cons; and a list for differences that (again
 * for efficient operation) require application before the current set of differences have been
 * applied - referred to as 'pre' operations, an example of which is snoc.
 *
 * This structure does have a cost over and above that of a standard DList for prepending another
 * AltDList. This structure requires 2 ':::' list prepend operations, where DList only requires a
 * single ':::'. This does not significantly add to the algorithmic complexity, but it is still
 * less then desirable. There is a possible solution by modifying the core structure to be a list of
 * pre/post pairs, i.e.  List[(List[A] => List[A], List[A] => List[A])], this would allow nesting
 * to be achieved without the additional overhead, but requires more investigation.
 *
 *
 * Examples for my own sanity:
 *
 *
 *
 *  Operations                |    DList                   |    ConsList                                       |    AltDList
 *  j = emptyDList            |    j = xs => xs            |    Nil                                            |    j = post = Nil,                                        pre = Nil
 *  k = a :: j                |    k = xs => a :: j(xs)    |    a :: Nil                                       |    k = post = {xs => a :: xs} :: Nil,                     pre = Nil
 *  l = k ::> b               |    l = xs => k(b :: xs)    |    a :: b :: Nil                                  |    l = post = {xs => a :: xs} :: Nil,                     pre = {xs => b :: xs} :: Nil
 *  m = c :: l                |    m = xs => c :: l(xs)    |    c :: a :: b :: Nil                             |    m = post = {xs => c :: xs} :: {xs => a :: xs} :: Nil,  pre = {xs => b :: xs} :: Nil
 *  n = m ::> d               |    n = xs => m(d :: xs)    |    c :: a :: b :: d :: Nil                        |    n = post = {xs => c :: xs} :: {xs => a :: xs} :: Nil,  pre = {xs => d :: xs} :: {xs => b :: xs} :: Nil
 *                            |                            |                                                   |
 *  o = emptyDList            |    o = xs => xs            |    Nil                                            |    o = post = Nil,                                        pre = Nil
 *  p = o ::> e               |    p = xs => o(e :: xs)    |    e :: Nil                                       |    p = post = Nil,                                        pre = {xs => e :: xs} :: Nil
 *  q = f :: p                |    q = xs => f :: p(xs)    |    f :: e :: Nil                                  |    q = post = {xs => f :: xs} :: Nil,                     pre = {xs => e :: xs} :: Nil
 *  r = q ::> g               |    r = xs => q(g :: xs)    |    f :: e :: g :: Nil                             |    r = post = {xs => f :: xs} :: Nil,                     pre = {xs => g :: xs} :: {xs => e :: xs}:: Nil
 *  s = h :: r                |    s = xs => h :: r(xs)    |    h :: f :: e :: g :: Nil                        |    s = post = {xs => h :: xs} :: {xs => f :: xs} :: Nil,  pre = {xs => g :: xs} :: {xs => e :: xs} :: Nil
 *                            |                            |                                                   |
 *  t = n ::: s               |    t = xs => s(n(xs))      |    h :: f :: e :: g :: c :: a :: b :: d :: Nil    |    t = post = {xs => h :: xs} :: {xs => f :: xs} :: Nil,  pre = {xs => d :: xs} :: {xs => b :: xs} :: {xs => a :: xs} :: {xs => c :: xs} :: {xs => g :: xs} :: {xs => e :: xs} :: Nil
 *
 *
 *  DList evaluation of n.toList -
 *
 *  > n(Nil)                                  // seed with Nil
 *  > m(d :: Nil)                             // apply n
 *  > c :: l(d :: Nil)                        // apply m
 *  > c :: k(b :: (d :: Nil))                 // apply l
 *  > c :: (a :: j(b :: (d :: Nil)))          // apply k
 *  > c :: (a :: (b :: (d :: Nil)))           // apply j
 *  > c :: a :: b :: d :: Nil
 *
 *  DList evaluation of t.toList -
 *
 *  > s(n(Nil))                                                    // seed with Nil
 *  > s(c :: a :: b :: d :: Nil)                                   // apply n (evaluation above)
 *  > h :: r(c :: a :: b :: d :: Nil)                              // apply s
 *  > h :: q(g :: (c :: a :: b :: d :: Nil))                       // apply r
 *  > h :: (f :: p(g :: (c :: a :: b :: d :: Nil)))                // apply q
 *  > h :: (f :: o(e :: (g :: (c :: a :: b :: d :: Nil))))         // apply p
 *  > h :: (f :: (e :: (g :: (c :: a :: b :: d :: Nil))))          // apply o
 *  > h :: f :: e :: g :: c :: a :: b :: d :: Nil                  // apply o
 *
 *
 *
 *  AltDList evaluation of n.toList -
 *
 *  > n = post = {xs => c :: xs} :: {xs => a :: xs} :: Nil,                    pre = {xs => d :: xs} :: {xs => b :: xs} :: Nil
 *  > n = difference = {xs => d :: xs} :: {xs => b :: xs} :: {xs => a :: xs} :: {xs => c :: xs} :: Nil
 *  > n'    = {d :: Nil}
 *  > n''   = {b :: n'}     = {b :: d :: Nil}
 *  > n'''  = {a :: n''}    = {a :: b :: d :: Nil}
 *  > n'''' = {c :: n'''}   = {c :: a :: b :: d :: Nil}
 *
 *  AltDList evaluation of s.toList -
 *
 *  > s = post = {xs => h :: xs} :: {xs => f :: xs} :: Nil,                    pre = {xs => g :: xs} :: {xs => e :: xs} :: Nil
 *  > s = difference = {xs => g :: xs} :: {xs => e :: xs :: {xs => f :: xs} :: {xs => h :: xs} :: Nil
 *  > s'    = {g :: Nil}
 *  > s''   = {e :: n'}     = {e :: g :: Nil}
 *  > s'''  = {f :: n''}    = {f :: e :: g :: Nil}
 *  > s'''' = {c :: n'''}   = {c :: f :: e :: g :: Nil}
 *
 *  AltDList evaluation of t.toList -
 *
 *  > t = post = {xs => h :: xs} :: {xs => f :: xs} :: Nil,                    pre = {xs => d :: xs} :: {xs => b :: xs} :: {xs => a :: xs} :: {xs => c :: xs} :: {xs => g :: xs} :: {xs => e :: xs} :: Nil
 *  > t = difference = {xs => d :: xs} :: {xs => b :: xs} :: {xs => a :: xs} :: {xs => c :: xs} :: {xs => g :: xs} :: {xs => e :: xs} :: {xs => f :: xs} :: {xs => h :: xs} :: Nil
 *  > t'           = {d :: Nil}
 *  > t''          = {b :: t'}                                       = {b :: d :: Nil}
 *  > t'''         = {a :: t''}                                      = {a :: b :: d :: Nil}
 *  > t''''        = {c :: t'''}                                     = {c :: a :: b :: d :: Nil}
 *  > t'''''       = {g :: t''''}                                    = {g :: c :: a :: b :: d :: Nil}
 *  > t''''''      = {e :: t'''''}                                   = {e :: g :: c :: a :: b :: d :: Nil}
 *  > t'''''''     = {f :: t''''''}                                  = {f :: e :: g :: c :: a :: b :: d :: Nil}
 *  > t''''''''    = {h :: t''''''}                                  = {h :: f :: e :: g :: c :: a :: b :: d :: Nil}
 *
 *
 *  Other alternatives -
 *
 *  I have been thinking of further alternatives, but at this stage they are just random
 *  thoughts. One I will pursue further is to introduce a more sophisticated structure
 *  to represent differences. By using a ADT that encodes each optimisation we may be able to
 *  store a single list of differences and implement the fold of the final list of changes with
 *  a delay queue, so each 'difference' can say, execute now or delay until later.
 */
sealed trait AltDList[A] {

  import AltDList._

  val pre: List[List[A] => List[A]]
  val post: List[List[A] => List[A]]

  def apply(as: List[A]): List[A] = {
    val x = pre.foldLeft(as)((l, f) => f(l))
    post.reverse.foldLeft(x)((l, f) => f(l))
  }

  def toList: List[A] = apply(Nil)

  def head: Option[A] = toList.headOption

  def tail: Option[List[A]] = toList match {
    case Nil => None
    case _ :: t => Some(t)
  }

  def ::(a: A): AltDList[A] = new AltDList[A] {
    val pre = AltDList.this.pre
    val post = ((xs: List[A]) => a :: xs) :: AltDList.this.post
  }

  def ::>(a: A): AltDList[A] = new AltDList[A] {
    val pre = ((xs: List[A]) => a :: xs) :: AltDList.this.pre
    val post = AltDList.this.post
  }

  def :::(as: AltDList[A]): AltDList[A] = new AltDList[A] {
    val pre = (AltDList.this.pre ::: AltDList.this.post.reverse) ::: as.pre
    val post = as.post
  }

  def foldRight[B](b: B, f: (A, B) => B): B = toList.foldRight(b)(f)

  def map[B](f: A => B): AltDList[B] = foldRight[AltDList[B]](empty[B], f(_) :: _)

  def flatMap[B](f: A => AltDList[B]): AltDList[B] = foldRight[AltDList[B]](empty[B], f(_) ::: _)

  def foreach(f: A => Unit): Unit = toList foreach f

  override def toString = 'D' + toList.toString
}

trait AltDLists {
  def dlist[A](f: List[A] => List[A]): AltDList[A] = new AltDList[A] {
    val pre = List(f)
    val post = Nil
  }

  def empty[A]: AltDList[A] = dlist(identity(_: List[A]))
}

object AltDList extends AltDLists