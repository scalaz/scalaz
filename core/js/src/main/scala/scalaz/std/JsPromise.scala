package scalaz
package std

import scala.scalajs.js
import scala.scalajs.js.JSConverters._
import scala.scalajs.js.|

trait JSPromiseInstances { this: jsPromise.type =>
  implicit def jsPromiseSemigroup[A](implicit A: Semigroup[A]): Semigroup[js.Promise[A]] =
    Semigroup.liftSemigroup[js.Promise, A](JSPromiseInstance, A)
}

object jsPromise extends JSPromiseInstances {

  implicit def jsPromiseMonoid[A](implicit A: Monoid[A]): Monoid[js.Promise[A]] =
    Monoid.liftMonoid[js.Promise, A]

  implicit object JSPromiseInstance extends Nondeterminism[js.Promise] with MonadError[js.Promise, Any] with Cobind[js.Promise] {

    def cobind[A, B](fa: js.Promise[A])(f: js.Promise[A] => B): js.Promise[B] =
      point(f(fa))

    @inline
    private def promiseToUnion[A](promise: js.Promise[A]): A | js.Thenable[A]
      = |.from(promise)(|.Evidence.right(|.Evidence.base))

    @inline
    private def valueToUndefOr[A](a: A): js.UndefOr[A]
      = |.from(a)(|.Evidence.left(|.Evidence.base))

    @inline
    def point[A](a: => A): js.Promise[A] = js.Promise.resolve[A](a)
    def bind[A, B](fa: js.Promise[A])(f: A => js.Promise[B]): js.Promise[B] = fa.`then`[B]{ a => promiseToUnion(f(a)) }

    def chooseAny[A](head: js.Promise[A], tail: IList[js.Promise[A]]): js.Promise[(A, IList[js.Promise[A]])] = {
      chooseAny(ICons(head, tail))
    }

    private def chooseAny[A](nonEmptyList: ICons[js.Promise[A]]): js.Promise[(A, IList[js.Promise[A]])] = {
      js.Promise.race(
        Foldable[IList].toList(nonEmptyList).view.map { jsPromise =>
          map(jsPromise) { a =>
            (a, nonEmptyList.filter(_ ne jsPromise))
          }
        }.toJSIterable) 
    }

    override def firstCompletedOf[A](as: Iterable[js.Promise[A]]): Option[js.Promise[A]] =
      if (as.isEmpty) {
        None
      } else {
        Some(js.Promise.race(as.toJSIterable))
      }

    override def gather[A](fs: IList[js.Promise[A]]): js.Promise[IList[A]] =
      map(js.Promise.all(Foldable[IList].toList(fs).toJSIterable)) { jsIterable =>
        IList.fromList(jsIterable.toList)
      }

    def raiseError[A](e: Any): js.Promise[A] =
      js.Promise.reject(e)

    def handleError[A](fa: js.Promise[A])(f: Any => js.Promise[A]): js.Promise[A] = {
      val jsFunction: js.Function1[Any, A | js.Thenable[A]] = { e =>
        promiseToUnion(f(e))
      }
      fa.`catch`(valueToUndefOr(jsFunction))
    }
  }
}
