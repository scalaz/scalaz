package scalaz
package data

import scalaz.Predef._
import scalaz.prop.IsCovariant
import tc._

sealed abstract class EvalModule {
  type Eval[A]

  /** Call by value */
  def value[A](a: A): Eval[A]

  /** Call by need */
  def need[A](a: () => Eval[A]): Eval[A]

  /** Call by name */
  def name[A](a: () => Eval[A]): Eval[A]

  def eval[A](thunk: Eval[A]): A

  private[data] def isCovariant: IsCovariant[Eval]
}

object EvalModule {
  implicit def isCovariant: IsCovariant[Eval] = Eval.isCovariant

  implicit def delay[A]: Delay[Eval[A]] = instanceOf[DelayClass[Eval[A]]](Eval.need[A] _)
}

object Eval {
  val module: EvalModule = EvalModuleImpl

  def unapply[A](thunk: Eval[A]): Evaluated[A] = new Evaluated(Eval.eval(thunk))

  import scala.language.implicitConversions
  @scala.inline
  implicit def evalModule(eval: Eval.type): module.type = eval.module
}

final class Evaluated[A] private[data] (private val value: A) extends scala.AnyVal {
  def isEmpty: Boolean = false
  def get: A           = value
}

private object EvalModuleImpl extends EvalModule {

  override type Eval[A] = scala.Any

  override def value[A](a: A): Eval[A] = a

  override def need[A](a: () => Eval[A]): Eval[A] = new Need[A](a)

  override def name[A](a: () => Eval[A]): Eval[A] = new Name[A](a)

  override def eval[A](thunk: Eval[A]): A = thunk match {
    case n: Need[A] @scala.unchecked => n.eval
    case n: Name[A] @scala.unchecked => n.eval
    case a: A @scala.unchecked       => a
  }

  override def isCovariant: IsCovariant[Eval] = IsCovariant.unsafeForce

  private final class Name[A](private val run: () => Eval[A]) {

    def eval: A = {
      var current      = this.run
      var evaluated: A = null.asInstanceOf[A]
      var continue     = true
      do {
        current() match {
          case n: Name[A] @scala.unchecked =>
            current = n.run
          case n: Need[A] @scala.unchecked =>
            current = n.run
            if (current eq null) {
              evaluated = n.value
              continue = false
            }
          case a: A @scala.unchecked =>
            evaluated = a
            continue = false
        }
      } while (continue)
      evaluated
    }
  }

  private final class Need[A](@scala.volatile private[EvalModuleImpl] var run: () => Eval[A]) {

    private[EvalModuleImpl] var value: A = _

    def eval: A =
      if (run eq null)
        value
      else runEval

    private def runEval: A = this.synchronized {
      var current      = this
      var evaluated: A = null.asInstanceOf[A]
      var continue     = true
      do {
        val run = current.run
        if (run eq null) {
          evaluated = current.value
          continue = false
        } else
          run() match {
            case n: Need[A] @scala.unchecked =>
              current = n
            case n: Name[A] @scala.unchecked =>
              evaluated = n.eval
              continue = false
            case a: A @scala.unchecked =>
              evaluated = a
              continue = false
          }
      } while (continue)

      this.value = evaluated
      this.run = null
      evaluated
    }
  }
}
