package fjs.control.parallel

import fj.control.parallel.QueueActor
import fjs.Effect._
import fjs.Unit._


class Actor[A](aa: fj.control.parallel.Actor[A]) {
  def !(a: A) = aa.act(a)
  def !(a: fj.control.parallel.Promise[A]) = a.to(aa)
}

object Actor {

  implicit def actor[A](e: A => Unit)(implicit sa: fj.control.parallel.Strategy[fj.Unit]): fj.control.parallel.Actor[A] =
    fj.control.parallel.Actor.actor(sa, e)

  implicit def queueActor[A](e: A => Unit)(implicit sa: fj.control.parallel.Strategy[fj.Unit]): QueueActor[A] =
    QueueActor.queueActor(sa, e)

  implicit def QueueActor_Actor[A](qa: QueueActor[A]) = qa.asActor

  implicit def Bangop[A](a: fj.control.parallel.Actor[A]) = new Actor[A](a)

}