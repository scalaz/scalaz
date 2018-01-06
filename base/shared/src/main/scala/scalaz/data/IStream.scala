package scalaz
package data

import scalaz.control.Inf


trait IStreamModule {
  type IStream[A]

  def scons[A](a: =>A)(as: =>Id[IStream[A]]): IStream[A]
  def uncons[A](as: =>IStream[A]): (Inf[A], Inf[IStream[A]])
}

private[data] object IStreamImpl extends IStreamModule { self =>
  type IStream[A] = data.Cofree[Id, A]

  def scons[A](a: =>A)(as: => Id[IStream[A]]): IStream[A] =
    Cofree.wrapCofree[Id, A](a)(as)

  def uncons[A](as: =>IStream[A]): (Inf[A], Inf[IStream[A]]) =
    Cofree.runCofree[Id, A](as)
}
