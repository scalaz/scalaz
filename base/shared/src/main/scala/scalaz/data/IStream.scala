package scalaz
package data

trait IStreamModule {
  type IStream[A]

  def scons[A](a: A)(as: IStream[A]): IStream[A]
  def uncons[A](as: IStream[A]): (A, IStream[A])
}

private[data] object IStreamImpl extends IStreamModule {
  type IStream[A] = data.Cofree[Id, A]

  def scons[A](a: A)(as: IStream[A]): IStream[A] =
    Cofree.wrapCofree[Id, A](a)(as)

  def uncons[A](as: IStream[A]): (A, IStream[A]) =
    Cofree.runCofree[Id, A](as)
}
