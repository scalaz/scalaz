package scalaz
package data

trait BiconstModule {
  type Biconst[A, B, C]

  def retag[A, B1, C1, B2, C2](bc: Biconst[A, B1, C1]): Biconst[A, B2, C2]
  
  def runBiconst[A, B, C](bc: Biconst[A, B, C]): A

  def apply[A, B, C](a: A): Biconst[A, B, C]
}

private[data] object BiconstImpl extends BiconstModule with BiconstInstances {
  type Biconst[A, B, C] = A

  def retag[A, B1, C1, B2, C2](bc: A): A = bc

  def runBiconst[A, B, C](bc: A): A = bc

  def apply[A, B, C](a: A): A = a
}
