package scalaz
package data

trait BiconstInstances {
  trait BiconstCompose[M] extends Compose[Biconst[M, ?, ?]] {
    val S: Semigroup[M]

    def compose[A, B, C](bc: Biconst[M, B, C], ab: Biconst[M, A, B]): Biconst[M, A, C] =
      Biconst(S.append(Biconst.runBiconst(ab), Biconst.runBiconst(bc)))
  }

  implicit def biconstCompose[M](implicit M: Semigroup[M]): Compose[Biconst[M, ?, ?]] = new BiconstCompose[M] {
    val S: Semigroup[M] = M
  }
}