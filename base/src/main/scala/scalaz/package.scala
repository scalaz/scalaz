import scalaz.data.FunctionH

package object scalaz extends scalaz.BaseHierarchy {
  /** A [[scalaz.data.FunctionH]][F, G]. */
  type ~>[-F[_], +G[_]] = FunctionH[F, G]

}

