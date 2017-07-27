package scalaz

package object control {
  type Lazy[A] = Unit => A
}
