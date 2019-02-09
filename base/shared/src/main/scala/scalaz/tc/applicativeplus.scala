package scalaz
package tc

trait ApplicativePlusClass[F[_]] extends ApplicativeClass[F] with PlusEmptyClass[F]
