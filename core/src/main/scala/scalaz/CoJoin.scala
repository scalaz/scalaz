package scalaz

trait CoJoin[M[_]] {
  def coJoin[A]: M[A] => M[M[A]]
}

object CoJoin extends CoJoins

trait CoJoins {
  implicit def Tuple1CoJoin: CoJoin[Tuple1] = new CoJoin[Tuple1] {
    def coJoin[A] = a => Tuple1(a)
  }

  implicit def Tuple2CoJoin[R]: CoJoin[({type λ[α]=(R, α)})#λ] = new CoJoin[({type λ[α]=(R, α)})#λ] {
    def coJoin[A] = a => (a._1, a)
  }

  implicit def Function0CoJoin: CoJoin[Function0] = new CoJoin[Function0] {
    def coJoin[A] = a => () => a
  }

  import java.util.concurrent.Callable

  implicit def CallableCoJoin: CoJoin[Callable] = new CoJoin[Callable] {
    def coJoin[A] = a => new Callable[Callable[A]] {
      def call = a
    }
  }

  import java.util.Map.Entry
  import java.util.AbstractMap.SimpleImmutableEntry

  implicit def MapEntryCoJoin[X]: CoJoin[({type λ[α]=Entry[X, α]})#λ] = new CoJoin[({type λ[α]=Entry[X, α]})#λ] {
    def coJoin[A] = a => new SimpleImmutableEntry(a.getKey, a)
  }

  implicit def IdentityCoJoin: CoJoin[Identity] = new CoJoin[Identity] {
    def coJoin[A] = a => Identity.id(a)
  }

}
