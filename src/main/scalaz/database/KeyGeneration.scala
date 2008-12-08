package scalaz.database

sealed trait KeyGeneration {
  def asInt: Int
}
final case object GeneratedKeys extends KeyGeneration {
  def asInt = RETURN_GENERATED_KEYS
}
final case object NoGeneratedKeys extends KeyGeneration {
  def asInt = NO_GENERATED_KEYS
}

object KeyGeneration {
  def keyGenerations = List(GeneratedKeys, NoGeneratedKeys)

  def fromInt(n: Int) = keyGenerations find (_.asInt == n)
}
