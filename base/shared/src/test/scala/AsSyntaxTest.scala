package scalaz

import scala.{ AnyRef, AnyVal, Either }

import Predef._
import prop.{<~<, IsCovariant}
import Scalaz._

class AsSyntaxTest {
  val ev1: Either[Int, String] <~< Either[AnyVal, AnyRef] =
    implicitly[Int <~< AnyVal] liftCvCv [Either] implicitly[String <~< AnyRef]

  val ev2: Either[Int, String] <~< Either[AnyVal, AnyRef] =
    (implicitly[Int <~< AnyVal] and implicitly[String <~< AnyRef]).liftCvCv[Either]

  def ev3[F[_]: IsCovariant](fa: F[Int]): Unit = {
    val (_, _) = (implicitly[Int <~< AnyVal].liftCvF[F], implicitly[Int <~< AnyVal].substCvF[F](fa): F[AnyVal])
  }
}
