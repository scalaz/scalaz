package scalaz

import org.scalacheck.Properties

trait SpecLitePlatform {
  def updateName: Unit = {
    val f = classOf[Properties].getDeclaredField("name")
    f.setAccessible(true)
    f.set(this, getClass.getName.stripSuffix("$"))
  }
}
