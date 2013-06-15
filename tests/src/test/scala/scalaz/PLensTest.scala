package scalaz

import std.AllInstances._

class PLensTest extends Spec {
  import PLens._

  "list head" in {
    listHeadPLens[Int].get(List(1, 2)) must be_===(Some(1))
    listHeadPLens[Int].get(Nil) must be_===(None)
  }

  object instances {
    def category = Category[PLens]
    def choice = Choice[PLens]
    def split = Split[PLens]
  }

}
