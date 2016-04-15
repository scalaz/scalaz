package scalaz

object NeedTestJVM extends SpecLite {

  "Need" should {
    "clear the Function0 reference" in {
      @volatile var flag = false
      val method = ThreadSafeNeed.getClass.getMethod("apply", classOf[Function0[_]])
      val need = method.invoke(
        ThreadSafeNeed,
        new runtime.AbstractFunction0[String]{
          override def finalize = {flag = true}
          override def apply = ""
        }
      ).asInstanceOf[Need[String]]

      flag must_== false
      print(need.value)
      System.gc()
      System.runFinalization()
      flag must_== true
    }
  }

}
