package fjs

import fj.Class.clas

object Class {
  implicit def JavaClass_Class[A](c: java.lang.Class[A]) = clas(c)

  implicit def Class_JavaClass[A](c: fj.Class[A]) = c.clas
}
