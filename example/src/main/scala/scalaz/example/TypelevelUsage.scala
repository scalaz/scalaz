package scalaz.example

import scalaz._
import Scalaz._
import typelevel._
import Typelevel._

object TypelevelUsage extends App {

  def typed[T](t: T) = ()

  object HLists {

    val hlist1 = 3 :: HNil
    val hlist2 = "foo" :: hlist1
    
    typed[Int :: HNil](hlist1)
    typed[String :: Int :: HNil](hlist2)

    hlist2 match {
      case str :: n :: _ =>
        val _str: String = str
        val _n: Int = n
    }

  }

  object KLists {

    val klist1 = None :^: Option(3) :^: Some("foo") :^: KNil
    val klist2 = klist1.append(klist1)

    klist1 match {
      case optionInt1 :^: optionInt2 :^: someString :^: KNil =>
        typed[Option[Int]](optionInt1)
        typed[Option[Int]](optionInt2)
        typed[Some[String]](someString)
    }
    
    val klist3 = klist1.fold[Option, GenericList[Option], HFold.Append[Option, klist1.type]](new HFold.Append[Option, klist1.type](klist1))

    typed[GenericCons[Option, Nothing, GenericCons[Option, Int, GenericCons[Some, String, GenericNil[Nothing]]]]](klist1)
    typed[GenericCons[Option, Nothing, GenericCons[Option, Int, GenericCons[Option, String, GenericCons[Option, Nothing, GenericCons[Option, Int, GenericCons[Some, String, GenericNil[Nothing]]]]]]]](klist2)
    typed[GenericCons[Option, Nothing, GenericCons[Option, Int, GenericCons[Option, String, GenericCons[Option, Nothing, GenericCons[Option, Int, GenericCons[Some, String, GenericNil[Nothing]]]]]]]](klist3)

  }

  object Kleislists {

    val f1: Int => Option[String] = { n => Some(n.toString) }
    val f2: String => Option[Float] = { s => s.parseFloat.toOption }

    val kleislist1 = f1 :: f2 :: HNil
    val fReverseCompose = kleislist1.reverseCompose

    val kleislist2 = f2 :: f1 :: HNil
    val fCompose = kleislist2.compose

    typed[Kleisli[Option, Int, Float]](fReverseCompose)
    typed[Kleisli[Option, Int, Float]](fCompose)

    val f3: Int => String = { _.toString }
    val f4: String => Float = { _.toFloat }

    val kleislist3 = f3 :: f4 :: HNil
    val fIdReverseCompose = kleislist3.reverseCompose

    typed[Kleisli[Id, Int, Float]](fIdReverseCompose)

  }

  object Folding {

    import KLists._

    object SomeCount extends HFold[Option, Int] {
      type Init = Int
      def init = 0

      type Apply[E, A <: Int] = Int
      def apply[E, A <: Int](elem: Option[E], acc: A) = 
        if (elem.isDefined) acc + 1
        else acc
    }

    val count1 = klist1.fold[Option, Int, SomeCount.type](SomeCount)
    assert(count1 === 2)

    val count2 = klist1.fold[Option, Int, HFold.Count[Option]](new HFold.Count[Option])
    assert(count2 === 3)

  }

  object ALists {

    import KLists._

    val aplist = klist1.coerce[Option]
    val func: (Nothing, Int, String) => Double = { (x, y, z) => 2.0 }

    val afunc = Option(func.curried)

    assert(klist1(afunc) === None)
    assert(aplist(afunc) === None)

  }

  object Downed {

    import ALists._

    val downed = aplist.down

    typed[Option[Nothing] :: Option[Int] :: Option[String] :: HNil](downed)

    assert(downed == aplist)

  }

  object Reversed {

    import KLists._

    val rev = klist1.fold[Option, GenericList[Option], HFold.Reverse[Option]](new HFold.Reverse[Option])

    typed[GenericCons[Option, String, GenericCons[Option, Int, GenericCons[Option, Nothing, GenericNil[Option]]]]](rev)

  }

  object Naturals {

    assert(_3.value === 3)

    val hlist = "foo" :: 3 :: 'a :: HNil

    val e0 = hlist.at(_0)
    val e1 = hlist.at(_1)
    val e2 = hlist.at(_2)

    typed[String](e0)
    typed[Int](e1)
    typed[Symbol](e2)

    assert(e0 == "foo")
    assert(e1 == 3)
    assert(e2 == 'a)

    import KLists._

    // Compiling the following snippets takes quite long, so try to avoid access
    // by index. It gets even worse if you use an index greater than _4.

    val f0 = klist2.at(_4)

    typed[Option[Int]](f0)

    assert(f0 === Some(3))

  }

  object Classes {

    val composed = Applicative[List] <<: Applicative[Option] <<: Applicative.compose

    assert(List(Some(5)) === composed.point(5))

    val prod = Applicative[List] *: Applicative[Option] *: Applicative.product

    assert(List("1") :: Option("2") :: HNil == prod.map(List(1) :: Option(2) :: HNil)(_.toString))

  }

  HLists
  KLists
  Kleislists
  Folding
  ALists
  Downed
  Reversed
  Naturals
  Classes

}

// vim: expandtab:ts=2:sw=2

