package fj.data

import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen.{lzy, sized, resize, choose}
import org.scalacheck.Gen
import ArbitraryList.arbitraryList
import ArbitraryList.{listOf}
import Tree.{node, leaf}
import Math.{round, sqrt}

object ArbitraryTree {
  implicit def arbitraryTree[A](implicit a: Arbitrary[A]): Arbitrary[Tree[A]] = {
    def tree(implicit a:Arbitrary[A], n:Int, g:Gen[A]) : Gen[Tree[A]] =  n match {
      case 0 => g.map(leaf(_))
      case n => choose(0, 10).flatMap((i) => lzy(g.map2(resize(i, listOf(tree(a, n/2, g)))) (node(_, _))))
    }
    Arbitrary(sized(tree(a, _, arbitrary[A])))
  }
}
