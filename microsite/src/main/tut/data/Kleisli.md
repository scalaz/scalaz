---
layout: docs
section: data
title:  "Kleisli"
---

# Kleisli [![GitHub](../img/github.png)](https://github.com/scalaz/scalaz/blob/series/8.0.x/base/shared/src/main/scala/scalaz/tc/kleisli.scala)

Given a [Category](./Category.html) C, and a [Monad](./Monad.html) M over C, we can construct a new Category, called the `Kleisli Category` of M, whose objects are those of C, but whose morphisms are arrows of the form `A => M[B]`. The arrows of this new category are called `Kleisli Arrows`, or, more succinctly, `Kleisli`s.

# Basics

While this may seem like a complicated topic, in practice, the notion of a `Kleisli` is simple. Any function `A => M[B]` is a `Kleisli[F, A, B]`. The `Kleisli[F, A, B]` type provides a succinct newtype wrapper for easy costless conversion between `Kleisli` and its corresponding function type via two special functions, `wrapKleisli`, and `runKleisli`:

```tut:silent
import scalaz._
import Predef._
import Scalaz._
import data.Kleisli
import tc._

val f: Int => List[Boolean] = i => List(i < 2)

val k: Kleisli[List, Int, Boolean] = Kleisli.wrapKleisli(f)
val ff: Int => List[Boolean] = Kleisli.runKleisli(k)

```

Note the following:

```
Kleisli.wrapKleisli(Kleisli.runKleisli(k)) === k

// And

Kleisli.runKleisli(Kleisli.wrapKleisli(f)) === f
```

Additionally, we provide the following utility allowing one to translate from `Kleisli[F, A, B]` to `Kleisli[G, A, B]` according to some natural transformation (`~>`) for convenience:

```scala
def hoist[F[_], G[_], A, B](k: Kleisli[F, A, B])(η: F ~> G): Kleisli[G, A, B]
```


As with any function, the most important operations we can do, aside from application, is composition. Note that this poses a problem for `Kleisli[F, A, B]`. How do we compose `A => M[B]` and `B => M[C]`? This is where the magic begins. It turns out that when M is a `Bind`, then `Kleisli`s compose via M's `flatMap`:

```scala
def compose[F[_], A, B, C](
  j: Kleisli[F, B, C],
  k: Kleisli[F, A, B]
)(implicit B: Bind[F]): Kleisli[F, A, C] =
  a => B.flatMap(k(a))(j)
```

With a little imagination, we can see that the `Kleisli[F, ?, ?]` construction not only forms a new Category, but when M is a `Monad`, `Kleisli[F, A, ?]`s form a `Monad` as well!

# Functions

We provide the following functions in addition to `wrapKleisli`, `runKleisli`, `hoist`, and `compose` for `Kleisli`:

```scala
// analogous to `andThen` for functions
def andThen[C](
  j: Kleisli[F, B, C]
)(implicit M: Monad[F]): Kleisli[F, A, C]

// Converts an arrow `Kleisli[F, A, B]` into an arrow of pairs that is applied to the
// first component in the pair, but leaves the second component untouched
def first[C](implicit F: Functor[F]): Kleisli[F, (A, C), (B, C)] =

// The mirror image of `first`
def second[C](implicit F: Functor[F]): Kleisli[F, (C, A), (C, B)] =

// symbolic version of `andThen`
def >=>[C](j: Kleisli[F, B, C])(implicit M: Monad[F]): Kleisli[F, A, C]

// symbolic version of `compose`
def <=<[E](j: Kleisli[F, E, A])(implicit M: Monad[F]): Kleisli[F, E, B]

// reverse version of `>>=`
def =<<(fa: F[A])(implicit M: Monad[F]): F[B]

// Another symbolic version of `andThen`
def >>>[C](j: Kleisli[F, B, C])(implicit M: Monad[F]): Kleisli[F, A, C]

// Split the input between the two argument arrows and combine their output.
def ***[C, D](j: Kleisli[F, C, D])(
  implicit A: Apply[F]
): Kleisli[F, (A, C), (B, D)] =
  wrapKleisli(t => A.ap(runKleisli(j)(t._2))(A.map(runKleisli(k)(t._1))(a => (a, _))))

// Referred to as fanout, this function sends the input to both argument arrows and combine their output
def &&&[C](j: Kleisli[F, A, C])(
  implicit A: Apply[F]
): Kleisli[F, A, (B, C)]
```

Usage will vary, but as an example, we will show how some of the symbols may be used:

```tut:silent
import scalaz._
import data.Kleisli

val f: Int => List[Boolean] = i => List(i < 2)
val g: Boolean => List[String] = b => if(b) List("Scalaz") else List("Scala")

val k: Kleisli[List, Int, Boolean] = Kleisli.wrapKleisli(f)
val j: Kleisli[List, Boolean, String] = Kleisli.wrapKleisli(g)

k >=> j // : Kleisli[List, Int, String]

k >>> j // : Kleisli[List, Int, String]

val l: Kleisli[List, String, Int] = Kleisli.wrapKleisli(s => List(s.length))

k <=< l // : Kleisli[List, String, Boolean]

k =<< List(3) // : List(false): List[Boolean]
```

# Instances

`Kleisli` arrows are especially interesting, and have an instance for many of the typeclasses in the Scalaz ecosystem. Some of these include:

- [Monad](./Monad.html) for `Kleisli[M, A, ?]` when M is a `Monad`
- [Compose](./Compose.html) for `Kleisli[M, ?, ?]` when M is a `Bind`
- [Monoid](./Monoid.html) for `Kleisli[M, A, B]` when `M[B]` is any `Monoid`
- [Strong](./Strong.html) for `Kleisli[M, ?, ?]` where `M` is any `Functor`
