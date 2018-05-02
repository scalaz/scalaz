---
layout: docs
title:  "Kleisli"
---

# Kleisli

Given a given `Category` C, and a `Monad` M over C, we can construct a new Category, called the `Kleisli Category` of M, whose objects are those of C, but whose morphisms are those arrows `A => M[B]`. The arrows of this new category are called `Kleisli Arrows`, or, more succinctly, `Kleisli`s.


## Basics

While this may seem like a complicated topic, in practice, the notion of a `Kleisli` is simple. Any function `A => M[B]` is a `Kleisli`. The `Kleisli` data object provides a succinct newtype wrapper for easy costless conversion between `Kleisli` and its corresponding function type via two special functions, `wrap`, and `run`:

```tut:silent
import scalaz._
import Scalaz._
import data._ // this gets us our Kleisli object

val f: Int => List[Boolean] = i => List(i < 2)

val k: Kleisli[F, Int, Boolean] = Kleisli.wrap(f)
val ff: Int => List[Boolean] = Kleisli.run(k)

```

Note the following:

```tut:silent
Kleisli.wrap(Kleisli.run(k)) === k

// And

Kleisli.run(Kleisli.wrap(f)) === f
```

As with any function, the most important operations we can do, aside from application, is composition. Note that this poses a problem for `Kleisli`. How do we compose `A => M[B]` and `B => M[C]`? This is where the magic begins. It turns out that when M is a `Monad`, then `Kleisli`s compose via M's `flatMap`:

```tut:silent

  override def compose[F[_], A, B, C](
    k: Kleisli[F, A, B],
    j: Kleisli[F, B, C]
  )(implicit M: Monad[F]): Kleisli[F, A, C] =
    a => M.flatMap(k(a))(j)
```

This is where things get interesting! With a little imagination, it turns out that the `Kleisli` construction not only forms a new Category, but when M is a `Monad`, `Kleisli`s form a `Monad` as well!

## Functions

We provide the following functions in addition to `wrap` and `run` for `Kleisli`:

```tut:silent

    // turn F into G via some natural transformation
    def hoist[G[_]](η: F ~> G): Kleisli[G, A, B]

    // analogous to `andThen` for functions
    def andThen[C](
      j: Kleisli[F, B, C]
    )(implicit M: Monad[F]): Kleisli[F, A, C]

    // analogous to `compose` for functions
    def compose[E](
      j: Kleisli[F, E, A]
    )(implicit M: Monad[F]): Kleisli[F, E, B]

    // symbolic version of `andThen`
    def >=>[C](j: Kleisli[F, B, C])(implicit M: Monad[F]): Kleisli[F, A, C]

    // symbolic version of `compose`
    def <=<[E](j: Kleisli[F, E, A])(implicit M: Monad[F]): Kleisli[F, E, B]

    // reverse version of `>>=`
    def =<<(fa: F[A])(implicit M: Monad[F]): F[B]

    // Split the input between the two argument arrows and combine their output. Note that this is in general not a functor.
    def ***[C, D](j: Kleisli[F, C, D])(
      implicit S: Strong[Kleisli[F, ?, ?]],
      M: Monad[F]
    ): Kleisli[F, (A, C), (B, D)]

    // Fanout: send the input to both argument arrows and combine their output
    def &&&[C](j: Kleisli[F, A, C])(
      implicit S: Strong[Kleisli[F, ?, ?]],
      M: Monad[F]
    ): Kleisli[F, A, (B, C)]

```

Usage will vary, but as an example, we will show how some of the symbols may be used:

```tut:silent

import scalaz._
import Scalaz._ // this reveals Kleisli functions aside from wrap and run
import data._ // this gets us our Kleisli object

val f: Int => List[Boolean] = i => List(i < 2)
val g: Boolean => List[String] = b => if(b) List("Scalaz") else List("Scala")

val k: Kleisli[List, Int, Boolean] = Kleisli.wrap(f)
val j: Kleisli[List, Boolean, String] = Kleisli.wrap(g)

k >=> j // : Kleisli[List, Int, String]

val l: Kleisli[List, String, Int] = Kleisli.wrap(s => List(s.size))

k <=< l // : Kleisli[List, String, Boolean]

k =<< List(3) // : List(false): List[Boolean]

```
## Instances

`Kleisli` arrows are especially interesting, and have an instance for many of the typeclasses in the Scalaz ecosystem. Some of these include:


- `Monad` where M is a `Monad`

- `Compose` where M is a `Monad`

- `Monoid` for `Kleisli[M, A, B]` when `M[B]` is any `Monoid`

- `Strong` where `M` is any `Functor`

