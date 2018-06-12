---
layout: docs
title:  "Compose"
---

# Compose [![GitHub](../img/github.png)](https://github.com/scalaz/scalaz/blob/series/8.0.x/base/shared/src/main/scala/scalaz/ct/compose.scala)

`Compose[F, G, X]` is a wrapper around the composition of type constructors `F`
and `G`, applied to `X`. It is isomorphic to `F[G[X]]`, but comes with instances
derived by combining instances for `F` and `G`.

The following instances are provided:

| if `F` is...                           | and `G` is...                           | then `Compose[F, G, ?]` is ...         |
|----------------------------------------|-----------------------------------------|----------------------------------------|
| a [`Functor`](./Functor.html)          | a [`Functor`](./Functor.html)           | a [`Functor`](./Functor.html)          |
| an [`Apply`](./Apply.html)             | an [`Apply`](./Apply.html)              | an [`Apply`](./Apply.html)             |
| an [`Applicative`](./Applicative.html) | an [`Applicative`](./Applicative.html)  | an [`Applicative`](./Applicative.html) |
| an `InvariantFunctor`                  | an `InvariantFunctor`                   | an `InvariantFunctor`                  |
| `Contravariant`                        | a [`Functor`](./Functor.html)           | `Contravariant`                        |
| a [`Functor`](./Functor.html)          | `Contravariant`                         | `Contravariant`                        |
| `Foldable`                             | `Foldable`                              | `Foldable`                             |
| `Traversable`                          | `Traversable`                           | `Traversable`                          |
