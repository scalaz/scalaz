---
layout: docs
title:  "Category"
---

# Category [![GitHub](../img/github.png)](https://github.com/scalaz/scalaz/blob/series/8.0.x/base/shared/src/main/scala/scalaz/tc/category.scala)


A semicategory with identity.

A setting for basic category theory; a type `F[_, _]` which allows for
associative composition of any `F[A, B]` and `F[B, C]`, with `id[A]: F[A, A]`
provided as the identity.

# Law

