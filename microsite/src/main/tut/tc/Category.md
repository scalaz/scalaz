---
layout: scalaz
module: base
section: typeclass
source: tc/category.scala
title:  "Category"
---

A semicategory with identity.

A setting for basic category theory; a type `F[_, _]` which allows for
associative composition of any `F[A, B]` and `F[B, C]`, with `id[A]: F[A, A]`
provided as the identity.

# Law
