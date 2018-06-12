---
layout: docs
title:  "Semicategory"
---

# Semicategory [![GitHub](../img/github.png)](https://github.com/scalaz/scalaz/blob/series/8.0.x/base/shared/src/main/scala/scalaz/ct/semicategory.scala)

A `Semicategory` is a binary type constructor with an associative binary 
operation `compose`. It is a [`Category`](./Category.html) that may not have
an identity element `F[A, A]` for every `A`.
