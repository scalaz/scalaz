---
layout: docs
title:  "Unfoldable"
---

# Unfoldable [![GitHub](../img/github.png)](https://github.com/scalaz/scalaz/blob/series/8.0.x/base/shared/src/main/scala/scalaz/tc/unfoldable.scala)

*Whereas a `Foldable` allows folding data structures to values, `Unfoldable` identifies data structures which can be unfolded.*

The generating function `f` in `unfoldr(f)(b)` is understood as follows:

- if `f(b)` is `Empty2`, then `unfoldr(f)(b)` should represent an empty structure.
- if `f(b)` is `Just2(a, b1)`, then `unfoldr(f)(b)` should consist of an appended to the result of `unfoldr(f)(b1)`.

**Typical imports**

```tut:silent
import scalaz.tc._
import scalaz.data._
import scalaz.Scalaz._
```

# Usage

```tut
Unfoldable[IList].unfoldRight[Int, Int](b => if (b == 0) empty2 else just2(b, b - 1))(10)
```
