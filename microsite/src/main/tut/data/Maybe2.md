---
layout: docs
section: data
title:  "Maybe2"
---

# Maybe2 [![GitHub](../img/github.png)](https://github.com/scalaz/scalaz/blob/series/8.0.x/base/shared/src/main/scala/scalaz/data/maybe2.scala)

`Maybe2[A, B]` is isomorphic to `Maybe[(A, B)]`.
The only difference between `Maybe2[A, B]` and `Maybe[(A, B)]` is that the first takes less
memory.

`Maybe2` is used to implement `IList`; each cons cell is a `Maybe2`.

**Typical imports**

```tut:silent
import scalaz.data.Maybe2._
```

# Creation

```tut
just2("Hello", 42)
empty2
```
