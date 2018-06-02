---
layout: docs
title:  "Maybe2"
---

# Maybe2 [![GitHub](../img/github.png)](https://github.com/scalaz/scalaz/blob/series/8.0.x/base/shared/src/main/scala/scalaz/data/maybe2.scala)

`Maybe2` is a specialized version of [Maybe](./Maybe.html) that avoids allocation of [Tuple2](https://www.scala-lang.org/api/current/scala/Tuple2.html) instances.

**Typical imports**

```tut:silent
import scalaz.data.Maybe2._
```

# Creation

```tut
just2("Hello", 42)
empty2
```
