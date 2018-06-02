---
layout: docs
title:  "Maybe2"
---

# Maybe2

`Maybe2` is a specialized version of [Maybe](./Maybe.html) that avoids allocation of `Tuple2` instances.

**Typical imports**

```tut:silent
import scalaz.data.Maybe2._
```

## Creation

```tut
just2("Hello", 42)
empty2
```
