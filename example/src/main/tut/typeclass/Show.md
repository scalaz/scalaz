---
layout: docs
title:  "Show"
---

# Show

`Show` presents a safe and explicit alternative to `Object.toString`.
By providing instances of this typeclass, a type explicitly defines that it can be converted into human-readable output.

**Typical imports**

```tut:silent
import scalaz._
import Scalaz._
```

# Built-in instances

```tut
"Scalaz 8".show
```

# Instance declaration

```tut
case class Foo(a: Int)
implicit val fooShow: Show[Foo] = (f: Foo) => s"Foo[${f.a}]"
```

# Usage

```tut
val bad = Foo(1).toString
val good = Foo(1).show

import scalaz.data.Maybe
Maybe.just(Foo(1)).show
```

*Note that the `Show` instance for `Maybe` resolves against the `Show` instance for `Foo`.*
