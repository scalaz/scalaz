---
layout: docs
title:  "Debug"
---

# Debug

`Debug` presents a safe and explicit alternative to `Object.toString`.
By providing instances of this typeclass, a type explicitly defines that it can be converted into human-readable output.

**Typical imports**

```tut:silent
import scalaz.Scalaz._
```

# Built-in instances

```tut
"Scalaz 8".debug
```

# Instance declaration

```tut
import scalaz.typeclass.DebugClass

case class Foo(a: Int)
implicit val fooDebug: Debug[Foo] = instanceOf[DebugClass[Foo]](f => s"Foo[${f.a}]")
```

# Usage

```tut
val bad = Foo(1).toString
val good = Foo(1).debug

import scalaz.data.Maybe
Maybe.just(Foo(1)).debug
```

*Note that the `Debug` instance for `Maybe` resolves against the `Debug` instance for `Foo`.*
