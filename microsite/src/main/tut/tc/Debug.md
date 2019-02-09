---
layout: scalaz
module: base
section: typeclass
source: tc/debug.scala
title:  "Debug"
---

Debug presents a safe and explicit alternative to [Object#toString](https://docs.oracle.com/javase/8/docs/api/java/lang/Object.html#toString--).
By providing instances of this typeclass, a type explicitly defines that it can be converted into human-readable output.

**Typical imports**

```tut:silent
import scalaz.Scalaz._
```

# Built-in instances

```tut:book
"Scalaz 8".debug
```

# Instance declaration

```tut
import scalaz.tc.{Debug, DebugClass}
import scalaz.data.Cord

case class Foo(a: Int)

implicit val fooDebug: Debug[Foo] =
  DebugClass.instance[Foo](a => Cord.wrap("Foo[", Cord(a.toString), "]"))
```

# Usage

```tut
val bad = Foo(1).toString
val good = Foo(1).debug

import scalaz.data.Maybe
Maybe.just(Foo(1)).debug
```

*Note that the `Debug` instance for [Maybe](../data/Maybe.html) resolves against the `Debug` instance for `Foo`.*

# String interpolator

Scalaz contains a custom string interpolator that takes advantage of `Debug`.
Inside the `z` String interpolation, only objects that have `Debug` instances can be used.

```tut
case class Bar(a: Int)

val foo = Foo(123)
val bar = Bar(234)

val interpolated = z"${foo}"
```

The following fails to compile as there is no `Debug` instance for `Bar`

```tut:fail
z"${bar}"
```

# Law

There are no laws for `Debug`, unlike Haskell's `Show`.

Just try to keep it readable.
