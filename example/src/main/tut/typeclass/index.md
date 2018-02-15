---
layout: docs
title:  "Type classes"
---

# Type classes

_In computer science, a type class is a type system construct that supports ad hoc polymorphism.
This is achieved by adding constraints to type variables in parametrically polymorphic types._ <sup>[1](#f1)</sup>

Type class instances allow us to add behaviour to types without changing the types themselves.

As an example, let us look at the [Debug](./Debug.html) type class, a safe alternative to the JVM's `Object.toString`.
It defines that all its instances implement a method `debug` with the following signature:

```tut:silent
trait Debug[A] {
  def debug(a: A): String
}
```

Now assume that we have a class which we are unable or unwilling to change in order to give it a conversion into a `String` representation.

```tut:silent
final case class BusinessObject(id: Long, value: Long)
```

Using the `Debug` type class, we can easily just define such a conversion:

```tut:silent
implicit val businessObjectDebug: Debug[BusinessObject] = new Debug[BusinessObject] {
  def debug(b: BusinessObject) = s"BO[${b.id} = ${b.value}]"
}
```

Now, whenever we would like to turn our `BusinessObject` into a String, we can do so using our `Debug` instance.

```tut
val bo = BusinessObject(1234L, 1234567L)

businessObjectDebug.debug(bo)
```

However, this is not too convenient and very specific to our `BusinessObject`. Would it not be nicer to be able to say `bo.debug`?

Luckily, **Scalaz** makes all this very easy.

We need something similar to the following:

```tut:silent
implicit final class DebugOps[A](self: A)(implicit s: Debug[A]) {
  def debug: String = s.debug(self)
}
```

Luckily, most of this ships in Scalaz by default.

```tut:reset
import scalaz.Scalaz._
import scalaz.typeclass.DebugClass

final case class BusinessObject(id: Long, value: Long)

implicit val businessObjectDebug: Debug[BusinessObject] = instanceOf[DebugClass[BusinessObject]](b => s"BO[${b.id} = ${b.value}]")

BusinessObject(1234L, 1234567L).debug
```

After showing that we can add behaviour, it should now be easy to see how type classes can be used as constraints for parametrically polymorphic functions.

For example, we could define a function `loudDebug` as follows:

```tut:silent
def loudDebug[A: Debug](a: A) = a.debug.toUpperCase
```

Notice that the type parameter `A` now has a constraint that requires it have an instance of `Debug`.
The above `loudDebug` can be used with our original `BusinessObject` class or any type that has a `Debug` instance.

---

Footnotes

<b id="f1">1</b> <https://en.wikipedia.org/wiki/Type_class>
