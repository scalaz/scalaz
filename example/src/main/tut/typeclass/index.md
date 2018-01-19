---
layout: docs
title:  "Type classes"
---

# Type classes

_In computer science, a type class is a type system construct that supports ad hoc polymorphism.
This is achieved by adding constraints to type variables in parametrically polymorphic types._ <sup>[1](#f1)</sup>

Type class instances allow us to add behaviour to types without changing the types themselves.

As an example, let us look at the [Show](./Show.html) type class, a safe alternative to the JVM's `Object.toString`.
It defines that all its instances implement a method `show` with the following signature:

```tut:silent
trait Show[A] {
  def show(a: A): String
}
```

Now assume that we have a class which we are unable or unwilling to change in order to give it a conversion into a `String` representation.

```tut:silent
final case class BusinessObject(id: Long, value: Long)
```

Using the `Show` type class, we can easily just define such a conversion:

```tut:silent
implicit val businessObjectShow: Show[BusinessObject] = new Show[BusinessObject] {
  def show(b: BusinessObject) = s"BO[${b.id} = ${b.value}]"
}
```

Now, whenever we would like to turn our `BusinessObject` into a String, we can do so using our `Show` instance.

```tut
val bo = BusinessObject(1234L, 1234567L)

businessObjectShow.show(bo)
```

However, this is not too convenient and very specific to our `BusinessObject`. Would it not be nicer to be able to say `bo.show`?

Luckily, **Scalaz** makes all this very easy.

We need something similar to the following:

```tut:silent
implicit final class ShowOps[A](self: A)(implicit s: Show[A]) {
  def show: String = s.show(self)
}
```

Luckily, most of this ships in Scalaz by default.

```tut:reset
import scalaz.Scalaz._

final case class BusinessObject(id: Long, value: Long)

implicit val businessObjectShow: Show[BusinessObject] = (b: BusinessObject) => s"BO[${b.id} = ${b.value}]"

BusinessObject(1234L, 1234567L).show
```

After showing that we can add behaviour, it should now be easy to see how type classes can be used as constraints for parametrically polymorphic functions.

For example, we could define a function `loudShow` as follows:

```tut:silent
def loudShow[A: Show](a: A) = a.show.toUpperCase
```

Notice that the type parameter `A` now has a constraint that requires it have an instance of `Show`.
The above `loudShow` can be used with our original `BusinessObject` class or any type that has a `Show` instance.

---

Footnotes

<b id="f1">1</b> <https://en.wikipedia.org/wiki/Type_class>
