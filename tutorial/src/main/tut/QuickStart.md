QuickStart
==

Prelude
--

Scalaz is based around a concept of `Prelude`,  a default one is provided enabling all core features.

```tut
import scalaz.Prelude._

val f: Int => String \/ Double = 
  i => if (i > 10) right(i + 0.5) else left("too small")

println((8 to 12).toList.traverse(f))

println((12 to 16).toList.traverse(f))
```

It's possible (or even recommended in case of a big proprietary stack) to create your own custom prelude.

```tut
{
  import scalaz._

  object AcmePrelude extends data.DisjunctionFunctions {
    type \/[L, R] = data.Disjunction.\/[L, R]
  }


  import AcmePrelude._

  def f(i: Int): String \/ Double = 
    if (i > 10) right(i + 0.5) else left("too small")

  println(f(42))
}
```

*If you are a library author and are writing an extension to `scalaz`, please follow the same design principles as the `base` library itself.*
