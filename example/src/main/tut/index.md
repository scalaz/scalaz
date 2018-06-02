---
layout: docs
title: "Home"
---

**Welcome to the Scalaz 8 examples**

Scalaz is a Scala library for functional programming.

It provides purely functional data structures to complement those from the Scala standard library. It defines a set of foundational type classes (e.g. `Functor`, `Monad`) and corresponding instances for a large number of data structures.

# Getting started

Include Scalaz in your project by adding the following to your `build.sbt`:

```tut:evaluated
println(s"""libraryDependencies += "org.scalaz" %% "scalaz-base" % "${scalaz.BuildInfo.version}"""")
```

# Modules

Scalaz provides additional modules for functionality beyond the basics included in `scalaz-base`

- `scalaz-effect`:  General-purpose effect monad
- `scalaz-std`: Typeclass instances for the Scala standard library

```tut:evaluated
println(s"""libraryDependencies += "org.scalaz" %% "scalaz-effect" % "${scalaz.BuildInfo.version}"""")
println(s"""libraryDependencies += "org.scalaz" %% "scalaz-std" % "${scalaz.BuildInfo.version}"""")
```

# Community

- Community: [Mobilize](https://scalaz.mobilize.io/)
- Gitter: [Gitter](https://gitter.im/scalaz/scalaz)
- IRC: [Freenode](https://webchat.freenode.net/?channels=%23scalaz&uio=d4)
- Mailing List: [Google Groups](https://groups.google.com/group/scalaz)
