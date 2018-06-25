---
layout: home
position: 1
section: home
title: "Home"
---

**Welcome to Scalaz 7.3**

Scalaz is a Scala library for functional programming.

It provides purely functional data structures to complement those from the Scala standard library. It defines a set of foundational type classes (e.g. `Functor`, `Monad`) and corresponding instances for a large number of data structures.

# Getting started

Include Scalaz in your project by adding the following to your `build.sbt`:

```tut:evaluated
println(s"""libraryDependencies += "org.scalaz" %% "scalaz-core" % "${scalaz.BuildInfo.version}"""")
```

# Modules

Scalaz provides additional modules for functionality beyond the basics included in `scalaz-core`

- `scalaz-effect`: Effectful programs

```tut:evaluated
println(s"""libraryDependencies += "org.scalaz" %% "scalaz-effect" % "${scalaz.BuildInfo.version}"""")
```

# Community

- Gitter: [Gitter](https://gitter.im/scalaz/scalaz)
- IRC: [Freenode](https://webchat.freenode.net/?channels=%23scalaz&uio=d4)
- Mailing List: [Google Groups](https://groups.google.com/group/scalaz)
- Voice Chat: [Discord](https://discord.gg/eYZhcW)
