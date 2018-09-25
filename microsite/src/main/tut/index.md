---
layout: home
position: 1
section: home
title: "Home"
---

**Welcome to Scalaz 8**

Scalaz is a Scala library for functional programming.

It provides purely functional data structures to complement those from the Scala standard library. It defines a set of foundational type classes (e.g. `Functor`, `Monad`) and corresponding instances for a large number of data structures.

# Getting started

Include Scalaz in your project by adding the following to your `build.sbt`:

```tut:evaluated
if (scalaz.zio.BuildInfo.isSnapshot) scala.Console.println(s"""resolvers += Resolver.sonatypeRepo("snapshots")""")
scala.Console.println(s"""libraryDependencies += "org.scalaz" %% "scalaz-base" % "${scalaz.BuildInfo.version}"""")
```

# Community

- Community: [Mobilize](https://scalaz.mobilize.io/)
- Gitter: [Gitter](https://gitter.im/scalaz/scalaz)
- IRC: [Freenode](https://webchat.freenode.net/?channels=%23scalaz&uio=d4)
- Mailing List: [Google Groups](https://groups.google.com/group/scalaz)
- Voice Chat: [Discord](https://discord.gg/eYZhcW)
