---
layout: docs
title: runner
---

# {{ page.title }}

The testz suite runner takes a list of suites to an `IO[Void, Unit]`, printing
test failures and successes to standard out.

The suites are passed as `() => Suite` in order to avoid keeping test data in
memory.

```scala
object Runner {
  def apply(suites: List[() => Suite]): IO[Void, Unit]
}
```

The test suite type expected by the runner is:

```scala
trait Suite {
  def run: IO[Void, SuiteResult]
}
```

Any value of type `Suite` can be run by the runner.

## Internals

The runner avoids interleaving test output because `Suite` doesn't print to
standard out itself; that's the runner's job.  It prints output given by the
`Suite`.
