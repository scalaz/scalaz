---
layout: docs
title: stdlib
---

# {{ page.title }}

The `testz-stdlib` module provides a basic testz suite type and
assertions using nothing more than the standard library, `testz-core`,
and `testz-runner`.

The suite type is called `PureSuite`. It's built to be used with
`testz-runner`, i.e. it extends the `testz.runner.Suite` class.
The only method to override on `PureSuite` is `test`, the signature
of which is:

```scala
def test[T](test: Test[Function0, T]): T
```
