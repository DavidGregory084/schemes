## Schemes

[![Build Status](https://api.travis-ci.org/DavidGregory084/schemes.svg)](https://travis-ci.org/DavidGregory084/schemes)
[![Coverage Status](http://codecov.io/github/DavidGregory084/schemes/coverage.svg?branch=master)](http://codecov.io/github/DavidGregory084/schemes?branch=master)
[![License](https://img.shields.io/github/license/DavidGregory084/schemes.svg)](https://opensource.org/licenses/Apache-2.0)
[![Latest Version](https://img.shields.io/maven-central/v/io.github.davidgregory084/schemes-core_2.12.svg)](http://search.maven.org/#search%7Cga%7C1%7Cg%3A%22io.github.davidgregory084%22%20AND%20a%3A%22schemes-core_2.12%22)

### Overview

This repository contains a (partial, WIP) port of the excellent [recursion schemes micro-library](https://github.com/japgolly/microlibs-scala/tree/master/recursion) originally authored by David Barri ([@japgolly](https://github.com/japgolly)) to ensure that it works nicely with the [Typelevel Cats](https://github.com/typelevel/cats/) functional programming library for Scala.

The original library was introduced by a [blog post](https://japgolly.blogspot.co.uk/2017/12/practical-awesome-recursion-ch-02.html) full of practical examples motivating the use of recursion schemes. I highly recommend you check out the whole series.

### Getting Started

Add the following to your `build.sbt`:

```scala
// For cats 1.0.0-RC1
libraryDependencies += "io.github.davidgregory084" %% "schemes-core" % "0.1.0"
```

### Examples

I'm currently working on better documentation and examples. In the meantime I recommend you check out:

  * @japgolly's [blog series](https://japgolly.blogspot.co.uk/2017/12/practical-awesome-recursion-ch-02.html)

  * [@zainab-ali](https://github.com/zainab-ali)'s amazing [talk from Scala eXchange 2017](https://skillsmatter.com/skillscasts/10959-topiary-and-the-art-of-origami)

  * this [pull request](https://github.com/DavidGregory084/titanic/pull/1/files) which ports the code from Zainab's talk from [matryoshka](https://github.com/slamdata/matryoshka) to `schemes`.

### Conduct

Contributors are expected to follow the [Typelevel Code of Conduct](http://typelevel.org/conduct.html) while participating on Github and any other venues associated with the project. 

### Acknowledgements

This library would not exist at all were it not for the original work of [@japgolly](https://github.com/japgolly).

Thanks are also due to Tomas Mikula ([@TomasMikula](https://github.com/TomasMikula)) for the [unboxed representation](https://github.com/scalaz/scalaz/pull/1472) of `Fix` which both libraries use.

### License

All code in this repository is licensed under the Apache License, Version 2.0.  See [LICENSE](./LICENSE).

