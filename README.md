# Composable POMP Models

This is a Scala library for partially observed Markov processes (POMP) with a continuous time hidden state. Composable POMP models can be used to model a variety of univariate time series data, allowing interpolation and forecasting. The library is built upon Akka Streams, allowing for scalable online inference.

## Quick Start

### Using a build.sbt File

This library is built using Scala 2.11, add the following dependency to your `build.sbt` file in the project root.

```scala
libraryDependencies += "com.github.jonnylaw" %% "composablemodels" % "0.1"
```

### Interactive Installation

If you want to play around with the library interactively, then run sbt and enter the following commands:

```bash
$ sbt
> set scalaVersion := "2.11.5"
> set libraryDependencies += "com.github.jonnylaw" %% "composablemodels" % "0.1"
> console
```

## Learning about the Library

* For usage examples see the [examples](src/main/scala/examples) directory
* For documentation, see the [documentation](https://jonnylaw.github.io/ComposableStateSpaceModels)
* For in depth discussion see the [paper](https://arxiv.org/abs/1609.00635) on Arxiv
