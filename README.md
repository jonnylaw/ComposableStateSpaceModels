# Composable POMP Models

This is a Scala library for partially observed Markov processes (POMP) with a continuous time latent state. Composable POMP models can be used to model a variety of univariate time series data. As of version 0.2, this library is built using [fs2 streams](https://github.com/functional-streams-for-scala/fs2).

For library documentation, see the [guide](https://jonnylaw.github.io/ComposableStateSpaceModels), this is currently being updated for version 0.2.1 of the library.

## Quick Start

### Using a build.sbt File

This library is built for Scala 2.11.8, add the following dependency to your `build.sbt` file in the project root.

```scala
libraryDependencies += "com.github.jonnylaw" %% "composablemodels" % "0.2.1"
```

### Interactive Installation

If you want to play around with the library interactively, then run sbt and enter the following commands:

```bash
$ sbt
> set scalaVersion := "2.11.8"
> set libraryDependencies += "com.github.jonnylaw" %% "composablemodels" % "0.2.1"
> console
```
