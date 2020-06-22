![Build Status](https://travis-ci.org/jonnylaw/ComposableStateSpaceModels.svg?branch=master)

# Composable POMP Models

This is a Scala library for inference and filtering of state space models with a continuous time latent state. The library relies on Akka Streams.

For library documentation, see the [guide](https://jonnylaw.github.io/ComposableStateSpaceModels) or the paper,

Law, J., Wilkinson, D.J. Composable models for online Bayesian analysis of streaming data. Stat Comput 28, 1119–1137 (2018). https://doi.org/10.1007/s11222-017-9783-1

## Quick Start

### Using a build.sbt File

This release version of this library is built for Scala 2.12.1, add the following dependency to your `build.sbt` file in the project root.

```scala
libraryDependencies += "com.github.jonnylaw" %% "composablemodels" % "0.6.5"
```

### Interactive Installation

If you want to play around with the library interactively, then run sbt and enter the following commands:

```bash
$ sbt
> set scalaVersion := "2.12.1"
> set libraryDependencies += "com.github.jonnylaw" %% "composablemodels" % "0.6.5"
> console
```
