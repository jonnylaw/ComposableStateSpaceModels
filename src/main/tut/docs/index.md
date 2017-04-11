---
layout: docs
title: Getting Started
---

# Composable POMP Models

This is a Scala library for inference and filtering of state space models with a continuous time latent state. The library relies on Akka Streams.

## Quick Start

### Using a build.sbt File

This library is built for Scala 2.11.8, add the following dependency to your `build.sbt` file in the project root.

```scala
libraryDependencies += "com.github.jonnylaw" %% "composablemodels" % "0.6.0"
```

### Interactive Installation

If you want to play around with the library interactively, then run sbt and enter the following commands:

```bash
$ sbt
> set scalaVersion := "2.11.8"
> set libraryDependencies += "com.github.jonnylaw" %% "composablemodels" % "0.6.0"
> console
```
