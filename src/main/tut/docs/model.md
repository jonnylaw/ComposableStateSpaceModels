---
layout: docs
title: Models
---

A composable state space model is a latent state model used to model time series data. Each simple state space model can be composed together with other models to build complex models hierarchically. The state space of the model evolves in continuous time according to a [stochastic differential equation](https://en.wikipedia.org/wiki/Stochastic_differential_equation) which are essentially differential equations with a random element.

## A Single Gaussian Model

The latent-state drives a transformation of the mean of the observation distribution, for instance a Gaussian state space model with a Brownian motion latent state would be specified in full as:

$$ \begin{align*}
y(t_i) | \eta(t_i) &\sim \mathcal{N}(\eta(t_i), V) \\
\eta(t_i) &= x(t_i) \\
X(t_i) | (X(t_{i-1}) = x(t_{i-1})) &\sim \mathcal{N}(x(t_{i-1}), \sigma \textrm{d}t)
\end{align*} $$

The mean of the Gaussian observation distribution at time \\(t_i\\) is \\(\eta(t_i\\), this is the same as the latent state since the mean of the Gaussian distribution can vary over the entire real line. Hence the linking function is the identity function. The model can be expressed as a DAG:

In order to build this model in Scala using the composable model frame work, we first need to specify the state space. A univariate Brownian motion:

```tut:book:silent
import com.github.jonnylaw.model._
import breeze.numerics.log

val m0 = 0.0
val c0 = log(1.0)
val sigma = log(0.02)
val sdeParam = SdeParameter.brownianParameter(m0, c0, sigma)
val sde = Sde.brownianMotion(1)
```

Note that strictly positive parameters are specified on the log-scale. The parameters of Brownian motion include the parameters of the initial state \\(x(t_0) \sim \mathcal{N}(m_0, C_0) \\) and the value of the diffusion coefficient, \\(\sigma\\). The function `Sde.brownianMotion` accepts an integer which determines how many dimensions the SDE has, in this case it is univariate Brownian motion.

Now, we must specify the observation variance \\(V\\), which is required by the Gaussian Model. The variance is an option parameter, and hence represented by a Scala `Option`, and is specified on the log scale.

```tut:book:silent
val gaussianModel = Model.linearModel(sde)
val gaussianParams = Parameters.leafParameter(Some(log(1.0)), sdeParam)
```

The `Model` object has many pre-defined models, the Gaussian model is called `linearModel` and all models are a function from `Sde => Parameters => Model`. Hence `model` is a function from `Parameters => Model`, once the parameters are supplied to the model we can access the functions belonging to the model:

```scala
trait Model {
  /**
    * The observation model, a function from eta to a distribution over the observations
    * realisations can be produced from the observation model by calling draw
    */
  def observation: Gamma => Rand[Observation]
  /**
    * The linking-function, transforms the state space into the parameter space of the 
    * observation distribution using a possibly non-linear transformation
    */
  def link(x: Gamma): Eta = x

  /**
    * The Linear, deterministic transformation function. f is used to add seasonal factors or
    * other time depending linear transformations
    */ 
  def f(s: State, t: Time): Gamma
  /**
    * An exact or approximate solution to a diffusion process, used to advance the latent state.
    * This function returns a distribution over the next state and can be simulated from
    */
  def sde: Sde
  /**
    * The data likelihood, given the linearly transformed latent state, gamma, and an observation
    * the log-likelihood can be calculated for use in inference algorithms
    */
  def dataLikelihood: (Gamma, Observation) => LogLikelihood
}
```

## Composing Models

To create a seasonal Gaussian model with a multivariate Brownian motion latent state, we can compose the model above with a seasonal model. Firstly, let's specify a seasonal model with a period of 3 and 5 harmonics, this requires a latent state with 10-dimensions:

```tut:book:silent
val sdeMulti = Sde.brownianMotion(10)
val seasonalModel = Model.seasonalModel(3, 5, sdeMulti)
val seasonalParams = Parameters.leafParameter(None, sdeParam)
```

Note that the parameters for each dimension of the Brownian motion are taken to be identical in this case. In order to compose this model, we need to compose both the parameters and the model in a specific order:

```tut:silent:book
import cats.implicits._

val composedParams = gaussianParams |+| seasonalParams
val composedModel = gaussianModel |+| seasonalModel
```

The parameters of the model, and the model, with the desired observation model must be on the left. The parameters form a binary tree, each single model is a `LeafParameter` which combines into a `BranchParameter`. The state of the model also forms a binary tree. The function `|+|` is a closed binary composition function defined on the space of composable models. The composable models form a semigroup.