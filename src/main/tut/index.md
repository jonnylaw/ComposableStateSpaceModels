This is the documentation for a [Scala library for continuous time partially observed Markov processes](https://git.io/statespace) (POMP). Partially observed Markov processes can be used to model time series data, allowing interpolation and forecasting. 

## Introduction to Partially Observed Markov Process Models

Partially observed Markov processes are a type of [State Space Model](https://en.wikipedia.org/wiki/State-space_representation). This means the models feature unobserved, or latent, variables. The unobserved system state is governed by a [diffusion process](https://en.wikipedia.org/wiki/Diffusion_process), these are continuous time Markov processes meaning that future values of the state space, are independent from all previous values given the current state, x(t).

The distribution, p, represents the Markov transition kernel of the state space. The distribution pi, represents the observation distribution, parameterised by the state space. The function f is a linear deterministic function, which can be used to add cyclic seasonal components to the state space. The function g is the linking-function from a [generalised linear model](https://en.wikipedia.org/wiki/Generalized_linear_model), which transforms the state space into the parameter space of the observation model. Define $\gamma(t) = F^T_t \textbf{x}(t)$ and $\eta(t) = g(\gamma(t))$.

## Simulating the State Space

An example of a diffusion process is the [Ornstein-Uhlenbeck process](https://en.wikipedia.org/wiki/Ornstein%E2%80%93Uhlenbeck_process), which can be simulated by specifying the parameters of the process, `theta`, the mean of the process, `alpha` how quickly the process reverts to the mean and `sigma` the noise of the process. Then we must specify an initial state, which is done by drawing from a Gaussian distribution, since the exact solution to the OU process is a Gaussian distribution. Then we pass a `stepFunction` containing the exact solution to the OU process, relying on only the previous value of the realisation (because the process is  Markovian) and the time difference between realisations.

```tut
import com.github.jonnylaw.model._ // import package contents
import breeze.stats.distributions.Gaussian
import breeze.linalg.{DenseVector, DenseMatrix, diag}
import cats.implicits._ // this is for the typeclass syntax and the show typeclass

// specify the parameters of the Ornstein Uhlenbeck process, m0 and c0 are the parameters of the initial state, x0 ~ N(m0, c0)
// theta is the mean, alpha is how quickly the process reverts to the mean and sigma controls the noise of the process
val ouParameter = SdeParameter.ornsteinParameter(
  m0 = DenseVector(0.0),
  c0 = DenseMatrix((1.0)),
  theta = DenseVector(1.0), 
  alpha = DenseVector(0.3), 
  sigma = DenseVector(0.2))

val ouProcess = Sde.ornsteinUhlenbeck

val ouSims = ouProcess(ouParameter).
  simStream[Nothing](0.0, 0.1)

ouSims.
  take(5).
  map(_.show).
  toList
```

```tut:silent
import breeze.plot._

val f = Figure()
val p = f.subplot(0) 
val data = ouSims.
  take(200).
  zipWithIndex.
  map { case (x, t) => (t * 0.1, x.flatten.head(0)) }.
  toList

p += plot(data.map(_._1), data.map(_._2))
p.title = "Ornstein Uhlenbeck Process"
f.saveas("Figures/ouProcess.png")
```

Notice, the state space can be multidimensional, and as such is represented by a `Vector`. A single state is represented by a LeafState, this will become clear when considering composition of models. The figure below shows a representation of the Ornstein-Uhlenbeck process with `theta = 6.0, alpha = 0.05, sigma = 1.0`.

## Simulating a Single Model

The observations of a POMP can be from any parameterised distribution. The observation distribution depends on the latent variables and sometimes on additional parameters not in the system state, such as a scaling parameter representing measurement noise. A simple non-gaussian observation model, used for representing count data is the Poisson distribution, parameterised by it's rate \lambda(t). If we consider the rate, \lambda(t) to vary stochastically, then we can represent it using a POMP mode. Firstly select a representation of the state space, we will use the Ornstein-Uhlenbeck process from the previous example, then specify the parameters and the times the process should be observed. There is a `LeafParameter` class which combines the initial State, optional scale parameter and the state space parameters for a single model.


```tut
val poissonParams = Parameters.leafParameter(None, ouParameter)
val poissonMod = Model.poissonModel(ouProcess)

val poissonSims = SimulatedData(poissonMod(poissonParams)).
  observations[Nothing]

poissonSims.
  take(5).
  map((d: Data) => d.show).
  toList
```

```tut:silent
// insert plot of the single poisson model
val f = Figure()
val p = f.subplot(3, 1, 0) // three rows, one column, select first row
val p1 = f.subplot(3, 1, 1)
val p2 = f.subplot(3, 1, 2)
val data = poissonSims.take(200).toList
val times = data.map(_.t)

p += plot(times, data.map(_.observation), name = "Simulated Observations")
p1 += plot(times, data.map(_.eta.head), name = "The Rate, Lamda(t)")
p2 += plot(times, data.map(x => {
   val mod = poissonMod(poissonParams)
   mod.link(mod.f(x.sdeState, x.t)).head
   }), name = "The Ornstein Uhlenbeck Latent State")

p.title = "Simulated Observations"
p1.title = "The Rate, Lamda(t)"
p2.title = "The Ornstein Uhlenbeck Latent State"

f.saveas("Figures/PoissonModel.png")
```

The figure shows the state space, which varies along the whole real line and the transformed state space and Eta, which is strictly positive. The linking function, g, is the log-link.

## Composing Multiple Models

If we wish to consider more complex process, for instance a Poisson model with a seasonally varying rate, then we have to add deterministic values to the state before applying the observation distribution. The function, f, is a linear deterministic function which can be used to add seasonal factors to the system state. 

Unparameterised models are represented as `Reader[Parameters, Model]`. A semigroup is defined on unparameterised models, using the library [cats](https://github.com/typelevel/cats), a semigroup is a set with an associative, closed binary operator. The binary operator is used to compose models, the function is associative, but not commutative, since the composition of two models selects the leftmost Model's observation and linking functions. The code snippet below shows how to construct a seasonal Poisson model, the observation distribution is Poisson, but the rate of an event occuring follows a daily (period T = 24) cycle if we assume count observations are made once every hour. We have to `import cats.implicits._` to use the semigroup notation `|+|`.

```tut
val seasonalParams = Parameters.leafParameter(
  None, SdeParameter.brownianParameter(
    DenseVector.fill(2)(0.0),
    diag(DenseVector.fill(2)(1.0)), 
    DenseVector.fill(2)(0.1), 
    diag(DenseVector.fill(2)(0.4))))

val composedParams = poissonParams |+| seasonalParams
val composedMod = poissonMod |+| Model.seasonalModel(24, 1, Sde.brownianMotion)

val composedSims = SimulatedData(composedMod(composedParams)).
  observations[Nothing].
  take(500).
  map((d: Data) => d.show).
  toList
```

```scala:silent
// insert a plot of the composed model
// extract sections of the state

val driftState = composedSims.
  map(x => {
    val mod = drift(poissonParam)
    x.sdeState match {
      case Branch(s, _) => mod.link(mod.f(s, x.t)).head
    }
  })

val dailySeasonality = composedSims.
  map(x => {
    val mod = daily(seasonalParamDaily)
    x.sdeState match {
      case Branch(_, s) => mod.link(mod.f(s, x.t)).head
    }
  })

val f = Figure()
val p = f.subplot(4, 1, 0) // four rows, one column, select first row
val p1 = f.subplot(4, 1, 1)
val p2 = f.subplot(4, 1, 2)
val times = composedSims.map(_.t)
p += plot(times, composedSims.map(_.observation), name = "Simulated Observations")
p1 += plot(times, driftState, name = "Drift State")
p2 += plot(times, dailySeasonality, name = "Daily State")

p1.title = "Drift State"
p2.title = "Daily State"

f.saveas("Figures/ComposedModel.png")
```
## Statistical Inference: The Particle Filter

If we have a fully specified model, ie the posterior distributions of the parameters given the data so far are available to us, then we can use a bootstrap particle filter (see [Sequential Monte Carlo Methods in Practice](https://www.springer.com/us/book/9780387951461) for a detailed review of the bootstrap particle filter) to determine the hidden state space of the observations. 

Consider the simulated Poisson model, the bootstrap particle filter can be applied to the simulated data using a draw from the parameter posterior distribution and the inferred state space can be compared to the previously simulated state space. The data can be read in from a CSV or database, or simulated again. However, since these are stochastic models we can't compare different realisations of the same model. 

Use 100 observations from the composed model we simulated from earlier:

```tut
val data = composedSims.take(100)
```

Define the particle filter, using 100 particles.

```tut
val t0 = 0.0
val filter = ParticleFilter.filter(t0, 100)
```

Run the particle filter over the observed data and calculate credible intervals of the state

```tut
val filteredStream = data.
  through(filter(composedMod(composedParams))).
  map(ParticleFilter.getIntervals(composedMod(composedParams)))
```

The figure below shows the actual simulated state, plotted next to the estimate state and 99% [credible intervals](https://en.wikipedia.org/wiki/Credible_interval).


```tut:silent
val filtered: Vector[PfOut] = filteredStream.drop(1).runLog.unsafeRun
val times = filtered.map(_.time)
val actualEta = data.map(_.eta.head).toList
val predEta = filtered.map(_.eta)
val etaLower = filtered.map(_.etaIntervals.lower)
val etaUpper = filtered.map(_.etaIntervals.upper)

val f = Figure()
val p = f.subplot(0) // four rows, one column, select first row
p += plot(times, actualEta, name = "Simulated Value of Eta")
p += plot(times, predEta, name = "Mean of Estimated Filtering Distribution for Eta")
p += plot(times, etaUpper, '.', name = "Upper")
p += plot(times, etaLower, '.', name = "Lower")

p.legend = true
p.title = "Mean of Estimated Filtering Distribution vs Simulated State"
f.saveas("Figures/FilteredPoisson.png")
```

## Inference for the Joint State and Parameter Posterior Distribution

Say we have observed a time depending process in the real world, and don't have the parameters available for the model. We wish to carry out inference for the state space and the parameters of the model simultaneously. This framework implements the Particle Marginal Metropolis Hastings (PMMH) Algorithm (see [Doucet et al. 2010](http://www.stats.ox.ac.uk/~doucet/andrieu_doucet_holenstein_PMCMC.pdf)). The likelihood of the state space and parameters given the observations can be determined using a particle filter, then a standard Metropolis-Hastings update step is used to create a Markov Chain representing the full join posterior of the model given the observed real-world process.

Now we can implement the PMMH algorithm for the simulated Bernoulli observations, and determine if the algorithm is able to recover the parameters.

```tut
val data = poissonSims.
  take(500).
  toList

// create a function from Parameters => LogLikelihood by composing the model (Parameters => Model) 
// with the filter (Model => LogLikelihood)
def mll = ParticleFilter.likelihood(ParticleFilter.multinomialResampling, data, 500) compose poissonMod

// specify a prior on the parameters
def prior: Parameters => LogLikelihood = p => 0.0

// build the PMMH algorithm using mll estimate (via particle filter), the
// initial parameters, the proposal distribution for new parameters and the prior distribution for the parameters
val iters = ParticleMetropolis(mll.run, poissonParams, Parameters.perturb(0.05), prior).
  iters[fs2.Task].
  take(10000)
```

`iters` is an [fs2](https://github.com/functional-streams-for-scala/fs2) (Functional Streams for Scala) stream which can be written to a file:

```tut
// iters.
//   intersperse("\n").
//   through(text.utf8Encode).
//   through(io.file.writeAll(Paths.get("data/PoissonModelParameters.csv"))).
//   run
```

If we want to get a `Vector` of parameters, then run:

```tut
// iters.runLog.unsafeRun
```

Note, this is an expensive operation.


```tut
// insert a plot of the parameter estimates here
// density, running mean, 
```

Note that the algorithm has been initialised at the same parameter values we used to simulate the model, this kind of prior information is not typically known for real world processes, unless similar processes have been extensively studied.