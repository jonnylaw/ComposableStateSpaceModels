## install and load packages
packages = c("dplyr","ggplot2","gridExtra", "ggmcmc", "coda", "tidyr", "plyr")
newPackages = packages[!(packages %in% as.character(installed.packages()[,"Package"]))]
if(length(newPackages)) install.packages(newPackages)
lapply(packages, require, character.only = T)

theme_set(theme_minimal())

## Simulate the simplest linear model
## m0 = 0.1 c0 = 1.0 V = 1.0, mu = -0.2, sigma = 1.0

system("sbt \"run-main examples.SimLinear\"")

sims = read.csv("./LinearModelSims.csv", header = F,
                col.names = c("time", "y", "eta", "gamma", "x"))

sims[,-(3:4)] %>%
    gather(key = "key", value = "value", -time) %>%
    ggplot(aes(x = time, y = value, colour = key)) + geom_line()

## Recover the parameters using Breeze MCMC
system("sbt \"run-main examples.BreezeMCMC\"")

iters = read.csv("./LinearModelBreeze.csv", header = F,
                 col.names = c("m0", "c0", "V", "mu", "sigma"))

plot(mcmc(iters))

## Check the streaming MCMC
## This uses the same model, data and metropolis hastings step
## However it uses Source.unfold to produce a Markov chain and doesn't work
## I'm not sure why :(
akkastreamIters = read.csv("./LinearModelStreamOut.csv", header = F,
                           col.names = c("m0", "c0", "V", "mu", "sigma", "accepted"))

plot(mcmc(akkastreamIters[,-1]))
