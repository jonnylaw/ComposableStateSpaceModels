## install and load packages
packages = c("dplyr","ggplot2","gridExtra", "ggmcmc", "coda", "tidyr", "plyr")
newPackages = packages[!(packages %in% as.character(installed.packages()[,"Package"]))]
if(length(newPackages)) install.packages(newPackages)
lapply(packages, require, character.only = T)

theme_set(theme_minimal())

######################
# Simulate Student T #
######################

data = read.csv("tdistSims.csv", header = F, 
                col.names = c("Time", "Observation", "Eta", "Gamma", "State"))
data %>%
  gather(key = "key", value = "value", -Time) %>%
  ggplot(aes(x = Time, y = value, colour = key)) + geom_line()

######################
# Seasonal Student T #
######################

system("sbt \"run-main examples.SeasStudentT\"")
seas = read.csv("seastdistSims.csv", header = F,
                col.names = c("Time", "Observation", "Eta", "Gamma", sapply(1:3, function(i) paste0("State", i))))

seas %>%
  gather(key = "key", value = "value", -Time) %>%
  ggplot(aes(x = Time, y = value, colour = key)) + geom_line() + facet_wrap(~key)

################################
# Filtering seasonal Student T #
################################

system
