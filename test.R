
library(ggplot2)
library(mixtools)
base <- ggplot(faithful, aes(x = waiting))
base <- base + geom_density()
base


mixout <- normalmixEM(faithful$waiting,k=2)
df <- function(x) (mixout$lambda[1])*dnorm(x, mean = mixout$mu[1], sd = mixout$sigma[1]) + (mixout$lambda[2])*dnorm(x, mean = mixout$mu[2], sd = mixout$sigma[2])
base <- base + stat_function(fun = df, colour = "red",linetype="dashed")
base
