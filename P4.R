library(ggplot2)
library(mixtools)

base <- ggplot(faithful, aes(x = waiting))
base <- base + geom_density()
base

mixout <- normalmixEM(faithful$waiting, k = 2)
# str(mixout) # simple structure print
mdf <- function(x){#function(x) (mixout$lambda[1])*dnorm(x, mean = mixout$mu[1], sd = mixout$sigma[1]) + (mixout$lambda[2])*dnorm(x, mean = mixout$mu[2], sd = mixout$sigma[2])
 (mixout$lambda[1]) * dnorm(x, mean = mixout$mu[1], sd = mixout$sigma[1]) + (mixout$lambda[2]) * dnorm(x, mean = mixout$mu[2], sd = mixout$sigma[2])
}

base <- base + stat_function(fun = mdf, colour = "red", linetype = "dashed")
base
