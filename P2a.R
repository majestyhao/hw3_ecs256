library(ggplot2)

# approximates the distribution of a given nonnegative RV by a mixture pf Erlangs 
# and approximation of a constant randoms variable (single Erlang is used)
# r is the desired value for the Erlang parameter r
# nmix is the desired # of compoents in the mixture; for use only if const is NULL
# qftn is the quantile function for the given distribution
# const is the value of a constant that we wish to approximate
erlangmix <- function(r, nmix = NULL, qftn = NULL, const = NULL) {
  # approximates the distribution of a given nonnegative RV by a mixture pf Erlangs
  # any distribution can be approximated by constants (constants/interpoints connected together)
  # any constants can be approximated by Erlang (as EX@Erlang)
  interpoints <- (1: nmix) / (nmix + 1)
  iMean <- qftn(interpoints) # EX/mean for ith interpoint
  lamb <- r/iMean # EX = r/lamb
  return(list(r = r, lamb = lamb))
}

# plot the mixture of Erlang distribution
plotErmix <- function (ermixobj, plotint) {
  N <- 10000
  lambIndex <- sample(1: ermixobj$r, N, replace = T)
  X <- rep(0, N)
  for (i in 1:N) { 
    lamb_i <- ermixobj$lamb[lambIndex[i]]
    # generate a Erlang RV, since Erlang is the sum of exp
    X[i] <- sum(rexp(n = ermixobj$r, rate = lamb_i)) 
    # print(X[i])
  }
  base <- qplot(X, geom = "density", xlim = plotint)
  # print(X)
  return(base)
}

test <- function() {
  qf <- function(q) sqrt(q)
  ermixobj <- erlangmix(500, 250, qf)
  print(ermixobj)
  plotint <- c(0, 1)
  base <- plotErmix(ermixobj, plotint)
  base + stat_function(fun = df, colour = "red", linetype = "dashed")
}

test()
