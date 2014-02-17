library("ggplot2")
# discrete scienario
# observe the simluated process at various points,
# which will produce simulated residual lifetimes
# w: vector of wait times (lifetime/interval/lasting mean time)
# nevents: the number of renewals to simulate (observation points)
# perwidth: the sampling period width (interval betwn obser pts)
# return residual life values at the observation points
simrenewal <- function(w, nevents, perwidth) {
  len = length(w) # amount of bulbs/buses
  obstotTime <- nevents * perwidth # total observation time
  N <- 100 # sample amount
  wtotTime <- -1 # total lasting time
  # draw max(N) samples from w
  while (wtotTime < obstotTime) {
    N <- 2 * N
    # sample from w
    index <- sample(1:len, N, replace = TRUE) # sample N ints from 1:len
    wtotTime <- sum(w[index])
  }
  # construct timeline to connect w's samples
  timeline <- rep(0, N)
  timeline[1] <- w[index][1]
  for (i in 2:N) {
    timeline[i] = timeline[i - 1] + w[index][i]
  }
  # obervation time vector
  obsTime <- perwidth * (1:nevents)
  # residual time
  resTime <- rep(0, nevents)
  # start iteration at timeline
  iw <- 1 # interator of w
  for (iobs in 1:nevents) {
    # move to timepoint until timeline is larger than obs time point 
    # for the situation that obs time's interval is bigger than w's
    while (timeline[iw] < obsTime[iobs]) {
      iw = iw + 1
    }
    # residual time = lifetime - age/observation time point
    resTime[iobs] <- timeline[iw] - obsTime[iobs]
  }
  return(resTime)    
}

testU <- function() {
  N <- 1000
  # RV derived from interval's distribution: a uniform one
  w <- runif(N, min = 0, max = 1) 
  nevents <- 10000
  perwidth <- 1
  residualTime <- simrenewal(w, nevents, perwidth)
  # Compute an empirical cumulative distribution function based on RV
  Fcdf <- ecdf(w) 
  # pdf of residual time computed by 11.21
  fd_U <- function(t) {(1 - Fcdf(t))/0.5} # 0.5 is EX: 1/2 * (max - min)
  base <- qplot(residualTime, geom = "density")
  # comparison
  base + stat_function(fun = fd_U, colour = "red", linetype = "dashed")
}

testU()
