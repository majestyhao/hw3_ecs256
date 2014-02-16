# covert hazard function into probability density function
htof <- function(hftn, t, lower) {
  len <- length(t)
  pdf <- rep(0, len)
  for (i in 1:len) {
    pdf[i] <- hftn(t[i]) * exp(-1 * integrate(hftn, lower, t[i])$val.)
  }
  return(pdf)
}