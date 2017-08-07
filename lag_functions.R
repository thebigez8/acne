
maxlag <- function() 6L

lag <- function(x, n, ...)
{
  if(is.na(n)) return(NULL)
  UseMethod("lag")
}

lag.numeric <- function(x, n, ...)
{
  n <- as.integer(n)
  return(c(rep(NA, n), x[1:(length(x) - n)]))
}

lag.factor <- function(x, n, ...)
{
  lev <- levels(x)
  x <- as.character(x)
  n <- as.integer(n)
  tmp <- c(rep(NA, n), x[1:(length(x) - n)])
  return(factor(tmp, levels = lev))
}


Rcpp::sourceCpp("lag_functions.cpp")

smooth <- function(x, alpha, beta = 1 - alpha, lod = 1e-8) smoothC(x, alpha, beta, lod)

spike <- function(x, alpha, lod = 1e-8) smoothC(x, alpha, 1, lod = lod)

window.mean <- function(x, window = min(4L, maxlag()), lod = 1e-8) windowMeanC(x, window, lod)

# source(paste0(getdir(), "bad_lag_functions.R"))
# trash <- runif(1e5)
# microbenchmark(
#   smooth(trash, 0.5),
#   smoothBAD(trash, 0.5),
#   spike(trash, 0.5),
#   spikeBAD(trash, 0.5),
#   window.mean(trash, 4),
#   window.meanBAD(trash, 4)
# )
# rm(trash)


