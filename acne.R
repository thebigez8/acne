MAX.N <- function() 30

is.acne <- function(x) inherits(x, "acne")

unique.acne <- function(x, ...)
{
  x2 <- unique(as.character(x))
  clean(x2, yvar = attr(x, "yvar"))
}

clean.character <- function(a, ...)
{
  clean.acne(a, ...)
}

clean.acne <- function(a, yvar = NULL, dat = DAT$training, ..., max.n = MAX.N())
{
  a <- unique(a)
  a <- a[a %in% colnames(dat)[-1]]
  if(length(a) > max.n) a <- a[sample(length(a), max.n, replace = FALSE)]
  if(!is.acne(a) && is.character(a) && !is.null(yvar))
  {
    a <- structure(a, class = "acne", yvar = yvar)
  } else if(!is.acne(a)) stop("'a' isn't an acne")
  a
}

acne <- function(dat = DAT$training, x.n = 10, max.n = MAX.N())
{
  yvar <- match.arg(attr(dat, "yvar"), YVARS())
  n <- min(rpois(1, as.integer(x.n)) + 1, ncol(dat) - 1, max.n) #added 1 to avoid 0's
  a <- sample(colnames(dat)[-1], size = n, replace = FALSE)
  clean(a, yvar = yvar)
}

as.data.frame.acne <- function(a, dat = DAT$training, ...)
{
  na.omit(dat[, c("y", a), drop = FALSE])
}

`[.acne` <- function(x, i)
{
  yvar <- attr(x, 'yvar')
  x <- NextMethod()
  clean(x, yvar = yvar)
}

print.acne <- function(x, ...)
{
  cat("\nAcne object with ", length(x), " variables:\n", sep = "")
  print(as.character(x))
  cat("\n\n")
  invisible(x)
}

