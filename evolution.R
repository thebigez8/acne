
model <- function(a)
{
  glm(y ~ ., family = "poisson", data = as.data.frame(a))
}

get.model.specs <- function(a, print.glm = FALSE)
{
  if(!is.acne(a)){stop("Argument 'a' is not of class 'acne'.")}
  if(!is.logical(print.glm) || length(print.glm) != 1){stop("print.lm must be single logical value.")}
  
  a.glm <- model(a)
  if(print.glm) print(summary(a.glm))
  aic <- AIC(a.glm)
  bic <- BIC(a.glm)
  eic <- sqrt(aic^2 + bic^2)
  # f <- summary(a.lm)$fstatistic
  # p.val <- if(is.null(f)) NA_real_ else unname(pf(f[1], f[2], f[3], lower.tail = FALSE))
  # r.sq <- summary(a.lm)$r.squared
  # adj.r.sq <- summary(a.lm)$adj.r.squared
  
  return(c(AIC = aic, BIC = bic, EIC = eic
           # , one.minus.r.sq = 1 - r.sq,
           # one.minus.adj.r.sq = 1 - adj.r.sq, log10.p.val = log10(p.val)
           ))
}

mate <- function(e1, e2, halve = TRUE)
{
  if(!is.acne(e1) || !is.acne(e2)){stop("'e1' and 'e2' must both be of class 'acne'.")}
  if(attr(e1, 'yvar') != attr(e2, 'yvar')){stop("'e1' and 'e2' have incompatible yvars.")}
  if(!is.logical(halve) || length(halve) != 1){stop("'halve' must be a single logical value.")}
  
  shared <- intersect(e1, e2)
  unionn <- union(e1, e2)
  differ <- unionn[unionn %nin% shared]
  
  if(halve)
  {
    # this will still work if length(differ) == 0
    output <- c(shared, differ[sample(c(TRUE, FALSE), length(differ), replace = TRUE)])
  } else
  {
    output <- unionn
  }
  
  output <- clean(output, yvar = attr(e1, "yvar"))
  
  return(output)
}

mut <- function(a, p = 0.75)
{
  if(!is.acne(a)){stop("'a' must be of class 'acne'.")}
  if(!is.numeric(p) || length(p) != 1 || p <= 0 || p >= 1){stop("'p' is not valid parameter.")}
  output <- a
  
  action <- sample(c("none", "del", "add"), 1, prob = c(1 - p, 0.25*p, 0.75*p))
  if(action == "del")
  {
    output <- output[sample(-(1:length(output)), 1)]
  } else if(action == "add")
  {
    output <- mate(output, acne()[1], halve = FALSE)
  }
  
  output <- clean(output, yvar = attr(a, "yvar"))
  return(output)
}
