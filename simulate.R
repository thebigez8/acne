WHICH <- function() c("AIC", "BIC", "EIC"
                      # , "one.minus.r.sq", "one.minus.adj.r.sq", "log10.p.val"
                      )

sim <- function(yvar = YVARS(), nsim = 2, npersim = 10, ..., which = "BIC", FILTER = c("split.and.filter", "split.and.filter2"))
{
  input <- list(...)
  yvar <- match.arg(yvar, several.ok = FALSE)
  if(!is.numeric(nsim) || length(nsim) != 1){stop("'nsim' not numeric of length 1.")}
  if(!is.numeric(npersim) || length(npersim) != 1){stop("'npersim' not numeric of length 1")}
  if(nsim < 2 || npersim < 10){stop("'nsim' or 'npersim' not big enough")}
  which <- match.arg(which, WHICH())
  FILTER <- match.fun(match.arg(FILTER, several.ok = FALSE))
  DAT <<- FILTER(yvar)
  
  acnes <- replicate(npersim, acne(), simplify = FALSE)
  acnes[[1]] <- clean(c("acne_p.lag1", "acne_p.lag2"), yvar = yvar)
  specs <- t(vapply(acnes, get.model.specs, numeric(length(WHICH()))))
  specs.median <- matrix(NA_real_, nrow = nsim, ncol = length(WHICH()))
  colnames(specs.median) <- WHICH()
  
  specs.worst <- specs.q3 <- specs.q1 <- specs.best <- specs.median

  specs.median[1, ] <- apply(specs, 2, median, na.rm = TRUE)
  specs.best[1, ] <- apply(specs, 2, min, na.rm = TRUE)
  specs.q1[1, ] <- apply(specs, 2, quantile, na.rm = TRUE, probs = 0.25)
  specs.q3[1, ] <- apply(specs, 2, quantile, na.rm = TRUE, probs = 0.75)
  specs.worst[1, ] <- apply(specs, 2, max, na.rm = TRUE)
  
  best <- list()
  for(metric in colnames(specs.median))
  {
    best[[metric]] <- acnes[[which.min(specs[, metric])]]
    best[[paste0(metric, ".val")]] <- min(specs[, metric], na.rm = TRUE)
  }
  
  
  for(i_ in 2:nsim)
  {
    if(i_ %% 5 == 0) message("Simulation ", i_)
    
    # stop early criterion
    if(i_ > 8 && length(unique(specs.best[(i_ - 1):(i_ - 8), which])) == 1){message(paste0("Early convergence: i=", i_ - 1)); break}
    
    old.acnes <- acnes
    
    rnks <- rank(specs[, which], ties.method = "random")
    acnes[[1]] <- old.acnes[[which(rnks == 1)]]
    
    mate.these <- which(rnks %in% 1:floor(npersim/4))
    for(j_ in 2:floor(npersim*0.9))
    {
      tmp <- sample(mate.these, 2, replace = FALSE)
      acnes[[j_]] <- mut(mate(old.acnes[[tmp[1]]], old.acnes[[tmp[2]]]))
    }
    for(j_ in (floor(npersim*0.9) + 1):npersim)
    {
      acnes[[j_]] <- acne()
    }
    specs <- t(vapply(acnes, get.model.specs, numeric(length(WHICH()))))
    specs.median[i_, ] <- apply(specs, 2, median, na.rm = TRUE)
    specs.best[i_, ] <- apply(specs, 2, min, na.rm = TRUE)
    specs.q1[i_, ] <- apply(specs, 2, quantile, na.rm = TRUE, probs = 0.25)
    specs.q3[i_, ] <- apply(specs, 2, quantile, na.rm = TRUE, probs = 0.75)
    specs.worst[i_, ] <- apply(specs, 2, max, na.rm = TRUE)
    
    for(metric in colnames(specs.median))
    {
      if(min(specs[, metric], na.rm = TRUE) < best[[paste0(metric, ".val")]])
      {
        best[[metric]] <- acnes[[which.min(specs[, metric])]]
        best[[paste0(metric, ".val")]] <- min(specs[, metric], na.rm = TRUE)
      }
    }
  }
  
  return(structure(list(best = best, specs.median = specs.median, specs.best = specs.best, specs.q1 = specs.q1, specs.q3 = specs.q3,
                        specs.worst = specs.worst), class = "acnesim", which = which))
}

plot.acnesim <- function(x, ...)
{
  
  df.median <- reshape2::melt(x$specs.median, value.name = "median", na.rm = TRUE)
  df.best <- reshape2::melt(x$specs.best, value.name = "best", na.rm = TRUE)
  df.q1 <- reshape2::melt(x$specs.q1, value.name = "q1", na.rm = TRUE)
  df.q3 <- reshape2::melt(x$specs.q3, value.name = "q3", na.rm = TRUE)
  df.worst <- reshape2::melt(x$specs.worst, value.name = "worst", na.rm = TRUE)
  
  df1 <- merge(df.median, df.best, by = c("Var1", "Var2"), sort = FALSE)
  # df2 <- merge(df1, df.worst, by = c("Var1", "Var2"), sort = FALSE)
  df3 <- merge(df1, df.q1, by = c("Var1", "Var2"), sort = FALSE)
  df4 <- merge(df3, df.q3, by = c("Var1", "Var2"), sort = FALSE)
  
  p <- ggplot(df4, aes(x = Var1)) +
    facet_wrap( ~ Var2, scales = "free_y", nrow = 2) +
    # geom_ribbon(aes(ymax = worst, ymin = best, fill = Var2), alpha = 0.2, color = NA) +
    geom_ribbon(aes(ymax = q3, ymin = q1, fill = Var2), alpha = 0.4, color = NA) +
    geom_line(aes(y = median, color = Var2)) +
    geom_line(aes(y = best), color = "black") +
    theme(legend.position = 'none') +
    ylab("Value") +
    ggtitle("Median things by round")
  print(p)
}

summary.acnesim <- function(object, which = attr(object, "which"), ...)
{
  which <- match.arg(which, WHICH())
  a <- object$best[[which]]
  a.val <- object$best[[paste0(which, ".val")]]
  cat("\nThe best result found for ", which, " (", round(a.val, 3), ") is an", sep = "")
  print(a)
  # cat("\nWith step(), the following variables are dropped:\n")
  # print(setdiff(a, names(coef(my.lm <- step(model(a), trace = 0)))))
  cat("Poisson regression model:\n")
  print(coef(summary(my.lm <- model(a))))
  
  cat("\nTraining correlation: ", round(cor(my.lm$fitted.values, as.data.frame(a)$y), 5), sep = "")
  
  invisible(my.lm)
}