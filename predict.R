predict.acnesim <- function(s, which = attr(s, "which"), ...)
{
  predict(s$best[[which]], ...)
}

predict.acne <- function(a, newdata = DAT$testing, ..., conf.int = FALSE, n = 1000L)
{
  if(!is.data.frame(newdata) || nrow(newdata) > 50) stop("newdata is too big or isn't a data.frame")
  if(!all(a %in% colnames(newdata))) stop("Can't find the right variables in 'newdata'")
  # if("y" %in% colnames(newdata)) newdata$y <- NULL
  
  yvar <- attr(a, "yvar")
  
  get.pred <- function(t = FALSE)
  {
    y.out <- numeric(nrow(newdata))
    for(i in 1:nrow(newdata))
    {
      pr <- predict(model(a), newdata = newdata[i, , drop = FALSE], se.fit = TRUE, type = "response")
      p <- if(t) pr$fit + rt(1, df = pr$df)*sqrt(pr$se.fit^2 + pr$residual.scale^2) else pr$fit
      y.out[i] <- p
      for(j in 1:maxlag())
      {
        if(i + j <= nrow(newdata)) newdata[i + j, paste0(yvar, ".lag", j)] <- p
      }
    }
    y.out
  }

  y.pred <- get.pred()  
  cors <- cor(newdata$y, y.pred)
  
  
  if(!conf.int) return(structure(data.frame(y = newdata$y, y.pred = y.pred, lower = y.pred, upper = y.pred),
                                 class = c("acnepredict", "data.frame"), yvar = yvar, cor = cors, a = a))

  sims <- t(replicate(n, get.pred(t = TRUE), simplify = TRUE))
  cis <- apply(sims, 2, quantile, probs = c(0.025, 0.975))
  return(structure(data.frame(y = newdata$y, y.pred = y.pred, y.pred.sim = colMeans(sims), lower = cis[1, ], upper = cis[2, ]),
                   class = c("acnepredict", "data.frame"), yvar = yvar, cor = cors, a = a))
}

plot.acnepredict <- function(x, ...)
{
  x$Day <- 1:nrow(x)
  p <- ggplot(x, aes(x = Day)) +
    geom_ribbon(aes(ymin = lower, ymax = upper, linetype = NA, color = "Predicted"), alpha = 0.2) +
    geom_line(aes(y = y.pred, color = "Predicted")) +
    geom_line(aes(y = y, color = "Actual")) +
    scale_color_manual(values = c("red", "black")) +
    ylab(attr(x, "yvar"))
  print(p)
}

summary.acnepredict <- function(object, ...)
{
  print(attr(object, "a"))
  cat("Predictions done on ", nrow(object), " new obs.\n", sep = "")
  cat("Cor = ", attr(object, "cor"), "\n", sep = "")
  cat("R-squared = ", attr(object, "cor")^2, "\n", sep = "")
}