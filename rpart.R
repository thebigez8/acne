
tree <- function()
{
  rp <- rpart::rpart(y ~ ., data = DAT$training)
  class(rp) <- c("acnetree", class(rp))
  rp
}

plot.acnetree <- function(at)
{
  rpart:::plot.rpart(at, main = attr(DAT$training, "yvar"))
  rpart:::text.rpart(at)
  
  plot(predict(at), DAT$training$y, main = attr(DAT$training, "yvar"))
  invisible(TRUE)
}

print.acnetree <- function(at)
{
  vars <- unique(as.character(at$frame$var))
  vars <- vars[vars != "<leaf>"]
  
  cat("An acne tree with ", length(vars), " variables:\n", sep = "")
  print(vars)
  cat("\nTraining correlation: ", round(cor(predict(at), DAT$training$y), 5), sep = "")
  invisible(at)
}

