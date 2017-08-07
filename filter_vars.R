
split.and.filter2 <- function(yvar = YVARS())
{
  yvar <- match.arg(yvar, several.ok = FALSE)
  combos.data <- get_rawdat(yvar)
  
  cutoff <- nrow(combos.data) - 2*(1 + maxlag())
  
  combos.data.training <- combos.data[1:nrow(combos.data) %in% 1:cutoff, ]
  combos.data.testing <- combos.data[1:nrow(combos.data) %nin% 1:cutoff, ]
  
  message("Filtering...")
  
  tmp <- vapply(combos.data.training, function(x, y = combos.data.training$y) coef(summary(glm(y ~ x, family = "poisson")))[2,4], numeric(1))[-1]
  
  df <- data.frame(var = combos[, 1], name = names(tmp), p = tmp, stringsAsFactors = FALSE)
  
  del.these <- unlist(by(df, factor(df$var), function(x) {if(all(x$var == 0)) return(c()); x <- x[order(x$p, decreasing = TRUE), ]; head(x$name, -1)}))

  training <- combos.data.training[, colnames(combos.data.training) %nin% del.these]
  testing <- combos.data.testing[, colnames(combos.data.testing) %nin% del.these]
  attr(training, "yvar") <- yvar
  attr(testing, "yvar") <- yvar
  
  message(paste0("Deleting ", length(del.these), " variables. ",
                 ncol(training), " variables remain in testing set."))
  
  return(list(training = training, testing = testing, del.these = del.these))
}



split.and.filter <- function(yvar = YVARS())
{
  yvar <- match.arg(yvar, several.ok = FALSE)
  combos.data <- get_rawdat(yvar)
  
  cutoff <- nrow(combos.data) - 2*(1 + maxlag())
  
  combos.data.training <- combos.data[1:nrow(combos.data) %in% 1:cutoff, ]
  combos.data.testing <- combos.data[1:nrow(combos.data) %nin% 1:cutoff, ]
  
  message("Filtering...")
  
  no.y.trn <- combos.data.training[, colnames(combos.data.training) %nin% c("y", grep(yvar, colnames(combos.data.training), fixed = TRUE, value = TRUE))]  
  
  #### Correlation analysis ####
  
  tmp <- vapply(no.y.trn, function(x, y = combos.data.training$y) coef(summary(glm(y ~ x, family = "poisson")))[2,4], numeric(1))
  # tmp2 <- p.adjust(tmp, method = 'fdr')
  del1 <- names(tmp)[tmp > 0.3]
  
  no.y.trn2 <- no.y.trn[, colnames(no.y.trn) %nin% del1]

  #### Cluster analysis ####
  
  clust <- hclust(as.dist(1 - abs(cor(no.y.trn2, method = 'spearman'))), method = 'complete')
  #plot(clust)
  grps <- cutree(clust, h = 0.4)
  
  delete.these <- function(num)
  {
    elts <- names(grps[grps == num])
    param <- as.numeric(grepl("param", elts, fixed = TRUE))
    cors <- vapply(elts, function(var) abs(cor(combos.data.training[[var]], combos.data.training$y, use = 'pairwise', method = 'spearman')), numeric(1))
    elts2 <- gsub(".half", "", gsub(".any", "", elts, fixed = TRUE), fixed = TRUE)
    vars <- sapply(elts2, function(x) strsplit(x, ".lag", fixed = TRUE)[[1]][1])
    lags <- sapply(elts2, function(x) substr(strsplit(x, ".lag", fixed = TRUE)[[1]][2], 1, 2)) # we take two elts here in case maxlag() > 9. The period will get erased for maxlag < 10
    lags <- as.numeric(lags)
    df <- data.frame(elts, vars, lags, cors, param,
                     stringsAsFactors = FALSE)
    df <- df[order(df$vars, df$lags, df$param, 1-df$cors), ]
    return(df$elts[duplicated(df$vars)])
  }
  
  del2 <- unlist(lapply(unique(grps), delete.these))
  
  training <- combos.data.training[, colnames(combos.data.training) %nin% c(del1, del2)]
  testing <- combos.data.testing[, colnames(combos.data.testing) %nin% c(del1, del2)]
  attr(training, "yvar") <- yvar
  attr(testing, "yvar") <- yvar
  
  message(paste0("Deleting ", length(del1), " variables for correlation with outcome, and ",
                 length(del2), " variables for too high of self-correlation. ",
                 ncol(training), " variables remain in testing set."))

  return(list(training = training, testing = testing, grps = grps, del1 = del1, del2 = del2))
}
