check.longitudinal.cor <- function(which, dat = DAT$training)
{
  dat$Day <- 1:nrow(dat)
  dat$i <- 1 + floor(dat$Day/(0.2*(nrow(dat) + 1)))
  dat2 <- dat
  dat2$i <- 0
  dat <- rbind(dat, dat2)
  which <- match.arg(which, colnames(dat))
  
  p <- ggplot(dat, aes_string(x = which, y = "y")) +
    facet_wrap(~ i) +
    geom_point() +
    geom_smooth(method = "lm", se = FALSE) +
    ylab(attr(dat, "yvar")) + xlab(which)
  p
}