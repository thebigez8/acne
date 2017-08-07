library(tidyverse)
library(magrittr)

VARS <- function()
{ 
  c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun", "hjf", # "dow", "mm", "swim", "fv",
    "sugar", "pb", "dairy", "alcohol", "exer.min", "exer.shower", "run", "hrs.stress",
    "sleep", "sleep.half", "nap", "sleep.total", "meds", "shave", "shave.days",
    "wd", "mn", "ej", "home", "work", "sick", "mean.hum", "mean.tmp", "shirt",
    "ptm", "aab", "dating", "sun", paste0(ANYVARS(), ".any"))
}

ANYVARS <- function()
{
  c("sugar", "pb", "alcohol", "exer.min", "run", "hrs.stress", "nap", "meds",
    "mn", "ej","home", "work", "sick", "ptm", "aab", "hjf")
}

YVARS <- function()
{
  c("acne", "acne_p", "bacne", "bacne_p", "acne_comb")
}

`%nin%` <- function(x, table) match(x, table, nomatch = 0L) == 0L

clean <- function(a, ...) UseMethod("clean")

source("lag_functions.R")
source("get_rawdat.R")
source("filter_vars.R")
source("acne.R")
source("evolution.R")
source("simulate.R")
source("predict.R")
source("check.long.cor.R")
source("rpart.R")






