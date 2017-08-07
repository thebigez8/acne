
update.weather <- function()
{
  find.weather <- function(date.)
  {
    tab <- date. %T>% 
      print() %>% 
      strsplit("/", fixed = TRUE) %>% 
      "[["(1) %>% 
      (function(d) paste0("https://www.wunderground.com/history/airport/KRST/",
        d[3], "/", d[1], "/", d[2], "/DailyHistory.html")) %>% 
      xml2::read_html() %>% 
      rvest::html_nodes("table") %>% 
      rvest::html_table() %>% 
      "[["(1)
    
    mean.tmp <- tab[tab[,1] == "Mean Temperature", "Actual"]
    mean.tmp <- as.numeric(substr(mean.tmp, 1, nchar(mean.tmp) - 3))
    mean.hum <- as.numeric(tab[tab[,1] == "Average Humidity", "Actual"])
    return(c(mean.tmp = mean.tmp, mean.hum = mean.hum))
  }
  
  rawdat <- "Acne_data.csv" %>% 
    read_csv(col_names = TRUE, col_types = cols()) %>% 
    filter(!is.na(acne)) %>% 
    mutate(
      weather = map(.$date, find.weather),
      mean.tmp = map_dbl(weather, ~ .x[1]),
      mean.hum = map_dbl(weather, ~ .x[2])
    ) %>% 
    select(-weather) %T>%
    write.table("Acne_data_weather.csv", quote = FALSE, sep = ",", row.names = FALSE, na = "")
  invisible(rawdat)
}

get_rawdat <- function(yvar = YVARS())
{
  yvar <- match.arg(yvar, several.ok = FALSE)
  
  rawdat <- "Acne_data_weather.csv" %>% 
    read_csv(col_names = TRUE, col_types = cols()) %>% 
    filter(!is.na(acne)) %>% 
    filter(as.Date(date, format = "%m/%d/%Y") - as.Date("2015-12-26") >= 0) %>% 
    mutate(
      dow = factor(dow, levels = c("M", "Tu", "W", "Th", "F", "Sa", "Su")),
      Mon = as.numeric(dow == "M"),
      Tue = as.numeric(dow == "Tu"),
      Wed = as.numeric(dow == "W"),
      Thu = as.numeric(dow == "Th"),
      Fri = as.numeric(dow == "F"),
      Sat = as.numeric(dow == "Sa"),
      Sun = as.numeric(dow == "Su"),
      shirt = replace(shirt, is.na(shirt), 1),
      sleep.total = sleep + nap,
      shave = as.numeric(shave.days < 1),
      ej = mn + wd + mm,
      sleep.half = as.numeric(sleep >= 7.5)
    )
  
  for(thing in ANYVARS())
  {
    rawdat[[paste0(thing, ".any")]] <- as.numeric(rawdat[[thing]] > 0)
  }
  
  rawdat <- mutate(rawdat,
    ptm = as.numeric(ptm > 0.5),
    aab = as.numeric(aab > 0.5),
    hjf = as.numeric(hjf > 0.5),
    acne_comb = (acne + acne_p) + 0.5*(bacne + bacne_p)
  )
  
  ##################################################################
  dys <- c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun", "dow")
  combos <- expand.grid(
      var = 0:length(VARS()),
      lag = 0:maxlag(),
      method = 0:1,
      param = 0.1*(1:9)
    ) %>% 
    as.tibble() %>% 
    mutate(
      # don't want any smoothing parameters at 0 or 1
      param = replace(param, param < 0.01, 0.01),
      param = replace(param, param > 0.99, 0.99),
      # set the "raw values" method's parameter to 0
      param = replace(param, method == 0, 0)
    ) %>%
    # get rid of lag 0's for yvar
    filter(!(var == 0 & lag == 0)) %>% 

    # dow and the various days won't need any lag
    filter(!(var %in% which(VARS() %in% dys) & lag > 0)) %>% 

    # Nor do they need smoothing
    filter(!(var %in% which(VARS() %in% dys) & method > 0)) %>% 
    
    # We'll fix y-var to only lag
    filter(!(var == 0 & method > 0)) %>% 
  
    # remove duplicates
    unique()
  
  
  getdat <- function(row, rawdat., yvar)
  {
    method. <- row["method"]
    var. <- if(row["var"] == 0) yvar else VARS()[row["var"]]
    var.. <- rawdat.[[var.]]
    lag. <- row["lag"]
    param. <- row["param"]
    
    if(method. == 0)
    {
      return(lag(var.., lag.))
    } else if(method. == 1)
    {
      return(lag(smooth(var.., alpha = param.), lag.))
    } else if(method. == 2)
    {
      return(lag(spike(var.., alpha = param.), lag.))
    } else if(method. == 3)
    {
      return(lag(window.mean(var..), lag.))
    }
  }
  dat <- apply(as.matrix(combos), 1, getdat, rawdat. = rawdat, yvar = yvar)
  colnames(dat) <- paste0(c(yvar, VARS())[combos$var + 1], ".lag", combos$lag,
                          c("", ".smooth", ".spike", ".window.mean")[combos$method + 1],
                          ifelse(combos$method %in% 1:2, paste0(".param", combos$param), ""))
  
  y <- rawdat[[yvar]]
  y[1:max(6, maxlag())] <- NA # this is to make sure all the models are based on the same number of data points
  
  return(na.omit(bind_cols(y = y, as.data.frame(dat))))
}

