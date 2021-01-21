#misc

date <- as.Date(x = "13dec2020", "%d%b%Y")
date
typeof(date)
str(date)
attributes(date)
class(date)

getAnywhere(print.Date)

300/as.numeric((date - Sys.Date()))
str(date)

library(medflex)
library(dplyr)

iris %>% select(contains("."))
