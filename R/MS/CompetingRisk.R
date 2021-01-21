library(tidyverse)
# MS 1.1 ------------------------------------------------------------------
library(survival)

head(pbc)

ggplot(pbc %>% slice(1:10), aes(y = factor(id))) + 
  geom_linerange(aes(xmin = age, xmax = age + time/365.24)) + 
  geom_point(aes(x = age + time/365.24, col = factor(status)))

