library(survival)
library(timereg)

timereg::trace
ds <- data(package = "survival")
str(ds)
ds$results

data(melanoma) #from timereg
melanoma

#log rank test 
survival::survdiff(formula = Surv(days/365, status == 1) ~ factor(sex), data = melanoma)


# Kaplan Meier ------------------------------------------------------------

library(timereg)
data(melanoma)
melanoma$dead <- melanoma$status!=2
## Kaplan-Meier curve
kmfit <- survival::survfit(Surv(days, dead) ~ survival::strata(sex), data=melanoma) 
str(kmfit)
plot(kmfit, conf.int=FALSE)
plot.