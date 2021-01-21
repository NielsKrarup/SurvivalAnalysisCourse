#-----------------------------------------------------------
# Survival Analysis 2019
# Exercises week 5
#-----------------------------------------------------------

rm(list=ls())

library(timereg)
library(ggplot2)

# ---------------------------------------------------------
# Exercise D
# Purpose: Understand relation between variables vf, chf and survival time in data TRACE

# Data: See Martinussen & Scheike (2006: p. 451)
# Survival of patients after myocardial infarction (MI) given various risk factors
# Variables:
#   status: Survival status, 0: alive, != 0: dead (from multiple causes).
#   time: survival time in years.
#   age: Age of patient.
#   chf: clinical heart pump failure, 1: present, 0: absent.
#   vf: Ventricular fibrillation, 1: present, 0: absent.
data(TRACE)
TRACE['event'] = as.numeric(TRACE$status != 0) 
head(TRACE)


# ---------------------------------------------------------
# D.1: Time scale

# Two possible time scales for analysis: Follow-up time or age

# Follow-up time is the time-scale of interest since we want to study lifetime after MI
# Age is an important risk factor, which we should correct for in the analysis. 
plot(sort(TRACE$time))
summary(TRACE$time)


# ---------------------------------------------------------
# D.2: Non-parametric estimates
# Kaplan-Meier estimates of survival function
KM.fit = survfit(Surv(time, event) ~ vf + chf, data = TRACE)
class(KM.fit)

plot(KM.fit, lwd = 10)
timereg::kmplot(KM.fit)
methods(plot)
getAnywhere(plot.survfit)
kmplot
plotConfregion
# Notes: There seems to be a difference in the probability of surival over time accros groups.
# Patients without vf and chf seems to have highest probability of survival over time,
# whereas patients with vf and chf seems to have the lowest probability of survival over time.

# Log rank test of equal hazards accross groups (alternative: proportional hazards)
survdiff(Surv(time, event) ~ vf + chf, data = TRACE) # chi-sq = 308   (df = 3), p < 2e-16

# Notes: Observed and "expected" number of events assuming equal hazard doesn't seem to match.
# At a 0.05-significans level we reject the hypothesis of equal hazards.

# Nelson-Aalen estimates of cumulative hazards
NA.fit = survfit(Surv(time, event) ~ vf + chf, data = TRACE)
kmplot(NA.fit, fun = 'cumhaz')
kmplot
# Notes: Steep initial hazard (high risk of death right after MI) when both vf and chf 
# are present, otherwise monotone increasing over time (probably due to aging).

# Comparing Kaplan-Meier and exp(-A(t)) for A(t) being the Nelson-Aalen estimate
nonparam.est = do.call(data.frame, lapply(c(2:11) , function(x) summary(KM.fit)[x]))
nonparam.est['NA.surv'] = exp(-nonparam.est$cumhaz)
nonparam.est['KM.NA.diff'] = nonparam.est$surv - nonparam.est$NA.surv
ggplot(data = nonparam.est, aes(x = time, y = KM.NA.diff, group = strata, color = strata)) + 
  geom_line() + 
  labs(x = 'Time after MI (years)', y = 'Difference', color = 'Strata') +
  ggtitle('Difference between KM and NA estimates of survival time') +
  theme_classic()

# Notes: The two estimators are asymtotic equivalent and consitent, so differences should be
# more pronounced for smaller sample sizes. We see differences between the estimates, 
# especially for the last two groups where the sample sizes are smallest
KM.fit # See sample sizes
