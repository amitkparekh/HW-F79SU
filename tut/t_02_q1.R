#+ setup, include=FALSE
library(magrittr)
library(glue)
library(knitr)
library(ezknitr)
opts_chunk$set(dpi = 200, fig.retina = 2, fig.width = 10)
library(survival)

#+ import-data
time <- c(2, 5, 6, 8, 9, 10, 12, 14, 16, 20)
censor <- c(1, 1, 0, 1, 1, 0, 1, 0, 0, 1) # 0 = censored, 1 = interest

#+ KM-plot
Surv(time, censor)
KM.surv <- survfit(Surv(time, censor) ~ 1, conf.type = "plain")
summary(KM.surv)
plot(KM.surv,
	main = "Time to recurrence of symptoms",
	xlab = "Time (in days)",
	ylab = "Survival function with 95% CI"
)