#+ setup, include=FALSE
library(magrittr)
library(glue)
library(knitr)
library(ezknitr)
opts_chunk$set(dpi = 200, fig.retina = 2, fig.width = 10)
library(survival)

#+ functions
combined <- function(data_a, data_b) {
	factor(
		c(
			rep(1, length(data_a)),
			rep(2, length(data_b))
		)
	)
}

#+ data
steroid <- c(1, 1, 4, 4, 5, 5, 7, 8, 10, 10, 12, rep(16,3))
steroid.censor <- c(1, 0, 1, 0, 1, 0, 1, 1, 1, 0, 0, 0, 0, 0)
control <- c(1, 2, 3, 3, 5, 5, 5, rep(16, 8))
control.censor <- c(0, 1, 1, rep(0, 4 + 8))

time <- c(steroid, control)
censor <- c(steroid.censor, control.censor)
group <- combined(steroid, control)

#+ compute-partlogL



betaMax <- coxph(Surv(time, censor) ~ group)
beta1 <- coxph(Surv(time, censor) ~ group, init = c(1),
	control = coxph.control(iter.max = 0))

betaMax$loglik
beta1$loglik


betas <- seq(-2, 2, len = 1000)



logliks <- sapply(betas, )