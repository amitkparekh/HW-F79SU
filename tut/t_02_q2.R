#+ setup, include=FALSE
library(magrittr)
library(glue)
library(knitr)
library(ezknitr)
opts_chunk$set(dpi = 200, fig.retina = 2, fig.width = 10)
library(survival)

#+ data
drug_a <- c(3, 6, 8, 10)
drug_a_censor <- c(1, 0, 0, 1)
drug_b <- c(5, 7, 9, 12)
drug_b_censor <- c(1, 0, 0, 1)

time <- c(drug_a, drug_b)
censor <- c(drug_a_censor, drug_b_censor)


#+ plot-part-likelihood
partL <- function(b) {
	y <- exp(b)
	b - 2*log(1+y) - log(3+4*y)
}
plotPartLogL <- function(from, to, formula, ylab) {
	x <- seq(from, to, len = 1000)
	plot(x, formula(x), type = "l", col = "red", lwd = 2, xlab = "beta", ylab = "partial-logL")
	grid()
}
plotPartLogL(-2, 2, partL)
plotPartLogL(-.7925, -.7915, partL)
abline(h = partL(-.7920), col="blue")

#+ cox-model
type <- factor(c(rep(1, 4), rep(2, 4)))
cox.fit <- coxph(Surv(time, censor) ~ type)
summary(cox.fit)