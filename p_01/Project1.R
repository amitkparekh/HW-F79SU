#
#     R-code to generate data for Project 1.  This code MUST be
#     placed at the start of your own R-script.  You must edit
#     the argument to the set.seed( ) function to fit your own
#     registration number
#
set.seed(4445)
#
#     Generate sample A times with censorng
#
timeA <- sort(round(rexp(8, 0.5), digits = 3))
censorA <- c(1, 0, 0, 0, 1, 0, 0, 1)
cbind(timeA, censorA)
#
#     Generate sample B times with censorng
#
timeB <- sort(round(rexp(6, 0.5), digits = 3))
censorB <- c(1, 0, 0, 0, 0, 1)
cbind(timeB, censorB)
#
#     Combine data
#
time <- c(timeA, timeB)
censor <- c(censorA, censorB)
group <- factor(c(rep(1,8), rep(2,6)))
cbind(time, censor, group)

####################################################
# Please insert your R code after this line
####################################################

#### External packages and libraries
packages = c("survival")
packageCheck <- lapply(packages, FUN = function(name) {
	if (!require(name, character.only = TRUE)) {
		install.packages(name, dependencies = TRUE)
		library(name, character.only = TRUE)
	}
})
# search()


######
## Question 1

# Surv(time, censor)
surv.KM <- survfit( Surv(time, censor) ~ 1, conf.type = "plain" )
# summary(surv.KM)
# plot(surv.KM,
#   ylab = "Survival function with 95% CI",
#   xlab = "Time")
# abline(v = 1.2, col="red")

surv.KM.est <- stepfun(surv.KM$time, c(1, surv.KM$surv))
# surv.KM.est(0:0.5:10)
# surv.KM.est(1.2)
# 1-surv.KM.est(2)

######
## Question 3 

cox.fit <- coxph(Surv(time, censor) ~ group)
#summary(cox.fit)
#phi.hat <- exp(cox.fit$coefficients)

######
# Question 4

partLogL <- function(p) {
	2*log(p) - log(8+6*p) - log(8+5*p) - log(4+p) - log(1+p) 
}

plotPartLogL <- function(lower, upper) {
	x <- seq(lower, upper, length = 1000)
	plot(x, partLogL(x),
		main = "Partial loglikelihood",
		type = "l",
		xlab = "Phi",
		ylab = "loglikelihood",
		col = "red",
		lwd = 2
	)
	grid()
}

score <- function(p) {
	2/p - 6/(8+6*p) - 5/(8+5*p) - 1/(4+p) - 1/(1+p)
}

plotScore <- function(lower, upper) {
	x <- seq(lower, upper, length = 1000)
	plot(x, score(x),
		type = "l",
		col = "red",
		lwd = 2,
		main = "Score function",
		xlab = "Phi",
		ylab = "Score"
	)
	grid(col = "lightblue")
}

plotScore(1.68, 1.70)
abline(v = exp(cox.fit$coefficients), col = "blue")
abline(h = 0, col="blue")

###### 
# Question 5

# 5a - answered below

# 5b

lrtTest <- function(coxTest, sig) {
	if (summary(coxTest)$logtest["pvalue"] < sig) {
		1 # reject
	} else {
		0 # do not reject
	}
}

####################################################
# Make sure you define the variables
# AnswerQ1 = 
# AnswerQ2 = 
# AnswerQ3 = 
# AnswerQ5a = 
# AnswerQ5b = should be 0 (don't reject) or 1 (reject)
# Make sure the plot for Q4 is generated automatically 
####################################################

AnswerQ1 <- surv.KM.est(1.2)
AnswerQ2 <- 1- surv.KM.est(2)
AnswerQ3 <- exp( cox.fit$coefficients )
AnswerQ5a <- summary(cox.fit)$logtest["pvalue"]
AnswerQ5b <- lrtTest(cox.fit, 0.05)
plotPartLogL(1.67, 1.72)
abline(v = exp(cox.fit$coefficients), col = "blue")