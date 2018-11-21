#
#     R-code to generate data for Project 2.  This code MUST be placed
#     at the start of your own R-script.  You must edit the argument to
#     the set.seed( ) function. This argument must be the LAST FOUR
#     DIGITS OF YOUR MATRICULATION NUMBER.
#
set.seed(4445)
#
#     Generate values of Start, End and Death
#
Start <- c(rep(0, 7), sort(round(runif(3, 0, 0.45), digits = 2)))
End <- c(rep(1, 7), round(runif(3, 0.55, 1), digits = 2))
Death <- c(0,0,0,1,0,1,0,0,0,1)
# cbind(Start, End, Death, End-Start)

####################################################
# Please insert your R code after this line
####################################################

packages = c("survival", "data.table", "magrittr")
packageCheck <- lapply(packages, FUN = function(name) {
	if (!require(name, character.only = TRUE)) {
		install.packages(name, dependencies = TRUE)
		library(name, character.only = TRUE)
	}
})
rm(packages, packageCheck)

data <- data.table(
	   start = Start,
	   end = End,
	   death = Death
	   )
# rm(Start, End, Death)


logMu <- function(data, Mu) {

	Start <- data$start
	End <- data$end
	Death <- data$death

	points <- length(Start)

	living <- numeric(points)
	dead <- numeric(points)

	for (i in 1:points) {
		if (Death[i] == 0) {
			living[i] <- End[i] - Start[i]
		}

		if (Death[i] == 1) {
			dead[i] <- End[i] - Start[i]
		}
	}

	living <- sum(living)
	dead <- dead[dead != 0]

	mu_living <- numeric(length(Mu))
	mu_living <- -1 * living * Mu


	mu_dead <- numeric(length(dead))

	output <- numeric(length(Mu))

	for (i in 1:length(Mu)) {
		for (j in 1:length(dead)) {
			mu_dead[j] <- log(1 - exp(-dead[j] * Mu[i]))
		}
		output[i] <- sum(mu_dead) + mu_living[i]
	}

	output
}


plotLogMu <- function(lower, upper) {
	Mu <- seq(lower, upper, length = 1000)
	plot(Mu, logMu(data, Mu),
		main = "Partial loglikelihood for Mu",
		type = "l",
		xlab = "Mu",
		ylab = "LogLikelihood (Mu)",
		col = "red",
		lwd = 2
	)
	grid()
}

# plotLogMu(0.421, 0.426)

muHat <- 0.423

# let phi = exp(-2* Mu / 100)

logPhi <- function(Phi) {
	299 * log(Phi) + 2*log(1-Phi^50) + log(1-Phi^19)
}

plotLogPhi <- function(lower, upper) {
	Phi <- seq(lower, upper, length = 1000)
	plot(Phi, logPhi(Phi),
		main = "Partial loglikelihood of Phi",
		type = "l",
		xlab = "Phi",
		ylab = "Loglikelihood (Phi)",
		col = "red",
		lwd = 2
	)
	grid()
}

# plotLogPhi(0.9914,0.9917)
phiHat <- 0.9916

# exp(-2 * muHat / 100)

scorePhi <- function(Phi) {
	299*Phi - (2*50*Phi^49 / (1-Phi^50)) - (19*Phi^18 / (1-Phi^19))
}

plotScore <- function(lower, upper) {
	x <- seq(lower, upper, length = 1000)
	plot(x, scorePhi(x),
		type = "l",
		col = "red",
		lwd = 2,
		main = "Score function",
		xlab = "Phi",
		ylab = "Score"
	)
	grid(col = "lightblue")
}

# plotScore(0.42, 0.43)

scoreMu <- function(Mu) {
	Phi <- exp(Mu * -0.02)
	scorePhi(Phi) * Phi * (-0.02)
}

h <- 0.001
slopeMu <- (scoreMu(muHat + h/2) - scoreMu(muHat-h/2)) / h

SE <- sqrt(-1 / slopeMu)


## Q4 - using 2 state model

deathTime <- numeric(10)
waitingTime <- numeric(10)
deathNum <- 0

for (i in 1:10) {
	if (data$death[i] == 1) {
		deathTime[i] <- ((data$end[i] - data$start[i]) / 2 + data$start[i])
		waitingTime[i] <- deathTime[i] - data$start[i]
		deathNum <- deathNum + 1
	} else {
		deathTime[i] <- data$end[i]
		waitingTime[i] <- data$end[i] - data$start[i]
	}
}
data <- cbind(data, deathTime, waitingTime)


# find mle of mu = mu tilde
muTilde <- deathNum / sum(data$waitingTime)

# estimate SE of mu tilde
muSE <- sqrt( muTilde / sum(data$waitingTime) )



####################################################
# Make sure you define the variables
# AnswerQ2 = 
# AnswerQ3 = 
# AnswerQ4a = 
# AnswerQ4b = 
# Make sure the plot for Q1 is generated automatically 
# and no other plot is generated
####################################################


AnswerQ2 = muHat
AnswerQ3 = SE
AnswerQ4a = muTilde
AnswerQ4b = muSE
plotLogMu(0.421, 0.426)

c(AnswerQ2, AnswerQ3, AnswerQ4a, AnswerQ4b)