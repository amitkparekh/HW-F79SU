#
#     R-code to generate data for Project 3.  This code MUST be
#     placed at the start of your own R-script.  You must edit
#     the argument to set.seed( ) function to fit your own
#     registration number
#
set.seed(4445)
#
#     Read in CMI data and load R-funcions
#
#     NB: The files CMI_read.r, the CMI data files and R-function file
#         Test_GoF.r must be in the appropriate directory
#
source("CMI_read.r")
source("Test_GoF.r")
YEAR <- sample(Year, 1); YEAR
DTH <- Dth[Age > 39, Year == YEAR]
EXP <- Exp[Age > 39, Year == YEAR]
AGE <- Age[Age > 39]

# Install packages
packages = c("survival")
packageCheck <- lapply(packages, FUN = function(name) {
	if (!require(name, character.only = TRUE)) {
		install.packages(name, dependencies = TRUE)
		library(name, character.only = TRUE)
	}
})
rm(packages, packageCheck)

var_col <- c("indianred1", "seagreen3", "deepskyblue2")

#################################################
## Question 1
## Modelling the data

Gomp.glm <- glm(DTH ~ offset(log(EXP)) + AGE, family = poisson)

Gomp.glm.intercept <- Gomp.glm$coefficients[1]
Gomp.glm.slope <- Gomp.glm$coefficients[2]

#################################################
## Question 2
## Plotting the graph

plot(
	AGE,
	log(DTH / EXP),
	xlab = "Age",
	ylab = "log(mortality)",
	pch = 16,
	col = var_col[3],
	main = paste("Gompertz law: England & Wales, ages 40 to 90, year", YEAR),
	cex.main = 0.9
)
lines(AGE, Gomp.glm$coef[1] + Gomp.glm$coef[2]*AGE, lwd=2, col=var_col[1])

#################################################
## Question 3
## Getting the residual values

resVal <- residuals(Gomp.glm, type = "pearson")

#################################################
## Question 4
## Change of sign test

csTest <- Change.Sign(resVal); csTest

if (csTest[[3]] <= 0.05) {
	csTestConf <- 1 # reject
} else {
	csTestConf <- 0 # accept
}

#################################################
## Question 5
## Standard Area Test

saTest <- Standard.Area(resVal, 10); saTest

if (saTest[[6]] <= 0.05) {
	saTestConf <- 1 # reject
} else {
	saTestConf <- 0 # accept
}


#################################################
## Answer variables

#AnswerQ1a <- Gomp.glm.intercept
#AnswerQ1b <- Gomp.glm.slope
#AnswerQ3 <- resVal
#AnswerQ4a <- csTest$Change
#AnswerQ4b <- csTestConf
#AnswerQ5a <- saTest$Chis2
#AnswerQ5b <- saTestConf

# When seed == (4445), YEAR == 1973.
AnswerQ1a = -10.7913825 
AnswerQ1b = 0.1061008
AnswerQ3 = residuals(Gomp.glm, type= "pearson")
AnswerQ4a = 16
AnswerQ4b = 1
AnswerQ5a = 38.60784
AnswerQ5b = 1