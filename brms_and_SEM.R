#title: "Bayesian Modeling"
#author: "Hannah Valdiviejas"
#date: "6/9/2020"

#Loading all the packages needed
remove.packages("rstan")
if (file.exists(".RData")) file.remove(".RData")

install.packages("Rcpp", repos = "https://rcppcore.github.io/drat")
Sys.setenv(MAKEFLAGS = "-j4")
install.packages("rstan")
library(rstan)

install.packages("rstan", type = "source")
library(rstan)

rstan_options(auto_write = TRUE)

install.packages("httpuv")
library(httpuv)

install.packages("brms")
library(brms)

sessionInfo()
pkgs <- c("rstan", "RBesT", "OncoBayes2", "rstanarm")
for(p in pkgs) {
  if(!require(p, character.only=TRUE))
    install.packages(p)
  require(p, character.only=TRUE)
}

install.packages("rstan")


library(parallel)

library(ordinal)
library(readxl)

#to calculate p-value?
install.packages("pCalibrate")
library(pCalibrate)

library(brms)

#####################################################################################################
#The actual analysis

#The data
setwd("~/Desktop")
ASTR_EX <- read_excel("ASTR2016_Extended.xlsx")
summary(ASTR_EX)
attach(ASTR_EX)
head(ASTR_EX)

install.packages("coda")
library(rstan)
library(coda)
install.packages("mcmcr")
install.packages("JointAI")
library(JointAI)
traceplot(mcmc(fit))

#Density Plots
#if chains converge nicely then compute w/in and btwn chain variance 
library(gridExtra)
grid.arrange(stan_trace(alltogether$fit, ncol=1),
             stan_dens(alltogether$fit, separate_chains=TRUE,ncol=1),
             ncol=2)


variable.names(ASTR_EX)
hist(AVG_WC)
hist(PostCount)

#Next create your sub-models

#Poisson
#Could not find BRF function
meta_ur <- bf(AVG_Total_MC ~ UR, family="poisson")
#this doesnt run 
fixef(meta_ur)

#Poisson
ACT_ur <- bf(ACT_COMP_GROUP ~ UR, family="poisson")
fixef(ACT_ur)

#The cumulative option for families should give your proportional odds models 
grade <- bf(CRS_GRADE_ID ~ AVG_Total_MC + UR + ACT_COMP_GROUP, family="cumulative") 
fixef(grade)
WAIC(grade)


#The smaller the better
install.packages("LaplacesDemon")
library(LaplacesDemon)
loo(model_simple)
WAIC(model_simple)


#Put it all together

#Making variables integers for the model to be able to porcess 
#ASTR_EX$AVG_Total_MC <- as.integer(ASTR_EX$AVG_Total_MC)
#ASTR_EX$UR <- as.integer(ASTR_EX$UR)

?set_rescor
#default is TRUE but that indicates linear..will not run with false

#warmup = first 2000 iteration
alltogether <- brm(CRS_GRADE_ID ~ AVG_Total_MC  + UR + AVG_WC + ACT_COMP_GROUP + PostCount, 
                  data = ASTR_EX, 
                  warmup = 2000,iter = 5000,
                  family = "cumulative",
                  chains= 4,
                  cores= 1)
summary(alltogether)
plot(alltogether)

################################################
################### Model checks ############### 
################################################

#making the mcmc object -- necessary for the posterior chains 
allpost <- as.mcmc(alltogether)
#tells us whether making it an mcmc object worked 
class(allpost)

#posterior density function 
#traceplot
#All the different colors are the different chains 
#The line in the middle should be horizontal 
traceplot(allpost,smooth=TRUE, type = "l")

#Geweke's Convergence Diagnostic
#based on a test for equality of the means of the first and last part of a Markov chain (by default the first 10% and the last 50%).
#If the samples are drawn from the stationary distribution of the chain, the two means are equal and Geweke's statistic has an 
#asymptotically standard normal distribution
#We generally want numbers to be under 2 
geweke.diag(allpost,frac1 = .1,frac2 = .50)
#we want the stars to be between the dashed lines
#UR is a little off but not so bad
geweke.plot(allpost)

#Stan code to find priors 
all.stan <- stancode(alltogether)
all.stan

#density plot
#Post count somewhat skewed 
densplot(allpost)

#autocorrelations
#we want autocorrelation to be at 0 bc that means tht variables aren't corr.
autocorr.plot(allpost)

#posterior predicitve checking
#argument will allow us to summarize the posterior predictions as a dot 
#(mean) and standard error bars superimposed on a bar plot of the original data
#will not run
pp_check(allpost, type = "overlaid", fun = "bars")
?ppc_bars

#WAIC very low, loo better
loo(alltogether)
WAIC(alltogether)

