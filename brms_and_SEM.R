#title: "Bayesian Modeling"
#author: "Hannah Valdiviejas"
#date: "6/9/2020"

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

#####################################################################################################

#The data
setwd("~/Desktop")
ASTR_EX <- read_excel("ASTR2016_Extended.xlsx")
summary(ASTR_EX)
attach(ASTR_EX)
head(ASTR_EX)

#This was supposed to be the trace plot
#Do we need to create a sequence for the trace plots -- Markov Chain 
install.packages("coda")
library(rstan)
library(coda)
install.packages("mcmcr")
fit2 <- lm_imp(CRS_GRADE_ID ~ AVG_Total_MC  + UR + AVG_WC + ACT_COMP_GROUP + PostCount,data = ASTR_EX,n.chains = 2)
install.packages("JointAI")
library(JointAI)
traceplot(mcmc(fit))

#First posterior predicitve checks
#Density Plots
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
summary(meta_ur)

#Poisson
ACT_ur <- bf(ACT_COMP_GROUP ~ UR, family="poisson")
summary(ACT_ur)

#The cumulative option for families should give your proportional odds models 
grade <- bf(CRS_GRADE_ID ~ AVG_Total_MC + UR + ACT_COMP_GROUP, family="cumulative") 
summary(grade)
WAIC(grade)

#Putting it all together but starting simple, maybe
#The ratio of Estimate divided by Error = Z score
#Z-score = -0.27
#model_simple <-brm (AVG_Total_MC ~ UR,       
                     #family="poisson",
                     #data= ASTR_EX,
                     #chains=1,
                     #cores=4)
#summary(model_simple)

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

library(brms)

alltogether <- brm(CRS_GRADE_ID ~ AVG_Total_MC  + UR + AVG_WC + ACT_COMP_GROUP + PostCount, 
                  data = ASTR_EX, 
                  family = "Poisson",
                  chains= 4,
                  cores= 1)
summary(alltogether)
plot(alltogether)


#WAIC very low, loo better
loo(alltogether)
WAIC(alltogether)

?prior

#Calculating R^2
#What does it all mean!?
install.packages("rstanarm")
library(rstanarm)
fit <- stan_glm(
  CRS_GRADE_ID ~ AVG_Total_MC  + UR + AVG_WC + ACT_COMP_GROUP + PostCount,
  data = ASTR_EX,
  QR = TRUE,
  chains = 2,
  refresh = 0
)

rsq <- bayes_R2(fit)
print(median(rsq))

hist(rsq)

loo_rsq <- loo_R2(fit)
print(median(loo_rsq))
