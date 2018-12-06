################################################################
#             Biostatistical Methods I: Lecture 15             #
#                 Multiple Linear Regression                   #
#           Author: Cody Chiuzan; Date: Nov 5, 2018            #
################################################################


rm(list = ls())

# Load libraries
install.packages(c('faraway','broom','dplyr'))
library(faraway)
library(broom)
library(dplyr)

# Read data 'Hospitals'
data_hosp<-read.csv("O:\\Teaching\\Methods1\\P8130_Fall18\\Lectures\\Lecture15\\Hospital.csv")
names(data_hosp)

# Simple linear regression: Length of stay (LOS) vs number of BEDS 
reg_hos<-lm(data_hosp$LOS~data_hosp$BEDS)
summary(reg_hos)


# Matrix model
model.matrix(reg_hos)

model.matrix(reg_hos) %>% head


# Multiple linear regression: 
# Var 1: Number of BEDS 
# Var 2: INFRISK (prob. % of getting an infection during hospitalization)

regmult1_hos<-lm(data_hosp$LOS~data_hosp$BEDS + data_hosp$INFRISK)

# Analyze the regression results
summary(regmult1_hos)


# Multiple linear regression: BEDS and INFRISK and NURSE
regmult2_hos<-lm(data_hosp$LOS~data_hosp$BEDS + data_hosp$INFRISK+data_hosp$NURSE)
summary(regmult2_hos)



# Multiple linear regression: BEDS and MEDSCHL (Medical School Affiliation: 1-Yes, 2-No)

# Recode MEDSCHL: Yes:1 and No:0

data_hosp$MS<-ifelse(data_hosp$MEDSCHL==1,1,ifelse(data_hosp$MEDSCHL==2, 0, NA))

# Multiple linear regression: INFRISK and new MS (Medical School Affiliation: 1-Yes, 0-No)
regmult3_hos<-lm(data_hosp$LOS~data_hosp$INFRISK +data_hosp$MS)
summary(regmult3_hos)


# Categorical predictor REGION: multiple levels

# Simple linear regression with predictor REGION (1:NE, 2:NC, 3:S, 4:W)
data_hosp %>% lm(LOS~REGION, data=.) %>% summary
# How does it look?

# Make it a factor
data_hosp %>% lm(LOS~factor(REGION), data=.) %>% summary


# Change the reference category for REGION (from 1 to 3)
# Intercept added
data_hosp %>% mutate(REGION=relevel(factor(REGION),ref=3)) %>% lm(LOS~factor(REGION), data=.) %>% summary


# No intercept model
data_hosp %>% lm(LOS~0+factor(REGION), data=.) %>% summary



# Multiple linear regression: INFRISK, new MS and Region (1:NE, 2:NC, 3:S, 4:W)
regmult4_hos<-lm(data_hosp$LOS~data_hosp$INFRISK +data_hosp$MS+factor(data_hosp$REGION))
summary(regmult4_hos)

# 'General' global test for all predictors
anova(regmult4_hos)


# Multiple linear regression: new MS, Region (1:NE, 2:NC, 3:S, 4:W) and their interaction
regmult5_hos<-lm(LOS~INFRISK*MS, data=data_hosp)
summary(regmult5_hos)

# Vizualize interaction for reg5
gen_risk<-seq(0,8,0.1)
beta<-regmult5_hos$coefficients
yhat0<-beta[1]+beta[2]*gen_risk
yhat1<-beta[1]+beta[3]+(beta[2]+beta[4])*gen_risk

plot(data_hosp$INFRISK, data_hosp$LOS, xlab='Infection Risk (%)', ylab='Length of stay (days)',
     main=('Scatter plot with overlaid fitted lines by medical school affiliation'))
lines(gen_risk, yhat0, lwd=2, col=2)
lines(gen_risk, yhat1, lwd=2, col=3)

