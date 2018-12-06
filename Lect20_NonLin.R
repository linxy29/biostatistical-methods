
################################################################
#             Biostatistical Methods I: Lecture 20             #
#                   Non-linear Models                          #
#           Author: Cody Chiuzan; Date: Nov 27, 2018           #
################################################################


rm(list = ls())

# Load libraries
install.packages(c('dplyr','tidyr','broom','ggplot2','splines'))

library(dplyr)
library(tidyr)
library(broom)
library(ggplot2)
library(splines)


# Generate non-linear data: order 4 polynomial
set.seed(1)

data.nonlin = data.frame(x = runif(100, 0, 1)) %>%
  mutate(y = -30*(x-.5)^2 + 100*(x-.5)^4 + rnorm(100, 0, .3))

# Data plot
ggplot(data.nonlin, aes(y=y, x=x)) + geom_point() + theme_bw()

# Polynomial regression
data.nonlin = mutate(data.nonlin, 
                     x.pow2 = x^2, x.pow3 = x^3, x.pow4 = x^4)

# Fit data with quartic polynomial, plot
quartfit = lm(y ~ x + x.pow2 + x.pow3 + x.pow4, data = data.nonlin)
summary(quartfit)


# Another way to fit the model
quartfit_alt = lm(y ~ x + I(x^2)+I(x^3)+I(x^4), data = data.nonlin)
summary(quartfit_alt)

# What if we use function poly()?
# This creates orthogonal (not correlated polynomials), ie., the columns of the X matrix are orthogonal. 
# This does not change the fitted values but allows you to see whether a certain order in the polynomial significantly improves the regression over the lower orders.
# For example X^4 captures only the quartic part not captured by the cubic term.

quartfit_ortho = lm(y ~poly(x, 4), data = data.nonlin)
summary(quartfit_ortho)

# Use option raw=T to get original resuls.
# quartfit_ortho1 = lm(y ~poly(x, 4, raw=T), data = data.nonlin)
# summary(quartfit_ortho1)

mutate(data.nonlin, fitted = fitted(quartfit)) %>%
  ggplot(., aes(y=y, x=x)) + geom_point() + 
  geom_line(aes(y = fitted), color = "red") + theme_bw()

# Fit a quadratic polynomial, plot
quadfit = lm(y ~ x + x.pow2, data = data.nonlin)
mutate(data.nonlin, fitted = fitted(quadfit)) %>%
  ggplot(., aes(y=y, x=x)) + geom_point() + 
  geom_line(aes(y = fitted), color = "red") + theme_bw()


# Piecewise linear regression: 1 knot at Nurse=250
# Read data 'Hospitals.csv'
data_hosp<-read.csv("Hospital.csv")
names(data_hosp)

# Create an indicator variable for values less and greater than 250 (knot)
data_hosp$NURSEstar <- ifelse(data_hosp$NURSE<250,0,1)

# MLR Model 1: Length of stay (LOS) vs number of BEDS and INFRISK
reg_spline<-lm(LOS ~ NURSE + NURSEstar, data=data_hosp)
summary(reg_spline)

mutate(data_hosp, fitted = fitted(reg_spline)) %>%
  ggplot(., aes(y=LOS, x=NURSE)) + geom_point() + 
  geom_line(aes(y = fitted), color = "red") + theme_bw()
  
# Add a smoother
# Default is weighted least squares
# f is smoothing parameter (proportion), larger values give more smoothness
smoother<-lowess(data_hosp$NURSE, data_hosp$LOS, f=0.2)
plot(data_hosp$NURSE, data_hosp$LOS, pch=16)
lines(smoother, lwd=2, col=2)


# Piecewise linear regression: 3 knots at 0.2, 0.5, 0.8
data.nonlin = mutate(data.nonlin, 
                     spline_2 = (x - .2) * (x >= .2),
                     spline_5 = (x - .5) * (x >= .5),
                     spline_8 = (x - .8) * (x >= .8))

piecewise.fit = lm(y ~ x + spline_2 + spline_5 + spline_8, data = data.nonlin)
summary(piecewise.fit)

mutate(data.nonlin, fitted = fitted(piecewise.fit)) %>%
  ggplot(., aes(y=y, x=x)) + geom_point() + 
  geom_line(aes(y = fitted), color = "red") + theme_bw()


# Example of B-splines (cubic)

# Generate data
set.seed(1)
data.nonlin = data.frame(x = runif(100, 0, 1)) %>%
  mutate(y = -30*(x-.5)^2 + 100*(x-.5)^4 + rnorm(100, 0, .3))

# Generate basis matrix for cubic splines: 5 df
# The basis will include two boundary knots: min and max and
# 4 (5-1) internal knots placed at 20th, 40th, 60th, and 80th quantiles of x

data.nonlin = data.nonlin %>% bind_cols(., data.frame(ns(.[['x']], df = 5))) %>%
  rename(sp.1 = X1, sp.2 = X2, sp.3 = X3, sp.4 = X4, sp.5 = X5)

bspline.fit = lm(y ~ sp.1 + sp.2 + sp.3 + sp.4 + sp.5, data = data.nonlin)

mutate(data.nonlin, fitted = fitted(bspline.fit)) %>%
  ggplot(., aes(y=y, x=x)) + geom_point() + 
  geom_line(aes(y = fitted), color = "red") + theme_bw()







