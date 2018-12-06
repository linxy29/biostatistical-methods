#################################################
#              Lectures 21                      #
#  Penalized Methods: Ridge and Lasso Regresion #
#         Last updated: 12.05.2018              #
#################################################


rm(list = ls())

install.packages(c('dplyr','faraway','glmnet'))
       

library('dplyr')
library('faraway')
library('MASS')                           # for lm.ridge()
library('glmnet')                         # for glmnet()
library('dplyr')
library('reshape2')
library('ggplot2')


# Read R dataset 'state.x77 that contains information on 50 states from 1970s collected by US Census Bureau. 
# The goal is to predict 'life expectancy' using a combination of remaining variables.

dat_state<- data.frame(state.x77)

# Data attributes
names(dat_state)
dim(dat_state)
head(dat_state)

# Start with the full model
mult.fit <- lm(Life.Exp ~ ., data=dat_state)
summary(mult.fit)

# Notice several coeff of small magnitudes.

#####################################################
#                 Ridge Regression                  #
#####################################################

# Can use lm.ridge() or glmnet()

ridge1 <- lm.ridge(Life.Exp ~., data=dat_state)
ridge1

# Compare the LS and Ridge coefficients.
# No difference because the default value for lambda (tunning parameter) is set to 0 by default.

coef(ridge1)
coef(mult.fit)


# Try a grid of values for lambda: from 10^-2 to 10^5

grid <- 10^seq(5,-2, length=100)


# Matrix of 100X8 containing coefficients for all 100 values of lambda
ridge2 <- lm.ridge(Life.Exp ~., data=dat_state, lambda=grid)
dim(coef(ridge2))

# What are the coeffcients for 10^5?
coef(ridge2)[1,]
coef(mult.fit)

# What are the coeffcients for 10^-2?
coef(ridge2)[100,]
coef(mult.fit)

######################################################
#            Use function glmnet()                   #
######################################################

# For this function, you have to declare your Y and Xs

Y <- dat_state[,4]

X <- as.matrix(dat_state[,-4])


# Penalty term: alpha; Ridge alpha=0 (default); Lasso alpha=1 (default)

ridge3<-glmnet(X, Y, alpha=0, lambda=grid)
dim(coef(ridge3))


# Look at lambda and the coeff estimates on position 50
# Lambda=34.30469

ridge3$lambda[50]

coef(ridge3)[,50]

# L2 norm for Ridge
# sqrt(sum(coef(ridge3)[-1,50]^2))

###########################################################
#          Choice of lambda: Cross Validation (CV)        #
###########################################################

# Use build-in CV function; performs a 10-fold validation by default
# glmnet() generates it's own lambda sequence

set.seed(2)
cv.out<-cv.glmnet(X[train,],Y[train], alpha=0)
plot(cv.out)


# cv.glmnet() object contains the mean cross-validation error (cvm),
# lambda min that gives the minimum cvm, etc.
cv.out
best.lambda<-cv.out$lambda.min
best.lambda                # 0.3919997

# Re-fit the model with the min lambda value, look at the coeff and MSE
ridge.pred <- predict(ridge.cv,s=best.lambda,newx=X[test,])

mean((ridge.pred-Y.test)^2)

# Ridge regression using all observations and 'best' lambda
ridge3<-glmnet(X, Y, alpha=0, lambda=best.lambda)
res_ridge_ls<- cbind(coef(mult.fit),coef(ridge3))
colnames(res_ridge_ls) <- c("LS", "Ridge")
res_ridge_ls

#############################################################################
#                           Lasso regression                                #
#############################################################################

lasso1<- glmnet(X[train ,],Y[train], alpha =1, lambda =grid)


# Cross-validation
set.seed(2)
cv.out<-cv.glmnet(X[train,],Y[train])
plot(cv.out)

best.lambda<-cv.out$lambda.min

#lasso.pred=predict(lasso1,s=best.lambda,newx=X[test,])
#mean((lasso.pred-Y.test)^2)

# Fit a Lasso model with all observations with the best lambda
lasso2<- glmnet(X, Y, alpha =1, lambda=best.lambda)
coef(lasso2)
coef(mult.fit)

# Fraction of deviance explained
# Similar interpretation to R-squared: % variance explained by non-zero variables variables
lasso2$dev.ratio

# Compare LS, Ridge and Lasso regression coefficients
res_ls_ridge_lasso<- cbind(coef(mult.fit),coef(ridge3),coef(lasso2))
colnames(res_ls_ridge_lasso) <- c("LS", "Ridge","Lasso")
res_ls_ridge_lasso

##############################################################################
#                             Additional code                                #
##############################################################################
# Using the entire data, fit Lasso regressions using the lambda grid used above.
# Save the estimated 'standardized' coefficients for all 7 predictors and plot them as functions of lambda or log(lambda).
# Use different colors for each variable and add a legend. 


# Using the entire data, fit Lasso regressions using the lambda grid.
lasso3 <- glmnet(X,Y, alpha=1, lambda=grid)

# Save the estimated 'standardized' coefficients for all 7 predictors without the intercept that is not of interest.
coef_lasso3 <- coef(lasso3)[-1,]
# Transpose the matrix
coef_lasso3_mat <- t(as.matrix(coef_lasso3))
# Rename and sort the matrix by asceding  grid
rownames(coef_lasso3_mat) <- grid
coef_lasso3_mat_sort <- coef_lasso3_mat[order(grid),]

# Plot using different colors
matplot(coef_lasso3_mat_sort,type="l",lty=1,xlim=c(0,50),
        xlab="lambda",ylab="coefficient",col=1:7) 
### add legend
legend('bottomright', inset=.05, legend=colnames(coef_lasso3_mat_sort), 
       pch=20, cex=0.8, col=1:7)

# Because of the different magnitudes, some of the predictors are not visible.
# You can separate them in different plots or play with the y-limits.



