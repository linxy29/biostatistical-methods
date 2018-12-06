################################################################
#             Biostatistical Methods I: Lecture 8              #
#             One-Way Analysis of Variance (ANOVA)             #
#           Author: Cody Chiuzan; Date: Oct 2, 2018            #
################################################################

rm(list=ls())

################################################################
# A study is examining the effect of glucose on insulin release. 
# Specimens of pancreatic tissue from experimental animals were 
# treated with five different stimulants and the insulin levels were recorded.
# Use an ANOVA test to compare the mean insulin levels across the five groups.

################################################################

ins1<-c(1.53,1.61,3.75,2.89,3.26)
ins2<-c(3.15, 3.96,3.59,1.89,1.45,1.56)
ins3<-c(3.89,3.68,5.70,5.62,5.79,5.33)
ins4<-c(8.18,5.64,7.36,5.33,8.82,5.26,7.10)
ins5<-c(5.86,5.46,5.69,6.49,7.81,9.03,7.49,8.98)

# Re-shape the data
insulin<-c(ins1,ins2,ins3,ins4,ins5)
ind<-c(rep(1,length(ins1)),rep(2,length(ins2)),rep(3,length(ins3)),rep(4,length(ins4)),rep(5,length(ins5)))

new_data<-as.data.frame(cbind(insulin,ind))
head(new_data)

# Summarize the data

tmp_functn<-function(x)c(sum=sum(x), mean=mean(x), var=var(x), n=length(x))
tapply(insulin, ind, tmp_functn)

# Create box-plots 
boxplot(insulin~ind,data=new_data,main="Effect of glucose on insulin release",xlab="Experimental Group", ylab="Insulin levels")

# Perform an ANOVA test: are the mean insulin levels significantly different?
# Need to mention the independent variable as a factor; o/w will be considered continuous
# Function lm() is broader, including linear regression models
res<-lm(insulin~factor(ind), data=new_data)

# Coefficients of the ANOVA model with 'grand mean' and alpha effects.
# Will use them later in regression.
res

# Our regular ANOVA table with SS, Mean SS and F-test
anova(res)


# Another option using aov()
res1<-aov(insulin~factor(ind), data=new_data)
summary(res1)


# Multiple comparisons adjustments: includes Bonferroni, Holm, Benjamini-Hochberg
pairwise.t.test(new_data$insulin, new_data$ind, p.adj='bonferroni')

# For Tukey, we need to use another function with an object created by aov()
TukeyHSD(res1)


