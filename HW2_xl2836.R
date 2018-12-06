########################################
######   Biostatistical Methods   ######
######         Homework 2         ######
######         Xinyi Lin          ######
########################################

## Problem 3

# question a
dbinom(5, 10, 0.08)

# question b
dbinom(5, 10, 0.3)

## Problem 4

# question a
dpois(30, 42.75)

# question b
dpois(30, 22.96)
dpois(30, 0.665)
dpois(30, 0.393)

## Problem 5

# question c
pnorm(20, 35, 5) + pnorm(50, 35, 5, lower.tail = FALSE)

# Problem 6

library(readxl)
library(tidyverse)

migraine_data <- read_excel("./BM/Migraine.xlsx") # import data
migraine_data = janitor::clean_names(migraine_data)

## question a
migraine_neg = filter(migraine_data, migraine == 0)    # get the data of patients without migraine
migraine_pos = filter(migraine_data, migraine == 1)    # get the data of patients with migraine

# epilepsy patients with migraine

# calculate the mean of three variables
mean(migraine_pos$cesd, na.rm = TRUE)
mean(migraine_pos$nddie, na.rm = TRUE)
mean(migraine_pos$abnas_memory, na.rm = TRUE)
mean(migraine_pos$abnas_language, na.rm = TRUE)

# calculate the standard deviation of three variables
sd(migraine_pos$cesd, na.rm = TRUE)
sd(migraine_pos$nddie, na.rm = TRUE)
sd(migraine_pos$abnas_memory, na.rm = TRUE)
sd(migraine_pos$abnas_language, na.rm = TRUE)

# calculate the missing values number of three variables
sum(is.na(migraine_pos$cesd))
sum(is.na(migraine_pos$nddie))
sum(is.na(migraine_pos$abnas_memory))
sum(is.na(migraine_pos$abnas_language))

# calculate the sample sizes of three variables
length(migraine_pos$migraine) - sum(is.na(migraine_pos$cesd))
length(migraine_pos$nddie) - sum(is.na(migraine_pos$nddie))
length(migraine_pos$abnas_memory) - sum(is.na(migraine_pos$abnas_memory))
length(migraine_pos$abnas_language) - sum(is.na(migraine_pos$abnas_language))

# get CESD (for subjects with score >= 16) and CESD (for subjects with score < 16)
pos_cesdlar = migraine_pos$cesd[migraine_pos$cesd >= 16]
pos_cesdsma = migraine_pos$cesd[migraine_pos$cesd < 16]

# get NDDIE (for subjects with score >= 16) and NDDIE (for subjects with score < 16)
pos_nddielar = migraine_pos$nddie[migraine_pos$nddie >= 16]
pos_nddiesma = migraine_pos$nddie[migraine_pos$nddie < 16]

# calculate mean of these four data sets
mean(pos_cesdlar, na.rm = TRUE)
mean(pos_cesdsma, na.rm = TRUE)
mean(pos_nddielar, na.rm = TRUE)
mean(pos_nddiesma, na.rm = TRUE)

# calculate standard deviation of these four data sets
sd(pos_cesdlar, na.rm = TRUE)
sd(pos_cesdsma, na.rm = TRUE)
sd(pos_nddielar, na.rm = TRUE)
sd(pos_nddiesma, na.rm = TRUE)

# calculate the missing values number of these four data sets
sum(is.na(pos_cesdlar))
sum(is.na(pos_cesdsma))
sum(is.na(pos_nddielar))
sum(is.na(pos_nddiesma))

# calculate the sample sizes of three variables
length(pos_cesdlar) - sum(is.na(pos_cesdlar))
length(pos_cesdsma) - sum(is.na(pos_cesdsma))
length(pos_nddielar) - sum(is.na(pos_nddielar))
length(pos_nddiesma) - sum(is.na(pos_nddiesma))

# epilepsy patients without migraine

# calculate the mean of three variables
mean(migraine_neg$cesd, na.rm = TRUE)
mean(migraine_neg$nddie, na.rm = TRUE)
mean(migraine_neg$abnas_memory, na.rm = TRUE)
mean(migraine_neg$abnas_language, na.rm = TRUE)

# calculate the standard deviation 
sd(migraine_neg$cesd, na.rm = TRUE)
sd(migraine_neg$nddie, na.rm = TRUE)
sd(migraine_neg$abnas_memory, na.rm = TRUE)
sd(migraine_neg$abnas_language, na.rm = TRUE)

# calculate the missing values number of three variables
sum(is.na(migraine_neg$cesd))
sum(is.na(migraine_neg$nddie))
sum(is.na(migraine_neg$abnas_memory))
sum(is.na(migraine_neg$abnas_language))

# calculate the sample sizes of three variables
length(migraine_neg$migraine) - sum(is.na(migraine_neg$cesd))
length(migraine_neg$nddie) - sum(is.na(migraine_neg$nddie))
length(migraine_neg$abnas_memory) - sum(is.na(migraine_neg$abnas_memory))
length(migraine_neg$abnas_language) - sum(is.na(migraine_neg$abnas_language))

# get CESD (for subjects with score >= 16) and CESD (for subjects with score < 16)
neg_cesdlar = migraine_neg$cesd[migraine_neg$cesd >= 16]
neg_cesdsma = migraine_neg$cesd[migraine_neg$cesd < 16]

# get NDDIE (for subjects with score >= 16) and NDDIE (for subjects with score < 16)
neg_nddielar = migraine_neg$nddie[migraine_neg$nddie >= 16]
neg_nddiesma = migraine_neg$nddie[migraine_neg$nddie < 16]

# calculate mean of these four data sets
mean(neg_cesdlar, na.rm = TRUE)
mean(neg_cesdsma, na.rm = TRUE)
mean(neg_nddielar, na.rm = TRUE)
mean(neg_nddiesma, na.rm = TRUE)

# calculate standard deviation of these four data sets
sd(neg_cesdlar, na.rm = TRUE)
sd(neg_cesdsma, na.rm = TRUE)
sd(neg_nddielar, na.rm = TRUE)
sd(neg_nddiesma, na.rm = TRUE)

# calculate the missing values number of these four data sets
sum(is.na(neg_cesdlar))
sum(is.na(neg_cesdsma))
sum(is.na(neg_nddielar))
sum(is.na(neg_nddiesma))

# calculate the sample sizes of three variables
length(neg_cesdlar) - sum(is.na(neg_cesdlar))
length(neg_cesdsma) - sum(is.na(neg_cesdsma))
length(neg_nddielar) - sum(is.na(neg_nddielar))
length(neg_nddiesma) - sum(is.na(neg_nddiesma))

## question b

# changes migraine variables for further analysise
migraine_data = mutate(migraine_data, migraine = recode(migraine, `0` = "without_migraine", `1` = "with_migraine")) 

# drawing histogram pictures of three variables.
ggplot(migraine_data, aes(x = nddie)) +
  geom_histogram(alpha = .4, adjust = .5) +
  facet_grid(. ~ migraine)

ggplot(migraine_data, aes(x = cesd)) +
  geom_histogram(alpha = .4, adjust = .5) +
  facet_grid(. ~ migraine)

ggplot(migraine_data, aes(x = abnas_memory)) +
  geom_histogram(alpha = .4, adjust = .5) +
  facet_grid(. ~ migraine)

ggplot(migraine_data, aes(x = abnas_language)) +
  geom_histogram(alpha = .4, adjust = .5) +
  facet_grid(. ~ migraine)

# drawing boxplot pictures of three variables.

ggplot(migraine_data, aes(x = migraine, y = nddie)) +
  geom_boxplot()

ggplot(migraine_data, aes(x = migraine, y = cesd)) +
  geom_boxplot()

ggplot(migraine_data, aes(x = migraine, y = abnas_memory)) +
  geom_boxplot()

ggplot(migraine_data, aes(x = migraine, y = abnas_language)) +
  geom_boxplot()