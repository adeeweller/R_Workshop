######################################################
# Session 2: Introduction to R                        #
# R Workshop Fall 2023                                #
# Author: Adee Weller and Eddy Yeung                  #
# Date: 11/20/2023                                    #
######################################################

setwd("C:\\Users\\adeew\\OneDrive\\Documents\\Teaching_files\\R_Workshop\\Tech_bootcamp")
# /Users/eddy/Desktop/Emory/teaching/R workshop

# Load packages
#install.packages("estimatr")
library(estimatr)
library(ggplot2)
library(lmtest)
library(texreg)
library(dplyr)

# Load WVS
load("~/Downloads/WVS_Cross-National_Wave_7_rData_v5_0.rdata")
df <- `WVS_Cross-National_Wave_7_v5_0`

head(df)

# For-loops
# These can be helpful to iterate commands over a loop of objects

# LNGE_ISO is the language the survey was conducted in

# unique elements
langs <- unique(df$LNGE_ISO)

# for each element in unique elements
	# print the name
for (lang in langs){
	print(paste0("This survey was conducted in ", lang))
}

# Often, there may be more efficient commands for simple actions (for example, you can manually add each element of X to each element of Y to produce a new column Z)

# But it is *very* useful for computer coding, ex. in python 

# When there is not a built-in function, for loop can be intuitive

# Say we want to find out all the countries where the survey was conducted in a language that we have proficiency in

# known languages: english, spanish, french
known <- c("en", "es", "fr")

# empty storage vector
known_countries <- NULL

# What we want to do: subset the WVS to only surveys in known languages -- save the country IDs

# will use 'i' as a numerical indicator of the element number in a vector. For example: "en, es, fr" = "i=1, i=2, i=3"

# for each element in ``langs''
for (i in 1:length(langs)){
	# if it is known
	if (langs[i] %in% known){
		# subset the main dataframe
		df_2 <- subset(df, df$LNGE_ISO == langs[i])
		# save the country ID codes once
		known_countries <- unique(df$B_COUNTRY)
		# add it to the running list
		known_countries <- rbind(known_countries)
	}
}

known_countries

# (for loops in R can take a while to run!)

# these are all the countries which conducted interviews in any of the three languages listed above
unique(as.vector(known_countries))

# But you can also produce for-loops for internal organization (such as renaming files), which can be very helpful!

# like loading packages:
pkgs <- c("tmap", "dplyr", "plm")

for (pkg in pkgs) {
  if (! (pkg %in% rownames(installed.packages()))) { install.packages(pkg) }
  lapply(pkg, library, character.only=T )
}


### python example ###


# US only
df_US <- subset(df, df$B_COUNTRY == 840)

head(df_US)

# Writing your own functions
# This can be useful for repetitive tasks

# are there equal distribution in the average ages of men and women in the survey respondents?

# Q261: year of birth
# Q260: gender

# subset to men and women
df_1 <- subset(df, df$Q260 == 1)
df_2 <- subset(df, df$Q260 == 2)

# define function
diff_means <- function(y_vec, x_vec) {
  mean_y_group <- mean(y_vec)
  mean_x_group <- mean(x_vec)
  difference <- mean_y_group - mean_x_group

  t_test_result <- t.test(y_vec, x_vec)

  return(list(
    difference = difference,
    t_test_result = t_test_result
  ))
}

# see if there is a difference in the average ages of men and women respondents
diff_means(df_1$Q261, df_2$Q261)



# regression basics (lm_robust())

# Why do some people think their country is much more/less corrupt than other places, even when the statistical evidence does not support this?

# IV: 
# Q59 -- trust people in your neighborhood
table(df$Q59)
df$trust_neighbor <- ifelse(df$Q59 < 1, NA, 4 - df$Q59)
table(df$trust_neighbor)

# DV:
# Q112 -- perceptions of corruption in the country
table(df$Q112)
df$corrupt_view <- ifelse(df$Q112 < 1, NA, df$Q112)
table(df$corrupt_view)

# Control:
# Q52 -- frequency you/family (last 12 month): Felt unsafe from crime in your own home
table(df$Q52)
df$felt_unsafe <- ifelse(df$Q52 < 1, NA, 4 - df$Q52)
table(df$felt_unsafe)

# First regress using lm()
m1 <- lm(corrupt_view ~ trust_neighbor + felt_unsafe, data = df)

summary(m1)

# Now fit the same model using lm_robust() for robust standard errors
m2 <- lm_robust(corrupt_view ~ trust_neighbor + felt_unsafe, data = df)

summary(m2)

# Compare standard errors 
cat("Standard Errors from Ordinary Least Squares (OLS):\n")
print(summary(m1)$coefficients[, "Std. Error"])

cat("Standard Errors from Robust Regression:\n")
print(summary(m2)$coefficients[, "Std. Error"])

# country fixed effects 
m3 <- lm_robust(corrupt_view ~ trust_neighbor + felt_unsafe, data = df, fixed_effects = ~B_COUNTRY)

summary(m3)

# Outputting regressions into LaTeX
robust_table <- texreg(m3, caption = "Impact of Trust on Perceptions of Corruption",
  custom.coef.names = c("Trust in Neighbors", "Victim of Crime"),
  custom.model.names = c("Robust Model"))

robust_table

# multiple models
models <- list("OLS" = m1, "Robust" = m2, "Fixed Effects" = m3)

tables <- texreg(models)

# Customize the output
texreg(models, stars = c(0.01, 0.05, 0.10), include.ci = FALSE)

# ChatGPT for Table Formatting

# AI can be really helpful for simple coding tasks, like formatting or identifying where coding errors occur. 

# Use ChatGPT to change one formatting element of the table you produced above



# Run a new regression that adds demographic controls

# Sex
table(df$Q260)
df <- df %>% mutate(female = case_when(
  Q260 == 2 ~ 1,
  Q260 == 1 ~ 0
))
table(df$female)

# Age
table(df$Q262)
df <- df %>% mutate(age = case_when(
  Q262 >= 16 ~ Q262
))
table(df$age)

# College education
table(df$Q275)
df <- df %>% mutate(college = case_when(
  Q275 >= 0 & Q275 <= 5 ~ 0,
  Q275 >= 6 & Q275 <= 8 ~ 1
))
table(df$college)

# Class
table(df$Q287)
df <- df %>% mutate(class = case_when(
  Q287 == 1 ~ 4,
  Q287 == 2 ~ 3,
  Q287 == 3 ~ 2,
  Q287 == 4 ~ 1,
  Q287 == 5 ~ 0
))
table(df$class)

m4 <- lm_robust(corrupt_view ~ trust_neighbor + felt_unsafe +
                  female + age + college + class, 
                data = df, 
                fixed_effects = ~B_COUNTRY)

# Coefficient Plot
df_coef <- tidy(m4)

# Ugly
ggplot(df_coef, aes(x = estimate, y = term)) +
  geom_point() +
  geom_errorbar(aes(xmin = conf.low, xmax = conf.high))

# Better
df_coef$term <- 
  factor(df_coef$term, 
         levels = c("class", "college", "female", "age", "felt_unsafe", "trust_neighbor"),
         labels = c("Subjective Social Class", "College Degree", "Female", "Age",
                    "Victim of Crime", "Trust in Neighbors"))

ggplot(df_coef, aes(x = estimate, y = term)) +
  geom_point(size = 2) +
  geom_errorbar(aes(xmin = conf.low, xmax = conf.high), width = 0) +
  theme_bw() +
  ylab("") +
  xlab("Coefficient Estimate with 95% Confidence Intervals") +
  coord_cartesian(xlim = c(-0.25, 0.15)) +
  geom_vline(xintercept = 0, linetype = "dashed")

### Propensity score matching (PSM) ----
## Load the package
library(MatchIt)  # https://kosukeimai.github.io/MatchIt/
#install.packages("optmatch")
library(optmatch) # for full mathcing
#install.packages("marginaleffects") # for treatment effect estimation after matching
library(marginaleffects)

## Read the dataset
data("lalonde")
head(lalonde)

## Check initial imbalance
# No matching; constructing a pre-match matchit object
m.out0 <- matchit(treat ~ age + educ + race + married + 
                    nodegree + re74 + re75, data = lalonde,
                  method = NULL, distance = "glm")
summary(m.out0)

## Perform matching
# First specification: 1:1 nearest neighbor PSM w/o replacement
# (One by one, each treated unit is paired with an available control unit that has the closest propensity score to it)
m.out1 <- matchit(treat ~ age + educ + race + married + 
                    nodegree + re74 + re75, data = lalonde,
                  method = "nearest", distance = "glm")
summary(m.out1)
plot(summary(m.out1))
plot(m.out1, type = "jitter", interactive = FALSE)

# Second specification: full matching on a probit PS
# (Every treated unit is matched with at least one control AND every control is matched with at least one treated unit)
m.out2 <- matchit(treat ~ age + educ + race + married + 
                    nodegree + re74 + re75, data = lalonde,
                  method = "full", distance = "glm", link = "probit")
summary(m.out2)
plot(summary(m.out2)) # compare with 1:1 nearest neighbor PSM
plot(m.out2, type = "jitter", interactive = FALSE)

## Estimate the treatment effect
# Extract the matched dataset from the matchit object using match.data()
m.data <- match.data(m.out2)
head(m.data)

# Model the outcome in this dataset using the standard regression functions in R
fit <- lm(re78 ~ treat * (age + educ + race + married + nodegree + re74 + re75), 
          data = m.data, 
          weights = weights # make sure to include the matching weights in the estimation
          )

# Perform g-computation to estimate the ATT
avg_comparisons(fit,
                variables = "treat",
                vcov = ~subclass,
                newdata = subset(m.data, treat == 1),
                wts = "weights")



