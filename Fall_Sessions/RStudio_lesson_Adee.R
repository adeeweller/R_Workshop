

# commenting = good practice
# Always, always use google when confused
# code consistently
# file names -- avoid whitespace ('my_file.csv' vs 'my file.csv')



# get and set working directory
getwd()

# change this as needed!!!
setwd("C:/Users/adeew/OneDrive/Documents/Reelect_ref/data")


# load libraries -- put at top of code 

# only need to install packages once 
install.packages("dplyr")

# but load the libraries every time
library(dplyr)
library(tidyr)
library(stringr)
library(tidyverse)
library(texreg)
library(xtable)
library(ggplot2)
library(patchwork)
library(broom)

# see error when packages are not installed
library(ggmap)


########### ---- loading data ---------- ##############

# load data and take a look at it (.csv, .dta, .RData, etc.)
# encoding and headers are added here for visual nice-ness
panel <- read.csv("panel_lesson.csv", header = TRUE, encoding = "UTF-8")

# head, tail, names, View, dim
dim(panel)

# look at variables

# states
panel$State
state <- unique(panel$State)

# characters, numeric, int, string, list, dataframe, etc.
class(state)

# years
year <- unique(panel$Year)
class(year)
summary(year)
# 2015 vs '2015'



########### ---- basic transformations of variable ----- ##########

# basic math
110 + 210 + 60 + 90

# make a vector
vector_num <- c(1,2,4,5,6)
vector_char <- c("Hello", "World")

# set seed for reproducability (tells R where to start a randomization. Without it, will get slightly different numbers every time)
set.seed(1234)

# random vector
# drawn from a normal distribution with a defined mean and standard deviation
random <- rnorm(n = 100, mean = 0, sd = 1)
# sequence of numbers
listed <- seq(from = 1, to = 100, by = 1)

# to check the randomization, here is the correlation (should be somewhat close to 0)
cor(random, listed)

# make a dataframe
df <- data.frame(listed, random)


# rename column and pipes (Dplyr!) -- save to new data frame
panel1 <- panel %>%
  rename(Homicides = homicidio)

names(panel1)

# new data frame with homicide rate per 100,000 residents
panel2 <- panel1 %>%
  select(State, Municipality, Homicides, population) %>%
  mutate(hom_rate = (Homicides / population)*100000)

# need help with a function
?mutate()

# look at output
head(panel2)
summary(panel2$hom_rate)

# changed mind and want to add hom_rate back into panel
panel$hom_rate <- panel2$hom_rate


# subsetting and indexing
high_hom <- which(panel$hom_rate > 100)

# index using []
high_hom_ind <- panel[high_hom,]
head(high_hom_ind)

# subset
high_hom_sub <- subset(x = panel, hom_rate > 100)

sonora <- subset(high_hom_sub, State == "Sonora")


# for loops, ifelse, and functions

# basic loop
for (i in 1:10){
  print(i)
}

# storage vector
sonora_ind <- rep(0, times = length(panel$State))

# loop for indicator of Sonora
for (i in 1:length(sonora_ind)){
  if(panel[i,4] == "Sonora"){
    sonora_ind[i] <- 1
  } else {
    sonora_ind[i] <- 0
  }
}

# check and make sure it works (should be greater than 0)
sum(sonora_ind)/7

# can also do this in an ifelse statement
sonora_ind2 <- ifelse(panel$State == "Sonora", 1, 0)

# are the two lists different? Should be the same
setdiff(list(sonora_ind),list(sonora_ind2))

# add to df
panel$sonora <- sonora_ind


# functions

sonora.fun <- function(vector){
  sonora_indicator <- rep(0, times = length(vector))
  for (i in 1:length(vector)){
    if(vector[i] == "Sonora"){
      sonora_indicator[i] <- 1
    } else {
      sonora_indicator[i] <- 0
    }
  }
  return(sonora_indicator)
}

# see function in environment

# should look just like all the others
sonora_function <- sonora.fun(panel$State)



######### --- Models and Visualization ---- ########

year_2015 <- subset(panel, Year == 2015)

ggplot(data = panel, aes(x = population, y = homicidio)) + 
  geom_point()

# linear models
model <- lm(homicidio ~ population, data = panel)

summary(model)
coef <- coef(summary(model))

# scatterplot
p1 <- ggplot(data = panel, aes(x = population, y = homicidio)) + 
  geom_point() + 
  geom_abline(slope = coef[2,1], intercept = coef[1,1], color = "red") + 
  scale_x_continuous("Population") + 
  scale_y_continuous("Homicide") + 
  theme_bw()

# perhaps better, but not helpful for prediction
p2 <- ggplot(data = panel, aes(x = population, y = homicidio)) + 
  geom_point() + 
  stat_smooth(formula = y ~ x, method = 'loess') + 
  scale_x_continuous("Population") + 
  scale_y_continuous("Homicide") + 
  theme_bw()

# show plots and export in patchwork
p1 + p2

p3 <- p1 + p2

# this saves to your working directory
ggsave("models.pdf", p3)


# predicted values (million to billion)
new_data <- as.numeric(runif(n = 1000, min = 1000000, max = 1000000000))
new_data <- data.frame(population = new_data)

predicted <- predict(model, newdata = new_data)

new_data$predicted <- predicted

# should be a perfectly straight line with slope and intercept from model
ggplot(data = new_data) + 
  geom_point(aes(x = population, y = predicted)) + 
  scale_x_continuous("Synthetic Population") +
  scale_y_continuous("Predicted Homicides")

# discrete independent variables
model2 <- lm(hom_rate ~ as.factor(MayorParty), data = year_2015)
summary(model2)


#### Interactions in Linear Models ####

# y ~ x + z vs y ~ x + z + x*z (independent vs dependent)
model3 <- lm(hom_rate ~ Airports + Poppies, data = year_2015)
summary(model3)
coef3 <- coef(summary(model3))

model4 <- lm(hom_rate ~ Airports + Poppies + Airports*Poppies, data = year_2015)
summary(model4)
coef4 <- coef(summary(model4))

# anova to compare if models are different
anova(model3, model4)

# transformations in lm (I(x^2)) and poly(x,2)
model5 <- lm(narcomenudeo ~ I(population^2), data = year_2015)
summary(model5)

ggplot(data = year_2015) + 
  geom_point(aes(x = population, y = narcomenudeo))


# log()
# ggplot(data = year_2015) + 
  # geom_point(aes(x = narcomenudeo, y = hom_rate)) 

ggplot(data = year_2015) + 
  geom_point(aes(x = log(narcomenudeo), y = hom_rate)) 


# Difference-in-Differences
did <- lm(hom_rate ~ After_treat + Treatment2 + After_treat*Treatment2, data = panel)
summary(did)

# xtable(did)

did2 <- tidy(lm(hom_rate ~ After_treat + Treatment2 + After_treat*Treatment2, data = panel))
did2

# output to .csv file
write.csv(did2, file = "did.csv")

# this can be easily copied into a Word doc



# changing multiple values
not_sonora <- rep(0, times = length(panel$State))

for (i in 1:length(panel$State)){
  if (panel$State[i] == "Sonora" | panel$State[i] == "Aguascalientes"){
    not_sonora[i] <- NA
  } else {
    not_sonora[i] <- panel$State[i]
  }
}




