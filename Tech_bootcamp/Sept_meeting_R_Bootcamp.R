######################################################
# Session 1: Introduction to R                        #
# R Workshop Fall 2023                                #
# Author: Adee Weller and Eddy Yeung                  #
# Date: 09/18/2023                                    #
######################################################

######## Working Directory Questions -----

# Projects always start with a clean slate. With single R files it is however important to clean the directory. 
rm(list = ls())

# Since we are in a project, we do not need to set the working directory. 
# We are in a self contained space 
# If you are just using a single R file. It is necessary to set the working directory. You can do this with this:


getwd()

## define path if needed
setwd("C:/Users/adeew/OneDrive")



######## R Fundamentals -----

## We can do all basic operations here...

5/6
3+1
3*555
2^5

# We can save objects just like in Mathematica...

x <- 5
y <- x*10

# But we cannot do symbolic math (all objects need to be defined)

z*y


## The following operators:

# | OR

# &  AND

# ! NOT

# == Equals

# != NOT equal

# >= greater or equal

# <= smaller or equal

# %in% Belong to

# %*% Matrix Multiplication

# %>% Pipe operator



######## Load Packages -----

install.packages("dplyr")
library(dplyr)

# Automatically installs packages if needed
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse,haven,nycflights13,readxl) 


# Another way to install and load packages
pkgs <- c("tidyr", "readr",  "stargazer", "ggplot2", "stringr")

for (pkg in pkgs) {
  if (! (pkg %in% rownames(installed.packages()))) { install.packages(pkg) }
  lapply(pkg, library, character.only=T)
}

# Can be helpful to use a version control package to record which version of a specific package you use to do your analyses to replication later

# (This is generally more important in languages with higher turnover rates, like python.)


######## Making and Loading Data -----


## Creating Data (a.k.a. simulating)
# Most common way to create data is that assume some distribution and take random draws

set.seed(1234) # crucial for replication

x <- rnorm(mean = 4,sd=10,n=100)
z <- rpois(100,lambda = 4)
y <- 5*x - 3*z^2 + rnorm(100)  

fake.dat <- data.frame(y,x,z)

## Writing Data (a.k.a saving it on your drive)
# There are many functions that write data frames. 
# You want to have file format that is easiest to handle in R --> .csv and .RData 

readr::write_csv(fake.dat,'fake_df.csv') 

save(fake.dat, file = 'fake_df.RData')

## Loading Data
fake.dat.csv <- read.csv('fake_df.csv')

## There are a lot of formats in which data can be saved
# .rdata, .csv, etc....

## There are also different data set types:

# List
# Dataframe
# tibble
# matrix
# vector

head(fake.dat.csv)
tail(fake.dat.csv)
str(fake.dat.csv)
summary(fake.dat.csv)



## World Values Survey
load("~/Downloads/WVS_Cross-National_Wave_7_rData_v5_0.rdata")
df <- `WVS_Cross-National_Wave_7_v5_0`


# Subset the dataset
df_US <- subset(df, df$B_COUNTRY == 840) # US only

# Tabulate a variable
table(df_US$Q29)
table(df_US$Q29, useNA = "always") # show NAs
table(df_US$Q31, useNA = "always")

# Cross-tabulate two variables
table(df_US$Q29, df_US$Q31)

# Recode a variable
df_US$sexist_1 <- ifelse(df_US$Q29 == -1 | df_US$Q29 == -2, NA, df_US$Q29)
table(df_US$sexist_1, useNA = "always")
df_US$sexist_1 <- 4 - df_US$sexist_1
table(df_US$sexist_1, useNA = "always")

df_US$sexist_2 <- ifelse(df_US$Q31 == -1 | df_US$Q31 == -2, NA, df_US$Q31)
table(df_US$sexist_2, useNA = "always")
df_US$sexist_2 <- 4 - df_US$sexist_2
table(df_US$sexist_2, useNA = "always")

# Create an index by adding up the variables
df_US$sexism <- df_US$sexist_1 + df_US$sexist_2
table(df_US$sexism)

## Case study: knowledge about IOs (Q91, Q92, Q93)



######## Cleaning Data -----

######## Tidyverse and Pipe operator -----

# Tidyverse is a set of packages that should make data science easier
# One of the tidyverses key innovation is the pipe operator
# It allows you to stack operations within one line of code

# Consider the following "old school" code
df_US$demean_Q29 <- df_US$Q29 - mean(df_US$Q29)
df_US_new <- df_US[, ncol(df_US)]

head(df_US_new)

# This can all be done in one line of code
dat_new_t <- df_US %>% 
  mutate(demean_Q29 = Q29 - mean(Q29)) %>% 
  select(demean_Q29)

head(dat_new_t)

# Subset with pipe
df_US2 <- df %>% 
  filter(B_COUNTRY == 840)

# saving objects
mil_trust <- df_US2$I_TRUSTARMY

mil_trust_31 <- mil_trust[31]

# class of vector
class(mil_trust)

# data type conversion
mil_trust <- as.numeric(mil_trust)
# as.character
as.character(mil_trust)
# as.Date

# Only complete cases
complete.cases(df_US2)

# Handle missing values (replace NA with 0)
dat_cleaned <- df_US2 %>%
  mutate_all(funs(ifelse(is.na(.), 0, .)))

# text cleaning
text_data <- c("Hello, World!", "This is a sample text with punctuation.", "   Extra spaces     ")

# Remove special characters and punctuation
cleaned_text <- str_replace_all(text_data, "[^A-Za-z0-9 ]", "")

# Convert text to lowercase
cleaned_text <- tolower(cleaned_text)

# Tokenize text (split into words)
tokenized_text <- str_split(cleaned_text, "\\s+")

# Remove extra spaces and leading/trailing spaces
cleaned_text <- str_trim(cleaned_text)

# Print the cleaned text and tokenized text
cat("Cleaned Text:\n")
cat(cleaned_text, "\n\n")

cat("Tokenized Text:\n")
for (sentence in tokenized_text) {
  cat(sentence, "\n")
}

### manipulating data

# merging two subsets
data.frame(unique(df$B_COUNTRY_ALPHA), unique(df$B_COUNTRY))

df_US <- df %>% 
  filter(B_COUNTRY == 840)

df_MEX <- df %>% 
  filter(B_COUNTRY == 484)

# Binding by rows
df_dual <- rbind(df_US, df_MEX)

# Merge // left_join
# right_join

## You need to define a key variable(s) to match between *both* data sets!







