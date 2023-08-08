## Clear all
rm(list=ls())

# load libraries
pkgs <- c("fixest", "dplyr", "sandwich", "did", "stargazer", "ggplot2", "margins", "rddtools", "magrittr", "rdrobust", "MatchIt", "modelsummary", "stats", "FactoMineR", "corrplot", "broom", "miceadds")

# write a loop that installs and loads the library
for (pkg in pkgs) {
  if (! (pkg %in% rownames(installed.packages()))) { install.packages(pkg) }
  lapply(pkg, library, character.only=T )
}


# always comment code!



# load data
getwd()

setwd('C://Users//adeew//OneDrive//Documents//Teaching_files')

# list all files in directory
list.files(path = ".", pattern = NULL)

# list only CSVs
list.files(path = ".", pattern = '*.csv$')

# load data
panel <- read.csv('panel_lesson.csv', encoding = 'UTF-8')

# look at data
head(panel)

# rename a variable
panel <- panel %>%
  mutate(Homicides = Homicidio)




# Start with a simple RQ: What is the relationship of some treatment on homicides?

# for the sake of using multiple different estimators, this will be a hypothetical treatment


# We will be going through experimental analysis, matching, DiDs, RDDs, survey results, marginal effects and visualizations



# first, it may be helpful to think about correlation rather than causation


# in an experimental approach, we can randomly assign treatment and control

# for one year
assignment <- c(0,1)

panel_2015 <- subset(panel, panel$Year == 2015)

panel_2015$assigned_treatment <- sample(assignment, length(panel_2015$Year), replace = TRUE)

head(panel_2015)

corr_dat <- na.omit(panel_2015)


corr_dat <- corr_dat %>%
  select(State_code, Municipal_code, assigned_treatment, MajorPort, MajCity, Airports, Railline, Oilline, Intlborder, Shoreline, Poppies, Homicides, Population)

# visualization
result <- PCA(corr_dat)



# correlation-covariance matrix
vcov_mat <- cor(corr_dat)

corrplot(vcov_mat, method = 'number')


# difference in means
treated_2015 <- subset(panel_2015, panel_2015$assigned_treatment == 1)
control_2015 <- subset(panel_2015, panel_2015$assigned_treatment == 0)


# counts
table(panel_2015$assigned_treatment)

# check balance
panel_2015 %>% 
  group_by(assigned_treatment) %>% 
  summarize(mean_highway = mean(na.omit(MajorHighway)),
            mean_rail = mean(na.omit(Railline)),
            mean_pop = mean(na.omit(Population)))


mean_treat <- mean(na.omit(treated_2015$Homicides))
mean_control <- mean(na.omit(control_2015$Homicides))

diff_means <- mean_treat - mean_control
diff_means


## Matching and Inverse Probability Weighting (IPW)

# for matching, we can use the MatchIt package (Mahalanobis distance) 

# remove missing data
panel_2015 <- na.omit(panel_2015)

# preprocess data (covariates on treatment)
matched_data <- matchit(assigned_treatment ~ MajCity + Railline + Population + Poppies,
                        data = panel_2015,
                        method = "nearest",
                        distance = "mahalanobis",
                        replace = TRUE) # this is one-to-many matching
summary(matched_data)

# produce dataframe
matched_data <- match.data(matched_data)

# estimation

matched_model <- lm(Homicides ~ assigned_treatment, data = matched_data)

tidy(matched_model)

matched_model_w <- lm(Homicides ~ assigned_treatment, data = matched_data, weights = weights)

tidy(matched_model_w)

# if matching rejects too much data, can use IPW
model_ipw <- glm(assigned_treatment ~ MajCity + Railline + Population + Poppies,
                 data = panel_2015,
                 family = binomial(link = "logit"))
tidy(model_ipw)

# use these estimates to generate probabilites
?augment_columns()

probabilities <- augment_columns(model_ipw, panel_2015, type.predict = "response") %>% 
  rename(propensity = .fitted)

# Look at the first few rows of a few columns
probabilities %>% 
  select(assigned_treatment, MajCity, Railline, Population, Poppies, propensity) %>% 
  head()

# calculate weights
ipw <- probabilities %>%
  mutate(ipw = (assigned_treatment / propensity) + ((1 - assigned_treatment) / (1 - propensity)))

ipw %>% 
  select(assigned_treatment, MajCity, Railline, Population, Poppies, propensity, ipw) %>% 
  head()

# estimation with IPW
# we can use the IPW in regressions

model_ipw <- lm(Homicides ~ assigned_treatment, 
                data = ipw,
                weights = ipw)

tidy(model_ipw)



# what do all of these look like together?

# no matching at all

model_simple <- lm(Homicides ~ assigned_treatment, 
                   data = panel_2015)

# list all models
modelsummary(list("Naive" = model_simple, 
                  "Matched" = matched_model, "Matched + weights" = matched_model_w, 
                  "IPW" = model_ipw))





### Difference-in-Differences (DiDs)

# indicator of treatment

# creating a dummy variable of when treatment is 'turned on'
panel$Treat_ind <- ifelse(panel$Year == 2018, 1, 0)

# dummy variable based on group (for whom treatment is turned on)
panel$treated_ind <- ifelse(panel$State == "Aguascalientes" |
                              panel$State == 'Baja California' |
                              panel$State == 'Baja California', 1, 0)

# create an interaction of time and treatment
panel$did <- panel$treated_ind * panel$Treat_ind

# check work
head(panel)

# estimators
did <- lm(Homicides ~ treated_ind + Treat_ind + did + Poppies + MajCity + Population, data = panel)

summary(did)

# this will produce the same results
did1 <- lm(Homicides ~ treated_ind*Treat_ind + Poppies + MajCity + Population, data = panel)

summary(did1)


# fixed effects (fixest)
did_fe <- feols(Homicides ~ treated_ind*Treat_ind + Poppies + MajCity + Population | State, data = panel)
# discuss why there is an issue with collinearity here

summary(did_fe)

# what if we want to cluster standard errors?
did_cluster <- feols(Homicides ~ treated_ind*Treat_ind + Poppies + MajCity + Population, data = panel, cluster = ~State)

# note, there are a *lot* of ways to cluster standard errors (sandwich, lmtest, miceadds packages)

# example with miceadds
did_cluster2 <- lm.cluster(Homicides ~ treated_ind*Treat_ind + Poppies + MajCity + Population, 
                           data = panel,    
                           cluster = 'State')


summary(did_cluster2)

# compared to 
summary(did1)

# visualization of difference in SEs
coefplot(list(did1,did_cluster))

coefplot(list(did1,did_cluster), keep = 'treated_ind:Treat_ind')


# output as a table
etable(did_cluster)


# Testing assumptions of a DiD design: parallel trends and no anticipation

# parallel trends and event studies

event_study <- feols(Homicides ~ factor(Year) + i(Year, Treat_ind) + Poppies + MajCity + Population, data = panel)

summary(event_study)

coefplot(event_study, keep = 'Year')


# staggered treatment

# make a staggered indicator of treatment

# load another dataset (county-level teen employment rates from 2003-2007)
# from did package
data(mpdta)

# look at data
head(mpdta)

# estimate group-time average treatment effects using att_gt method (did package from Sant'Anna and Callway (2020))
stag_attgt <- att_gt(yname = "lemp",
                     gname = "first.treat",
                     idname = "countyreal",
                     tname = "year",
                     xformla = ~1,
                     data = mpdta,
)

# summarize the results
summary(stag_attgt)

# plot the results
# set ylim so that all plots have the same scale along y-axis
ggdid(stag_attgt, ylim = c(-.3,.3))

# dynamic effects
stag_dynamic <- aggte(stag_attgt, type = "dynamic")
summary(stag_dynamic)

ggdid(stag_dynamic, ylim = c(-.3,.3))


# there are other ways of estimating these effects






## RDDs

# key assumption: groups on either side of the running variable close to the cutoff point should be similar


# Sharp RDD
# threshold exact separates treatment and control

summary(panel$Population)

cutoff <- mean(na.omit(panel$Population)) + 200000

# simulate a running variable cutoff
panel <- panel %>%
  mutate(rdd_threshold = as.factor(ifelse(Population >= cutoff, 1, 0)),
         Homicides2 = ifelse(Population >= cutoff, Homicides + 100, Homicides))

# regress outcome on a dummy of treatment and population centered around the threshold (and controls)
rdd <- lm(Homicides2 ~ rdd_threshold + I(Population - cutoff) + Poppies + MajCity, data = panel)

summary(rdd)

# visualization
ggplot(data = panel, aes(x = Population, y = Homicides2)) +
  geom_point(aes(color = rdd_threshold)) +
  geom_smooth(method = "lm", se = FALSE) +
  scale_color_brewer(palette = "Accent") +
  geom_vline(xintercept = cutoff, color = "red",
             size = 1, linetype = "dashed") + 
  labs(y = "Homicides",
       x = "Municipal Population")

# this only tells us broad differences between the groups, not close to the cutoff

# modeling different slopes on both sides of the cutoff
rdd2 <- lm(Homicides2 ~ rdd_threshold*I(Population - cutoff) + Poppies + MajCity, data = panel)

summary(rdd2)

# limit sample size to get closer around cutoff

cutoff

panel_small <- panel %>%
  filter(Population >= 225000 & Population <= 300000)

# in selecting a bandsize, there is always a tradeoff between bias and variance
# it is best to run several regressions with varying bandsizes to see how robust the results are, as well as consider theoretical reasons why some bandsizes might be more appropriate than others


dim(panel_small)

rdd3 <- lm(Homicides2 ~ rdd_threshold*I(Population - cutoff) + Poppies + MajCity, data = panel_small)

summary(rdd3)

# visualization
ggrdd <- ggplot(data = panel_small, aes(x = Population, y = Homicides2)) +
  geom_point(aes(color = rdd_threshold)) +
  scale_color_brewer(palette = "Accent") +
  geom_vline(xintercept = cutoff, color = "red",
             size = 1, linetype = "dashed") + 
  labs(y = "Homicides",
       x = "Municipal Population")

ggrdd + stat_smooth(aes(Population, Homicides2, group = rdd_threshold), method = 'lm')



# other estimators

# using RDrobust (Calonic, Cattaneo and Titiunik 2014)

rdr <- rdrobust(y = panel$Homicides2, x = panel$Population, c = cutoff) 

summary(rdr)

rdplot(y = panel$Homicides2, x = panel$Population, c = cutoff)


# Fuzzy RDD
# where a cutoff indicates a probability of being treated (estimated as an Instrumental Variable (outcome is a LATE -- local average treatment effect))

# https://evalf20.classes.andrewheiss.com/example/rdd-fuzzy/

# creation of actual treatment indicator


panel <- panel %>%
  mutate(Population2 = ifelse(Population >= 275000, 1, 0),
         Population2 = ifelse(Population > 225000 & Population < 275000, rbinom(1, 1, 0.5), Population2),
         Population2 = ifelse(Population <= 225000, 0, Population2))


rdr2 <- rdrobust(y = panel$Homicides2, x = panel$Population, c = cutoff, fuzzy = panel$Population2) 

summary(rdr2)




## marginal effects of x on y

# here we can use the 'margins' package

# recall the DiD estimator
did_mod <- lm(Homicides ~ treated_ind*Treat_ind + Population + MajCity + Poppies, data = panel)

m <- margins(did_mod)

summary(m)

margins_summary(did_mod)

plot(m)
