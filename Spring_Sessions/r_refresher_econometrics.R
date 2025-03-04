
#### Econometrics in R #####
## Adee Weller and Eddy Yeung


# packages
pkgs <- c("dplyr", "fixest", "stargazer", "ggplot2", "modelsummary", "estimatr", "sandwich", "broom", "lmtest", "lfe", "rdrobust", "AER")

for (pkg in pkgs) {
  if (! (pkg %in% rownames(installed.packages()))) { install.packages(pkg) }
  lapply(pkg, library, character.only=T )
}


# Create Data
set.seed(123)

n <- 1000

years <- 2000:2019

state_id <- 1:50

data <- data.frame(
  year = rep(years, each=50),
  state = rep(state_id, times=20),
  treatment = c(rep(0, 500), rep(c(rep(0, times=10), rep(1, times=20), rep(0, times = 20)), times = 10)),
  outcome = rnorm(n),
  running_var = runif(n, -1, 1), 
  instrument = c(rep(0, 500), rep(c(rep(0, times=8), rep(1, times=18), rep(0, times = 20), rep(1, times=4)), times = 10)),
  covariate = rnorm(n)
)

head(data)

tail(data)

View(data)

# Example 1: Difference-in-Differences (DiD) -------------------------------
# Let's say treatment begins in 2010

# Designate post-treatment indicator
data$treat_post <- with(data, treatment * (year >= 2010))

# Main DiD regression
did_model <- feols(outcome ~ treat_post | year + state, data = data)

summary(did_model)

# Alternative estimator (with clustered standard errors) lfe
did_robust <- felm(outcome ~ treat_post | year + state | 0 | state, data = data)

summary(did_robust)

# Table
footnote_text <- "Significance levels: + p < 0.1, * p < 0.05, ** p < 0.01, *** p < 0.001."

modelsummary(did_robust, 
  estimate = "{estimate}{stars}",
  output = "latex", 
  gof_map = c("nobs", "r.squared"),
  title = "Difference-in-Differences",
  notes = footnote_text
  )

# Event studies and placebo tests to check Parallel Trends Assumption



# Example 2: Sharp Regression Discontinuity (RDD) --------------------------------
# RD requires a sharp cutoff, e.g., running_var = 0

rdd_data <- data %>% filter(abs(running_var) < 0.5)

dim(data)
dim(rdd_data)


rd_model <- feols(outcome ~ treatment + running_var, data = rdd_data)

summary(rd_model)

# Robust standard errors for RDD
coeftest(rd_model, vcov = vcovHC(rd_model, type = "HC1"))

# Alternative RDD estimator: rdrobust
rd_out <- rdrobust(y = data$outcome, x = data$running_var, c = 0)

summary(rd_out)

# Visualization
data %>%
  filter(abs(running_var) < 0.5) %>%
  ggplot(aes(x = running_var, y = outcome)) +
  geom_point() +
  geom_smooth(method = "loess") +
  geom_vline(xintercept = 0) +
  labs(title = "Regression Discontinuity Example", x = "Running Variable", y = "Outcome")




# Example 3: Instrumental Variables (IV) ----------------------------------
# Assume instrument predicts treatment

iv_model <- felm(outcome ~ 1 + covariate | year + state | (treat_post ~ instrument + covariate) | state, data = data)

summary(iv_model)

# Alternative estimator
iv_model_aer <- ivreg(outcome ~ treat_post + covariate | instrument + covariate, data = data)

summary(iv_model_aer)

# Weak instrument diagnostics
summary(iv_model_aer, diagnostics = TRUE)




# Example 4: Survey Data Analysis -----------------------------------------

# Create survey data

survey_data <- data.frame(
  group = sample(c("Control", "Treatment"), 500, replace = TRUE),
  outcome = rnorm(500)
)

# Difference in means
mean_diff <- survey_data %>%
  group_by(group) %>%
  summarise(mean_outcome = mean(outcome), .groups = 'drop')

mean_diff

# Regression on survey data
survey_model <- lm_robust(outcome ~ group, data = survey_data, se_type = "HC3")

summary(survey_model)

survey_model2 <- lm_robust(outcome ~ 1, data = survey_data, subset = group=="Control", se_type = "HC3")

summary(survey_model2)

# Robust standard errors
survey_model_robust <- coeftest(survey_model, vcov = vcovHC(survey_model, type = "HC1"))

survey_model_robust

# Visualization
survey_data %>%
  ggplot(aes(x = group, y = outcome)) +
  geom_boxplot() +
  labs(title = "Survey Analysis: Difference in Means", y = "Outcome")
