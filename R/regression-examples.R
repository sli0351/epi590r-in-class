library(tidyverse)
library(gtsummary)

# load and clean data
nlsy_cols <- c(
  "glasses", "eyesight", "sleep_wkdy", "sleep_wknd",
  "id", "nsibs", "samp", "race_eth", "sex", "region",
  "income", "res_1980", "res_2002", "age_bir"
)
nlsy <- read_csv(here::here("data", "raw", "nlsy.csv"),
  na = c("-1", "-2", "-3", "-4", "-5", "-998"),
  skip = 1, col_names = nlsy_cols
) |>
  mutate(
    region_cat = factor(region, labels = c("Northeast", "North Central", "South", "West")),
    sex_cat = factor(sex, labels = c("Male", "Female")),
    race_eth_cat = factor(race_eth, labels = c("Hispanic", "Black", "Non-Black, Non-Hispanic")),
    eyesight_cat = factor(eyesight, labels = c("Excellent", "Very good", "Good", "Fair", "Poor")),
    glasses_cat = factor(glasses, labels = c("No", "Yes"))
  )


# Univariate regression
# this funciton is BOTH running the regressions and creating the table

# regression of income on a series of predictor (x) variables
tbl_uvregression(
  nlsy,
  y = income,
  include = c(
    sex_cat, race_eth_cat,
    eyesight_cat, income, age_bir
  ),
  method = lm
)
## this is equivalent to
# lm(income ~ sex_cat, data = nlsy)
# lm(income ~ race_eth_cat, data = nlsy)
# lm(income ~ eyesight_cat, data = nlsy)
# lm(income ~ income, data = nlsy)
# lm(income ~ age_bir, data = nlsy)


# regression of glasses on a series of predictor (x) variables
# using a logistic regression model
tbl_uvregression(
  nlsy,
  y = glasses,
  include = c(
    sex_cat, race_eth_cat,
    eyesight_cat, glasses, age_bir
  ),
  method = glm,
  method.args = list(family = binomial()),
  exponentiate = TRUE
)


## Multivariable regressions
# now we need to fit the models first

# linear model
linear_model <- lm(income ~ sex_cat + age_bir + race_eth_cat,
  data = nlsy
)

# linear model with interaction
linear_model_int <- lm(income ~ sex_cat * age_bir + race_eth_cat,
  data = nlsy
)

# logistic model
logistic_model <- glm(glasses ~ eyesight_cat + sex_cat,
  data = nlsy, family = binomial()
)


## Tables
# we use the models we just fit to create the tables
tbl_regression(
  linear_model,
  # include the intercept
  intercept = TRUE,
  # relabel the variables
  label = list(
    sex_cat ~ "Sex",
    race_eth_cat ~ "Race/ethnicity",
    age_bir ~ "Age at first birth"
  )
)


tbl_regression(
  logistic_model,
  exponentiate = TRUE,
  label = list(
    sex_cat ~ "Sex",
    eyesight_cat ~ "Eyesight"
  )
)


# in order to compare the models with and without interaction
# we need to create and store the tables first

# table for the model without interaction
tbl_no_int <- tbl_regression(
  linear_model,
  intercept = TRUE,
  label = list(
    sex_cat ~ "Sex",
    race_eth_cat ~ "Race/ethnicity",
    age_bir ~ "Age at first birth"
  )
)

# table for the model with interaction
tbl_int <- tbl_regression(
  linear_model_int,
  intercept = TRUE,
  label = list(
    sex_cat ~ "Sex",
    race_eth_cat ~ "Race/ethnicity",
    age_bir ~ "Age at first birth",
    `sex_cat:age_bir` ~ "Sex/age interaction"
  )
)

## Table comparing the models with and without interaction
tbl_merge(list(tbl_no_int, tbl_int),
  tab_spanner = c("**Model 1**", "**Model 2**")
)


# exercise 3
# Each of the univariate regression examples held the outcome (y =) constant, while varying the predictor variables with include =. You can also look at one predictor across several outcomes. Create a univariate regression table looking at the association between sex (sex_cat) as the x = variable and each of nsibs, sleep_wkdy, and sleep_wknd, and income.

tbl_uvregression(
	nlsy,
	x = sex_cat,
	include = c(
		nsibs, sleep_wkdy,
		sleep_wknd, income
	),
	method = lm
)

# 4. Fit a Poisson regression (family = poisson()) for the number of siblings, using at least 3 predictors of your choice. Create a nice table displaying your Poisson regression and its exponentiated coefficients.

# poisson model
poisson_model <- glm(nsibs ~ eyesight_cat + sex_cat + income,
										 data = nlsy, family = poisson()
)

tbl_regression(
	poisson_model,
	exponentiate = TRUE,
	label = list(
		sex_cat ~ "Sex",
		eyesight_cat ~ "Eyesight",
		income ~ "Income"
	)
)

# 5. Instead of odds ratios for wearing glasses, as in the example in the slides., we want risk ratios. We can do this by specifying in the regression family = binomial(link = "log"). Regress glasses on eyesight_cat and sex_cat and create a table showing the risk ratios and confidence intervals from this regression.


# logistic model
log_model <- glm(glasses ~ eyesight_cat + sex_cat,
								 data = nlsy, family = binomial(link = "log")
)

tbl_regression(
	log_model,
	exponentiate = TRUE,
	label = list(
		sex_cat ~ "Sex",
		eyesight_cat ~ "Eyesight"
	)
)

# 6. combine

logistic_table <- tbl_regression(
	logistic_model,
	exponentiate = TRUE,
	label = list(
		sex_cat ~ "Sex",
		eyesight_cat ~ "Eyesight"
	)
)

log_table <- tbl_regression(
	log_model,
	exponentiate = TRUE,
	label = list(
		sex_cat ~ "Sex",
		eyesight_cat ~ "Eyesight"
	)
)

tbl_merge(list(logistic_table, log_table),
					tab_spanner = c("**Logistic regression**", "**Log-linear regression**")
)
