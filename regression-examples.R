library(tidyverse)
library(gtsummary)

nlsy_cols <- c("glasses", "eyesight", "sleep_wkdy", "sleep_wknd",
							 "id", "nsibs", "samp", "race_eth", "sex", "region",
							 "income", "res_1980", "res_2002", "age_bir")
nlsy <- read_csv(here::here("data", "raw", "nlsy.csv"),
								 na = c("-1", "-2", "-3", "-4", "-5", "-998"),
								 skip = 1, col_names = nlsy_cols) |>
	mutate(region_cat = factor(region, labels = c("Northeast", "North Central", "South", "West")),
				 sex_cat = factor(sex, labels = c("Male", "Female")),
				 race_eth_cat = factor(race_eth, labels = c("Hispanic", "Black", "Non-Black, Non-Hispanic")),
				 eyesight_cat = factor(eyesight, labels = c("Excellent", "Very good", "Good", "Fair", "Poor")),
				 glasses_cat = factor(glasses, labels = c("No", "Yes")))


# Univariate regression

tbl_uvregression(
	nlsy,
	y = income,
	include = c(sex_cat, race_eth_cat,
							eyesight_cat, income, age_bir),
	method = lm)


tbl_uvregression(
	nlsy,
	y = glasses,
	include = c(sex_cat, race_eth_cat,
							eyesight_cat, glasses, age_bir),
	method = glm,
	method.args = list(family = binomial()),
	exponentiate = TRUE)


## Multivariable regressions

## Some regressions

linear_model <- lm(income ~ sex_cat + age_bir + race_eth_cat,
									 data = nlsy)


linear_model_int <- lm(income ~ sex_cat*age_bir + race_eth_cat,
											 data = nlsy)


logistic_model <- glm(glasses ~ eyesight_cat + sex_cat + income,
											data = nlsy, family = binomial())


## Tables

tbl_regression(
	linear_model,
	intercept = TRUE,
	label = list(
		sex_cat ~ "Sex",
		race_eth_cat ~ "Race/ethnicity",
		age_bir ~ "Age at first birth"
	))


tbl_regression(
	logistic_model,
	exponentiate = TRUE,
	label = list(
		sex_cat ~ "Sex",
		eyesight_cat ~ "Eyesight",
		income ~ "Income"
	))


tbl_no_int <- tbl_regression(
	linear_model,
	intercept = TRUE,
	label = list(
		sex_cat ~ "Sex",
		race_eth_cat ~ "Race/ethnicity",
		age_bir ~ "Age at first birth"
	))

tbl_int <- tbl_regression(
	linear_model_int,
	intercept = TRUE,
	label = list(
		sex_cat ~ "Sex",
		race_eth_cat ~ "Race/ethnicity",
		age_bir ~ "Age at first birth",
		`sex_cat:age_bir` ~ "Sex/age interaction"
	))

## Table comparing the models with and without interaction

tbl_merge(list(tbl_no_int, tbl_int),
					tab_spanner = c("**Model 1**", "**Model 2**"))


###you try exercises:

# 3.)
tbl_uvregression(
	nlsy,
	x = sex_cat,
	include = c(nsibs, starts_with("sleep"), income),
	method = lm)
#or do it this way:
tbl_uvregression(
	nlsy,
	y = nsibs,
	include = sex_cat,
	method = lm)
# do for each variable it is asking for, then do a table merge


# 4.)
poisson_model <- glm(nsibs ~ eyesight_cat + sex_cat + income,
											data = nlsy, family = poisson())
#table:
tbl_regression(
	poisson_model,
	exponentiate = TRUE,
	label = list(
		sex_cat ~ "Sex",
		eyesight_cat ~ "Eyesight",
		income ~ "Income"
	))

# 5.)
tbl_uvregression(
	nlsy,
	y = glasses,
	include = c(sex_cat, eyesight_cat),
	method = glm,
	method.args = list(family = binomial(link = "log")),
	exponentiate = TRUE)

# 6.)
eyes_binomial <- glm(glasses ~ eyesight_cat + sex_cat,
										 family = binomial(link = "log"), data = nlsy)
eyes_poisson <- glm(glasses ~ eyesight_cat + sex_cat,
										 family = poisson(link = "log"), data = nlsy)
tbl_eyes_binomial <- tbl_regression(eyes_binomial,
																		exponentiate = TRUE,
																		tidy_fun = partial(tidy_robust, vcov= "HC1"))
tbl_eyes_poisson <- tbl_regression(eyes_poisson,
																		exponentiate = TRUE,
																		tidy_fun = partial(tidy_robust, vcov= "HC1"))
#then merge tables
tbl_merge(list(tbl_eyes_binomial, tbl_eyes_poisson),
					tab_spanner = c("**binomial**", "**poisson**"))
