crude_rate_household <- regression_formula("`Fatal Overdose Crude Rate`", "`Median Household Income`", "Year")
reg <- lm(crude_rate_household, df_acs1)
test <- resid(reg)
test <- test ^ 2

# obtain the squared OLS residuals
# replace dependent variable with squared residuals
# squared residuals = estimate of variance of each observation
# looking to see if variance is significantly associated with any variables in underlying model
# heteroskedacity = variance associated with variables
# in an ideal world, there are no significant t-statistics with this
# otherwise, there is heteroskedacity
# take the R^2 from 2nd regression and multiply by number of observations
# that becomes LM statistic
# which is distributed chi^2 with K degrees of freedom
# estimating 22 parameters including constant (K degrees of freedom)
# if R2 * 160 >  then reject null hypothesis of homoskedastic errors
# test is valid under the null (null = absence of pathology)
# reject the null => accept the pathology
# 
# cluster SE at level of county (look into how to do with R)
#   -> subset of robust SE
#   -> controls for some counties having more predictable overdose deaths
# 
# with 160 observations and K-DoF of 22