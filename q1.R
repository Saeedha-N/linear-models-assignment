library(rgl)
library(olsrr)
library(car)

ex1 <- read.table(file = "ex1.txt", header = TRUE)

full_model <- lm(Sales ~ Age + HS + Income + Black + Female + Price, data = ex1)
summary(full_model)

ncvTest(full_model) # check for homoscedasticity
qqPlot(full_model) # check for normality
shapiro.test(full_model$residuals) # check for normality
ols_plot_resid_stand(full_model) # check for independence
vif(full_model) # check for multicollinearity
residualPlots(full_model) # check for linearity and outliers

# check for influential points
ols_plot_cooksd_bar(full_model)
influencePlot(full_model)
influenceIndexPlot(full_model)

# remove outliers/influential points
ex1_clean <- ex1[-c(2, 9, 29, 30), ]
full_model_clean <- lm(Sales ~ Age + HS + Income + Black + Female + Price, data = ex1_clean)
summary(full_model_clean)
shapiro.test(full_model_clean$residuals) # now check for normality

# check for outliers/influential points
ols_plot_resid_stand(full_model_clean)
ols_plot_cooksd_bar(full_model_clean)
influencePlot(full_model_clean)
influenceIndexPlot(full_model_clean)

# (a) 
model_no_female_clean <- lm(Sales ~ Age + HS + Income + Black + Price, data = ex1_clean)
anova(model_no_female_clean, full_model_clean)

# (b)
model_no_female_hs_clean <- lm(Sales ~ Age + Income + Black + Price, data = ex1_clean)
anova(model_no_female_hs_clean, full_model_clean)

# (c)
confint(full_model_clean, "Income", level = 0.95)

# (d)
model_no_income_clean <- lm(Sales ~ Age + HS + Black + Female + Price, data = ex1_clean)
summary(model_no_income_clean)

# (e)
model_price_age_income_clean <- lm(Sales ~ Price + Age + Income, data = ex1_clean)
summary(model_price_age_income_clean) 

# (f)
model_income_only_clean <- lm(Sales ~ Income, data = ex1_clean)
summary(model_income_only_clean)



