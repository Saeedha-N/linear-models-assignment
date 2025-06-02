library(rgl)
library(olsrr)
library(car)

ex2 <- read.table("ex2.txt", header = TRUE)

# Create dummy variables for the 'Region' categorical variable 
# Only three dummy variables are created because the fourth (West) is the baseline
ex2$R1 <- ifelse(ex2$Region == 1, 1, 0) # Northeast
ex2$R2 <- ifelse(ex2$Region == 2, 1, 0) # North Central
ex2$R3 <- ifelse(ex2$Region == 3, 1, 0) # South

full_model <- lm(Y ~ X1 + X2 + X3 + R1 + R2 + R3, data = ex2)
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
ols_plot_dffits(full_model)
ols_plot_hadi(full_model)

# remove outliers/influential points
ex2_clean <- ex2[-c(22, 30, 40), ]
full_model_clean <- lm(Y ~ X1 + X2 + X3 + R1 + R2 + R3, data = ex2_clean)
summary(full_model_clean)
ncvTest(full_model_clean) # now check for homoscedasticity

# Effects of removing Region on the regression relationship
model_no_region_clean <- lm(Y ~ X1 + X2 + X3, data = ex2_clean)
summary(model_no_region_clean)
anova(model_no_region_clean, full_model_clean)
