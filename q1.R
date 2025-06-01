library(rgl)
library(olsrr)
library(car)

ex1 <- read.table(file = "ex1.txt", header = TRUE)

full_model <- lm(Sales ~ Age + HS + Income + Black + Female + Price, data = ex1)
summary(full_model)

residualPlots(full_model) # check for linearity
ncvTest(full_model) # check for heteroscedasticity
qqPlot(full_model) # check for normality
shapiro.test(full_model$residuals) # check for normality
ols_plot_resid_stand(full_model) # check for independence
vif(full_model) # check for multicollinearity

# check for outliers
ols_plot_cooksd_bar(full_model)
influencePlot(full_model)
influenceIndexPlot(full_model)

# remove outliers
ex1_clean <- ex1[-c(2, 9, 29, 30), ]
full_model_clean <- lm(Sales ~ Age + HS + Income + Black + Female + Price, data = ex1_clean)
summary(full_model_clean)
shapiro.test(full_model_clean$residuals) # now check for normality

# check for outliers
ols_plot_resid_stand(full_model_clean)
ols_plot_cooksd_bar(full_model_clean)
influencePlot(full_model_clean)
influenceIndexPlot(full_model_clean)

