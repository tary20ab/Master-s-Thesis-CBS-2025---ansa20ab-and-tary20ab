# ============================================
#              Preparing RScript
# ============================================

#Clearing console
cat("\014")

#Clearing environment
rm(list=ls())

#Clearing figures/ graphics 
graphics.off()

# Current working directory
getwd()

# Setting working directory
setwd('C:/Users/Talem/OneDrive - CBS - Copenhagen Business School/Master') ## my pc
#setwd('C:/Users/tary20ab/OneDrive - CBS - Copenhagen Business School/Master') ## CBS pc

# Confirming working directory
getwd()

# Packages to install
install.packages("DescTools")
install.packages("Rmisc")
install.packages("ggplot2")
install.packages("zoo")
install.packages("dplyr")
install.packages("carData")
install.packages("quantmod")
install.packages("dynlm")
install.packages("vars")
install.packages("forecast")
install.packages("aTSA")
install.packages("rstatix")
install.packages("tidyverse")
install.packages("fGarch")
install.packages("Quandl")
install.packages("estimatr")
install.packages("AER")
install.packages("readxl")
install.packages("moments")

# Downloading libraries 
library(MASS)
library(DescTools)
library(Rmisc)
library(readr)
library(stargazer)
library(ggplot2)
library(haven)
library(lmtest)
library(dplyr)
library(car)
library(sandwich)
library(survival)
library(AER)
library(quantmod)
library(dynlm)
library(vars)
library(forecast)
library(strucchange)
library(xts)
library(urca)
library(aTSA)
library(rstatix)
library(tidyverse)
library(fGarch)
library(Quandl)
library(estimatr)
library(readxl) 
library(lubridate)
library(tidyr)
library(scales)
library(moments)

# ============================================
#           Loading from Excel
# ============================================

# Load indexes from excel
green_index <- read_excel("C:/Users/Talem/OneDrive - CBS - Copenhagen Business School/Master/HY Green Bond index.xlsx") ## my pc
# green_index <- read_excel("C:/Users/tary20ab/OneDrive - CBS - Copenhagen Business School/Master/HY Green Bond index.xlsx") ## cbs pc
View(green_index)
head(green_index)

nbp_index <- read_excel("C:/Users/Talem/OneDrive - CBS - Copenhagen Business School/Master/Nordic HY Index.xlsx") ## my pc
# nbp_index <- read_excel("C:/Users/tary20ab/OneDrive - CBS - Copenhagen Business School/Master/Nordic HY Index.xlsx") ## cbs pc
View(nbp_index)
head(nbp_index)

# Load risk-free rate from excel
rf <- read_excel("C:/Users/Talem/OneDrive - CBS - Copenhagen Business School/Master/rf.xlsx") ## my pc
# rf <- read_excel("C:/Users/tary20ab/OneDrive - CBS - Copenhagen Business School/Master/rf.xlsx") ## cbs pc
View(rf)
head(rf)

# Load VIX data
VIX <- read_excel("C:/Users/Talem/OneDrive - CBS - Copenhagen Business School/Master/VIX.xlsx")
# VIX <- read_excel("C:/Users/tary20ab/OneDrive - CBS - Copenhagen Business School/Master/VIX.xlsx") # CBS PC
VIX$PriceDate <- as.Date(VIX$PriceDate) # Make sure dates are in Date format
VIX <- VIX %>%
  arrange(PriceDate) # Sort by date
View(VIX)
head(VIX)

# Increases the threshold for scientific notation
options(scipen = 999)  


# ============================================
#       Combining Daily Total Returns
# ============================================

# Create a combined dataset for descriptive statistics table
combined_returns <- data.frame(
  Date = green_index$PriceDate,
  Green = green_index$DailyTotalReturn,
  NBP = nbp_index$DailyTotalReturn
)

View(combined_returns)

# ===========================================
#              RISK-FREE RATE
# ===========================================

# Make sure date is in Date format
rf$Date <- as.Date(rf$'Time Period')

# Transform to decimal rate and keep only necessary columns
rf <- rf %>%
  mutate(rf_decimal = Rate / 100) %>%   # f.eks. 4.35% → 0.0435
  select(Date, rf_decimal)

# Merge daily risk-free rate with combined_returns
combined_returns <- left_join(combined_returns, rf, by = "Date")

# Check result
view(combined_returns)


# ===========================================
#           DEFINING SUB-PERIODS
# ===========================================

# Define period 1 and 2 using daily data
period1_daily <- combined_returns %>%
  filter(Date >= as.Date("2021-12-31") & Date <= as.Date("2023-12-29"))

period2_daily <- combined_returns %>%
  filter(Date >= as.Date("2024-01-02") & Date <= as.Date("2025-03-10"))

# View period 1 and 2 datasets
view(period1_daily)
view(period2_daily)


# ===========================================
#              Relative Returns
# ===========================================

# Full Period - Relative Returns
relative_returns_full <- data.frame(
  Date = combined_returns$Date,
  RelativeReturn = combined_returns$Green - combined_returns$NBP
)

# Period 1 - Relative Returns
relative_returns_p1 <- data.frame(
  Date = period1_daily$Date,
  RelativeReturn = period1_daily$Green - period1_daily$NBP
)

# Period 2 - Relative Returns
relative_returns_p2 <- data.frame(
  Date = period2_daily$Date,
  RelativeReturn = period2_daily$Green - period2_daily$NBP
)

# ================================================
#    DESCRIPTIVE STATISTICS - Relative Returns
# ================================================

# ------------ Full period ----------------

# Calculate descriptive statistics for each index (daily)
mean_relative <- mean(relative_returns_full$RelativeReturn, na.rm = TRUE)
median_relative <- median(relative_returns_full$RelativeReturn, na.rm = TRUE)
sd_relative <- sd(relative_returns_full$RelativeReturn, na.rm = TRUE)
n_obs <- nrow(relative_returns_full) 

# Create summary table 
summary_table_relative_returns <- data.frame(
  Statistic = c("Nr of Obs", "Mean (%)", "Median (%)", "Std. Dev. (%)"),
  `Relative Return` = round(c(
    n_obs,
    mean_relative * 100,
    median_relative * 100,
    sd_relative * 100
  ), 6)
)

# Present with Stargazer
stargazer(summary_table_relative_returns, type = "text",
          title = "Summary Statistics Full Period - Daily Relative Returns (%)",
          digits = 2,
          summary = FALSE)


# ------------ Period 1 ----------------

mean_relative_p1   <- mean(relative_returns_p1$RelativeReturn, na.rm = TRUE)
median_relative_p1 <- median(relative_returns_p1$RelativeReturn, na.rm = TRUE)
sd_relative_p1     <- sd(relative_returns_p1$RelativeReturn, na.rm = TRUE)
n_obs_p1           <- nrow(relative_returns_p1)

summary_table_relative_returns_p1 <- data.frame(
  Statistic = c("Nr of Obs", "Mean (%)", "Median (%)", "Std. Dev. (%)"),
  `Relative Return` = round(c(
    n_obs_p1,
    mean_relative_p1 * 100,
    median_relative_p1 * 100,
    sd_relative_p1 * 100
  ), 6)
)

stargazer(summary_table_relative_returns_p1, type = "text",
          title = "Summary Statistics Period 1 - Daily Relative Returns (%)",
          digits = 2,
          summary = FALSE)


# ------------ Period 2 ----------------

mean_relative_p2   <- mean(relative_returns_p2$RelativeReturn, na.rm = TRUE)
median_relative_p2 <- median(relative_returns_p2$RelativeReturn, na.rm = TRUE)
sd_relative_p2     <- sd(relative_returns_p2$RelativeReturn, na.rm = TRUE)
n_obs_p2           <- nrow(relative_returns_p2)

summary_table_relative_returns_p2 <- data.frame(
  Statistic = c("Nr of Obs", "Mean (%)", "Median (%)", "Std. Dev. (%)"),
  `Relative Return` = round(c(
    n_obs_p2,
    mean_relative_p2 * 100,
    median_relative_p2 * 100,
    sd_relative_p2 * 100
  ), 6)
)

stargazer(summary_table_relative_returns_p2, type = "text",
          title = "Summary Statistics Period 2 - Daily Relative Returns (%)",
          digits = 2,
          summary = FALSE)


# ================================================
#   FULL DESCRIPTIVE ANALYSIS - Relative Return
# ================================================

# Create summary table
combined_summary <- data.frame(
  Statistic = c("Nr of Obs", "Mean (%)", "Std. Dev. (%)"),
  
  `Period 1` = round(c(
    n_obs_p1,
    mean_relative_p1 * 100,
    sd_relative_p1 * 100
  ), 6),
  
  `Period 2` = round(c(
    n_obs_p2,
    mean_relative_p2 * 100,
    sd_relative_p2 * 100
  ), 6),
  
  `Full Period` = round(c(
    n_obs,
    mean_relative * 100,
    sd_relative * 100
  ), 6)
)

# Present with stargazer
stargazer(combined_summary, type = "text", summary = FALSE,
          title = "Summary Statistics of Daily Relative Returns by Period",
          digits = 4)

# Annualized daily relative return stats for easier interpretation
annualize_mean <- function(x) x * 252
annualize_sd   <- function(x) x * sqrt(252)

combined_summary_annualized <- data.frame(
  Statistic = c("Nr of Obs", "Mean (%)", "Std. Dev. (%)"),
  
  `Period 1` = round(c(
    n_obs_p1,
    annualize_mean(mean_relative_p1) * 100,
    annualize_sd(sd_relative_p1) * 100
  ), 4),
  
  `Period 2` = round(c(
    n_obs_p2,
    annualize_mean(mean_relative_p2) * 100,
    annualize_sd(sd_relative_p2) * 100
  ), 4),
  
  `Full Period` = round(c(
    n_obs,
    annualize_mean(mean_relative) * 100,
    annualize_sd(sd_relative) * 100
  ), 4)
)

stargazer(combined_summary_annualized, type = "text", summary = FALSE,
          title = "Annualized Statistics of Daily Relative Returns by Period",
          digits = 4)


# ================================================
#       Create dataframe with all variables
# ================================================

# View dataframe
view(relative_returns_full)

# -------- Add VIX and Rf to relative returns --------

# Date formats
VIX$Date <- as.Date(VIX$PriceDate)

# Create new dataframe
rf <- combined_returns %>%
  select(Date, rf_decimal)

# Combine to one dataframe
regression_data <- relative_returns_full %>%
  left_join(VIX[, c("Date", "VIX")], by = "Date") %>%
  left_join(rf, by = "Date")

# Remove NA-values
regression_data <- regression_data[-1, ]
regression_data <- regression_data %>%
  na.omit()

view(regression_data)

# ============================================
#          Check for Stationarity
# ============================================

# ------------- Daily Relative Returns ----------------
regression_data$Date <- as.Date(regression_data$Date)

# RELATIVE RETURNS 
regression_data$RelativeReturn

# Plot of Time Series
ggplot(regression_data, aes(x = Date, y = RelativeReturn)) +
  geom_line(size = 0.7) +
  geom_hline(yintercept = 0, color = "gray50", linetype = "dashed", size = 0.4) +
  labs(
    title = "Development in Relative Return: Green - NBP",
    x = "Date",
    y = "Relative Return"
  ) +
  scale_y_continuous(
    labels = percent_format(accuracy = 0.1)
  ) +
  scale_x_date(
    date_breaks = "3 months",
    date_labels = "%b %Y",
    limits = c(min(regression_data$Date), as.Date("2025-03-31")),
    expand = c(0, 0)
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 12, face = "bold", hjust = 0.5, margin = margin(b = 10)),
    axis.title = element_text(size = 14),
    axis.text.x = element_text(size = 12, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 12),
    panel.grid.major = element_line(color = "grey85", size = 0.3),
    panel.grid.minor = element_blank()
  )

# ACF plot
acf(regression_data$RelativeReturn)

# ADF test
summary(ur.df(regression_data$RelativeReturn, type='drift', selectlags="AIC"))
# Since the t-statistic is larger than the critical value, we are able to reject the null and there is evidence of stationarity in the time series. 

## Can be proven stationary in its original form 


# ------------- VIX ----------------

# VIX
regression_data$VIX

# ADF test
summary(ur.df(regression_data$VIX, type='drift', selectlags="AIC"))
# Since the t-statistic is larger than the ciritical values, we are able to reject the null and there is evidence of stationarity in the time series. 

## Can be proven stationary in its original form 


# ------------- rf ----------------

# rf
regression_data$rf_decimal

# ADF test
summary(ur.df(regression_data$rf_decimal, type='drift', selectlags="AIC"))

# Plot of Time Series
ts.plot(regression_data$rf_decimal, main = "Development in rf")

# ACF plot
acf(regression_data$rf_decimal)

## Can NOT be proven stationary in its original form 

# Taking the first difference: 
regression_data <- regression_data %>%
  arrange(Date) %>%
  mutate(d_rf = c(NA, diff(rf_decimal)))

view(regression_data)

regression_data <- regression_data[-1, ]

# ADF-test on differentiated rf
summary(ur.df(regression_data$d_rf, type='drift', selectlags="AIC"))

# Can be proven stationary after taking the first difference

# ============================================
#  OLS Regression (daily) - Relative Returns
# ============================================

# For easier interpretation when regressing: 
regression_data <- regression_data %>%
  mutate(d_rf_bp = d_rf * 10000)  

regression_data <- regression_data %>%
  mutate(RelativeReturn_bp = RelativeReturn * 10000)

view(regression_data)

# ADF test for stationarity on variables
summary(ur.df(regression_data$RelativeReturn_bp, type = "drift", selectlags = "AIC")) #Stationarity confirmed
summary(ur.df(regression_data$VIX, type = "drift", selectlags = "AIC")) #Stationarity confirmed
summary(ur.df(regression_data$d_rf_bp, type = "drift", selectlags = "AIC")) #Stationarity confirmed

# Stationarity is confirmed for all variables as the test statistics are below the critical value


# ----------- Full Period ------------

# Run the OLS regression 
relativereturn_reg <- lm(RelativeReturn_bp ~ VIX + d_rf_bp, data = regression_data)

# Hent ut Newey-West standard errors
nw_se_relativereturn <- sqrt(diag(NeweyWest(relativereturn_reg)))

# Print with robust SE using stargazer
stargazer(relativereturn_reg,
          type = "text",
          se = list(nw_se_relativereturn),
          title = "Regression Output: Relative Return Green vs NBP",
          notes = "Robust standard errors are Newey-West (1987) adjusted",
          digits = 4)

# ----------- Sub-Periods ------------

# Filter data for sub-periods
reg_data_p1 <- regression_data %>% filter(Date >= as.Date("2021-12-31") & Date <= as.Date("2023-12-29"))
reg_data_p2 <- regression_data %>% filter(Date >= as.Date("2024-01-02") & Date <= as.Date("2025-03-10"))

# Regressions for sub-periods
reg_p1 <- lm(RelativeReturn_bp ~ VIX + d_rf_bp, data = reg_data_p1)
reg_p2 <- lm(RelativeReturn_bp ~ VIX + d_rf_bp, data = reg_data_p2)

# Newey-West standard errors
nw_se_p1 <- sqrt(diag(NeweyWest(reg_p1)))
nw_se_p2 <- sqrt(diag(NeweyWest(reg_p2)))

# Vis med Stargazer
stargazer(reg_p1, reg_p2, 
          type = "text",
          se = list(nw_se_p1, nw_se_p2),
          column.labels = c("Period 1", "Period 2"),
          title = "Regression: Daily Relative Return in sub-periods",
          notes = "Robust standard errors are Newey-West (1987)-adjusted",
          digits = 4)

# ----------- Present regression-results for all periods ------------

stargazer(reg_p1, reg_p2, relativereturn_reg,
          type = "text",
          se = list(nw_se_p1, nw_se_p2, nw_se_relativereturn),
          column.labels = c("Period 1", "Period 2", "Full Period"),
          title = "Regression: Daily Relative Return in sub-periods",
          notes = "Robust standard errors are Newey-West (1987)-adjusted",
          digits = 4)


# ============================================
#          Testing OLS-assumptions
# ============================================

# Autocorrelation
dwtest(relativereturn_reg) #OK
dwtest(reg_p1) #OK
dwtest(reg_p2) #OK

# Heteroscedasticity
bptest(relativereturn_reg)  #OK
bptest(reg_p1)  #OK
bptest(reg_p2)  #OK

# Test for multicollinearity
vif(relativereturn_reg)   #OK
vif(reg_p1)   #OK
vif(reg_p2)   #OK

# Normality in residuals
shapiro.test(resid(relativereturn_reg))  # No
shapiro.test(resid(reg_p1))  # No
shapiro.test(resid(reg_p2))  # No

# Residual plot
# Full period
plot(fitted(relativereturn_reg), resid(relativereturn_reg),
     main = "Residuals vs Fitted: Full Period",
     xlab = "Fitted Values", ylab = "Residuals")
abline(h = 0, col = "forestgreen")

# Period 1
plot(fitted(reg_p1), resid(reg_p1),
     main = "Residuals vs Fitted: Period 1",
     xlab = "Fitted Values", ylab = "Residuals")
abline(h = 0, col = "forestgreen")

# Period 2
plot(fitted(reg_p2), resid(reg_p2),
     main = "Residuals vs Fitted: Period 2",
     xlab = "Fitted Values", ylab = "Residuals")
abline(h = 0, col = "forestgreen")


acf(resid(relativereturn_reg))
pacf(resid(relativereturn_reg))
acf(resid(reg_p1))
pacf(resid(reg_p1))
acf(resid(reg_p2))
pacf(resid(reg_p2))

# Q-Q plot
qqnorm(rstandard(relativereturn_reg), main = "Normal Q-Q Plot of Relative Returns (full sample)")
qqline(rstandard(relativereturn_reg), col = "forestgreen", lwd = 2)
qqnorm(rstandard(reg_p1), main = "Normal Q-Q Plot of Relative Returns (period 1)")
qqline(rstandard(reg_p1), col = "forestgreen", lwd = 2)
qqnorm(rstandard(reg_p2), main = "Normal Q-Q Plot of Relative Returns (period 2)")
qqline(rstandard(reg_p2), col = "forestgreen", lwd = 2)


########### MONTHLY ###########

# ============================================
#         Monthly Returns and Rf
# ============================================

# Aggregate to monthly returns
monthly_returns <- combined_returns %>%
  mutate(YearMonth = floor_date(Date, "month")) %>%
  group_by(YearMonth) %>%
  summarise(
    Green = prod(1 + Green, na.rm = TRUE) - 1,
    NBP = prod(1 + NBP, na.rm = TRUE) - 1,
    rf_monthly = prod(1 + rf_decimal, na.rm = TRUE) - 1,
    .groups = "drop"
  )

# Calculate monthly relative return
monthly_returns <- monthly_returns %>%
  mutate(RelativeReturn = Green - NBP)

# View results
View(monthly_returns)

# ============================================
#       Independent Variables (monthly)
# ============================================

# Create month-column
regression_data <- regression_data %>%
  mutate(YearMonth = floor_date(Date, unit = "month"))

# Aggregate daily data to monthly
regression_data_monthly <- regression_data %>%
  group_by(YearMonth) %>%
  summarise(
    d_rf_bp                = mean(d_rf_bp, na.rm = TRUE),
    VIX                    = mean(VIX, na.rm = TRUE),
    RelativeReturn_bp      = mean(RelativeReturn_bp, na.rm = TRUE)
  )

view(regression_data_monthly)

# ============================================
#         Check for Stationarity
# ============================================

# ------------- Monthly Relative Returns ----------------

# RELATIVE RETURNS 
regression_data_monthly$RelativeReturn_bp

# Plot of Time Series
ts.plot(regression_data_monthly$RelativeReturn_bp, main = "Development in Relative Returns (Green - NBP)")

# ACF plot
acf(regression_data_monthly$RelativeReturn_bp)

# ADF test
summary(ur.df(regression_data_monthly$RelativeReturn_bp, type='drift', selectlags="AIC"))
# Since the t-statistic is larger than the ciritical values, we are able to reject the null and there is evidence of stationarity in the time series. 

# ------------- VIX ----------------

# RELATIVE RETURNS 
regression_data_monthly$VIX

# Plot of Time Series
ts.plot(regression_data_monthly$VIX, main = "Development in VIX")

# ACF plot
acf(regression_data_monthly$VIX)

# ADF test
summary(ur.df(regression_data_monthly$VIX, type='drift', selectlags="AIC"))

# NOT STATIONARY

# ------------- rf ----------------

# RELATIVE RETURNS 
regression_data_monthly$d_rf_bp

# Plot of Time Series
ts.plot(regression_data_monthly$d_rf_bp, main = "Development in rf")

# ACF plot
acf(regression_data_monthly$d_rf_bp)

# ADF test
summary(ur.df(regression_data_monthly$d_rf_bp, type='drift', selectlags="AIC"))

# NOT STATIONARY

# ============================================
#  OLS Regression (monthly) - Relative Returns
# ============================================

# ADF test for stationarity on variables
summary(ur.df(regression_data_monthly$RelativeReturn_bp, type = "drift", selectlags = "AIC")) #Stationarity confirmed
summary(ur.df(regression_data_monthly$VIX, type = "drift", selectlags = "AIC")) #Stationarity NOT confirmed
summary(ur.df(regression_data_monthly$d_rf_bp, type = "drift", selectlags = "AIC")) #Stationarity NOT confirmed
# A limitation with the regression will be that VIX and d_rf are not stationary variables

# ----------- Full Period ------------

# Run the OLS regression 
relativereturn_reg_monthly <- lm(RelativeReturn_bp ~ VIX + d_rf_bp, data = regression_data_monthly)

# Extract Newey-West standard errors
nw_se_relativereturn_monthly <- sqrt(diag(NeweyWest(relativereturn_reg_monthly)))

# Print with robust SE using stargazer
stargazer(relativereturn_reg_monthly,
          type = "text",
          se = list(nw_se_relativereturn_monthly),
          title = "Regression Output: Relative Return Green vs NBP",
          notes = "Robust standard errors are Newey-West (1987) adjusted",
          digits = 4)


# Autocorrelation
dwtest(relativereturn_reg_monthly) #OK

# Test for multicollinearity
vif(relativereturn_reg_monthly)   #OK

# Heteroscedasticity
bptest(relativereturn_reg_monthly)  #OK

# Normality in residuals
shapiro.test(resid(relativereturn_reg_monthly))  # OK

# Residual plot
plot(fitted(relativereturn_reg_monthly), resid(relativereturn_reg_monthly))
abline(h = 0, col = "forestgreen")

acf(resid(relativereturn_reg_monthly))
pacf(resid(relativereturn_reg_monthly))

# Q-Q plot
qqnorm(rstandard(relativereturn_reg_monthly), main = "Normal Q-Q Plot of Relative Returns")
qqline(rstandard(relativereturn_reg_monthly), col = "forestgreen", lwd = 2)

# ----------- Sub-Periods ------------

# Sub-periods (monthly)
reg_data_monthly_p1 <- regression_data_monthly %>%
  filter(YearMonth >= as.Date("2022-01-01") & YearMonth <= as.Date("2023-12-31"))

reg_data_monthly_p2 <- regression_data_monthly %>%
  filter(YearMonth >= as.Date("2024-01-01") & YearMonth <= as.Date("2025-03-01"))

# Regressjoner (monthly)
reg_monthly_p1 <- lm(RelativeReturn_bp ~ VIX + d_rf_bp, data = reg_data_monthly_p1)
reg_monthly_p2 <- lm(RelativeReturn_bp ~ VIX + d_rf_bp, data = reg_data_monthly_p2)

# Newey-West standard errors
nw_se_monthly_p1 <- sqrt(diag(NeweyWest(reg_monthly_p1)))
nw_se_monthly_p2 <- sqrt(diag(NeweyWest(reg_monthly_p2)))

# Show with Stargazer
stargazer(reg_monthly_p1, reg_monthly_p2,
          type = "text",
          se = list(nw_se_monthly_p1, nw_se_monthly_p2),
          column.labels = c("Period 1", "Period 2"),
          title = "Regression: Monthly Relative Return in Sub-Periods",
          notes = "Robuste standard errors are Newey-West (1987)-adjusted",
          digits = 4)


# ----------- Sub-Periods ------------

stargazer(reg_monthly_p1, reg_monthly_p2, relativereturn_reg_monthly,
          type = "text",
          se = list(nw_se_monthly_p1, nw_se_monthly_p2, nw_se_relativereturn_monthly),
          column.labels = c("Period 1", "Period 2", "Full Period"),
          title = "OLS Regression: Monthly Relative Return (Green vs. NBP)",
          notes = "Robust standard errors are Newey-West (1987) adjusted",
          digits = 4)


########################################################################################################################################################

# ============================================
#         T-Test on Relative Returns 
# ============================================

# ----------- Defining sub-periods -----------
# Full period
monthly_returns$RelativeReturn

# sub-periods
monthly_returns_p1 <- monthly_returns %>%
  filter(YearMonth >= as.Date("2021-12-31") & YearMonth <= as.Date("2023-12-31"))

monthly_returns_p2 <- monthly_returns %>%
  filter(YearMonth >= as.Date("2024-01-01") & YearMonth <= as.Date("2025-03-01"))

# ----------- Testing if t-test assumptions hold -----------

# Shapiro-Wilk tests
shapiro_p1 <- shapiro.test(monthly_returns_p1$RelativeReturn)
shapiro_p2 <- shapiro.test(monthly_returns_p2$RelativeReturn)
shapiro_full <- shapiro.test(monthly_returns$RelativeReturn)

# Present
shapiro_table <- data.frame(
  Statistic = c("W-statistic", "p-value"),
  `Period 1 (2022–2023)` = c(
    round(shapiro_p1$statistic, 4),
    round(shapiro_p1$p.value, 4)
  ),
  `Period 2 (2024–2025)` = c(
    round(shapiro_p2$statistic, 4),
    round(shapiro_p2$p.value, 4)
  ),
  `Full Period` = c(
    round(shapiro_full$statistic, 4),
    round(shapiro_full$p.value, 4)
  )
)

stargazer(shapiro_table, type = "text", summary = FALSE,
          title = "Shapiro-Wilk Normality Test Results",
          digits = 4)


# Q-Q plot
qqnorm(monthly_returns$RelativeReturn); qqline(monthly_returns$RelativeReturn, col = "forestgreen")

# Assumptions hold: we can perform the T-tests

# ----------- Performing T-test for all periods -----------

t_test_p1 <- t.test(monthly_returns_p1$RelativeReturn, mu = 0)
t_test_p2 <- t.test(monthly_returns_p2$RelativeReturn, mu = 0)
t_test_full <- t.test(monthly_returns$RelativeReturn, mu = 0)


# create table
t_test_table <- data.frame(
  Statistic = c("N", "Mean", "t-statistic", "p-value", "95% CI lower", "95% CI upper"),
  `Period 1 (2022–2023)` = c(
    length(monthly_returns_p1$RelativeReturn),
    round(mean(monthly_returns_p1$RelativeReturn), 4),
    round(t_test_p1$statistic, 4),
    round(t_test_p1$p.value, 4),
    round(t_test_p1$conf.int[1], 4),
    round(t_test_p1$conf.int[2], 4)
  ),
  `Period 2 (2024–2025)` = c(
    length(monthly_returns_p2$RelativeReturn),
    round(mean(monthly_returns_p2$RelativeReturn), 4),
    round(t_test_p2$statistic, 4),
    round(t_test_p2$p.value, 4),
    round(t_test_p2$conf.int[1], 4),
    round(t_test_p2$conf.int[2], 4)
  ),
  `Full Period` = c(
    length(monthly_returns$RelativeReturn),
    round(mean(monthly_returns$RelativeReturn), 4),
    round(t_test_full$statistic, 4),
    round(t_test_full$p.value, 4),
    round(t_test_full$conf.int[1], 4),
    round(t_test_full$conf.int[2], 4)
  )
)


stargazer(t_test_table, type = "text", summary = FALSE,
          title = "Two-Sample T-Test: Monthly Relative Returns",
          digits = 4)


