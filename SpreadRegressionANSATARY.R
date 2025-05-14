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
library(lubridate)


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

# Load VIX data
VIX <- read_excel("C:/Users/Talem/OneDrive - CBS - Copenhagen Business School/Master/VIX.xlsx") ## my pc
# VIX <- read_excel("C:/Users/tary20ab/OneDrive - CBS - Copenhagen Business School/Master/VIX.xlsx") # CBS PC
VIX$PriceDate <- as.Date(VIX$PriceDate) # Make sure dates are in Date format
VIX <- VIX %>%
  arrange(PriceDate) # Sort by date
View(VIX)
head(VIX)

# Load NIBOR data
NIBOR <- read_excel("C:/Users/Talem/OneDrive - CBS - Copenhagen Business School/Master/NIBOR.xlsx") ## my pc
# VIX <- read_excel("C:/Users/tary20ab/OneDrive - CBS - Copenhagen Business School/Master/VIX.xlsx") # CBS PC
NIBOR$Date <- as.Date(NIBOR$Date) # Make sure dates are in Date format
NIBOR <- NIBOR %>%
  arrange(Date) # Sort by date
View(NIBOR)
head(NIBOR)

# Increases the threshold for scientific notation
options(scipen = 999) 

# ============================================
#                Sub-periods
# ============================================

# Define periods
period1_start <- as.Date("2021-12-30")
period1_end <- as.Date("2023-12-29")

period2_start <- as.Date("2024-01-02")
period2_end <- as.Date("2025-03-10")


# ============================================
#                   VIX
# ============================================

# Filter
VIX_period1 <- VIX %>%
  filter(PriceDate >= period1_start & PriceDate <= period1_end)

VIX_period2 <- VIX %>%
  filter(PriceDate >= period2_start & PriceDate <= period2_end)

# Average per period
mean_vix_p1 <- mean(VIX_period1$VIX, na.rm = TRUE)
mean_vix_p2 <- mean(VIX_period2$VIX, na.rm = TRUE)

# Print
cat("Average VIX in period 1:", round(mean_vix_p1, 2), "\n")
cat("Average VIX in period 2:", round(mean_vix_p2, 2), "\n")

# Plot
VIX_filtered <- VIX %>%
  mutate(PriceDate = as.Date(PriceDate)) %>%
  filter(PriceDate >= as.Date("2022-01-01")) %>%
  mutate(QuarterLabel = paste0(year(PriceDate), "-Q", quarter(PriceDate)))


labels_df <- VIX_filtered %>%
  group_by(QuarterLabel) %>%
  slice(1) %>%
  ungroup()

quarter_dates <- labels_df$PriceDate
quarter_labels <- labels_df$QuarterLabel

ggplot(VIX_filtered, aes(x = PriceDate, y = VIX)) +
  geom_line(color = "steelblue", size = 1) +
  labs(title = "VIX over Time",
       x = "Quarter",
       y = "VIX Level") +
  theme_minimal() +
  scale_x_date(breaks = quarter_dates,
               labels = quarter_labels,
               expand = c(0.01, 0)) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 9),
    plot.title = element_text(hjust = 0.5),
    axis.title.x = element_text(margin = margin(t = 10))
  )


# ============================================
#                 NIBOR
# ============================================

# Filter
NIBOR_period1 <- NIBOR %>%
  filter(Date >= period1_start & Date <= period1_end)

NIBOR_period2 <- NIBOR %>%
  filter(Date >= period2_start & Date <= period2_end)

# Average per period
mean_NIBOR_p1 <- mean(NIBOR_period1$NIBOR, na.rm = TRUE)
mean_NIBOR_p2 <- mean(NIBOR_period2$NIBOR, na.rm = TRUE)

# Print
cat("Average NIBOR in period 1:", round(mean_NIBOR_p1, 2), "\n")
cat("Average NIBOR in period 2:", round(mean_NIBOR_p2, 2), "\n")


# Ensure PriceDate is of Date class
NIBOR_filtered <- NIBOR %>%
  mutate(Date = as.Date(Date)) %>%
  filter(Date >= as.Date("2022-01-01")) %>%
  mutate(QuarterLabel = paste0(year(Date), "-Q", quarter(Date)))

# Get the first date of each quarter
labels_df <- NIBOR_filtered %>%
  group_by(QuarterLabel) %>%
  slice(1) %>%
  ungroup()

quarter_dates <- labels_df$Date
quarter_labels <- labels_df$QuarterLabel

# Plot NIBOR time series with quarterly x-axis labels
ggplot(NIBOR_filtered, aes(x = Date, y = NIBOR)) +
  geom_line(color = "steelblue", size = 1) +
  labs(title = "NIBOR over Time",
       x = "Quarter",
       y = "NIBOR Level") +
  theme_minimal() +
  scale_x_date(breaks = quarter_dates,
               labels = quarter_labels,
               expand = c(0.01, 0)) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 9),
    plot.title = element_text(hjust = 0.5),
    axis.title.x = element_text(margin = margin(t = 10))
  )


# ============================================
#    Prepare data and test for stationarity
#============================================

# Ensure proper date format
green_index$PriceDate <- as.Date(green_index$PriceDate)
nbp_index$PriceDate <- as.Date(nbp_index$PriceDate)

names(green_index)
names(nbp_index)
names(VIX)
names(NIBOR)

NIBOR <- NIBOR %>%
  rename(PriceDate = Date)

# Combine to one dataframe
spread_data <- green_index %>%
  select(PriceDate, Spread_Green = AverageSpread) %>%
  left_join(nbp_index %>% select(PriceDate, Spread_NBP = AverageSpread), by = "PriceDate") %>%
  left_join(VIX, by = "PriceDate") %>%
  left_join(NIBOR, by = "PriceDate") %>%
  arrange(PriceDate)

view(spread_data)

# Remove the first row (where average spread is 0)
spread_data <- spread_data[-1, ]

view(spread_data)

# ------------------- Test stationarity on spread ----------------------------

spread_data <- spread_data %>%
  mutate(PriceDate = as.Date(PriceDate))

# Plot of Time Series
ts.plot(spread_data$Spread_Green, main = "Development in Spread (Green)")
ts.plot(spread_data$Spread_NBP, main = "Development in Spread (NBP)")

  ## Green
ggplot(spread_data, aes(x = PriceDate, y = Spread_Green)) +
  geom_line(color = "forestgreen", size = 0.8) +
  labs(
    title = "Development in Spread (Green)",
    x = "Date",
    y = "Spread (bps)"
  ) +
  scale_x_date(date_breaks = "3 months", date_labels = "%b %Y") +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
    axis.text.y = element_text(size = 12),
    axis.title = element_text(size = 12)
  )

  ## NBP
ggplot(spread_data, aes(x = PriceDate, y = Spread_NBP)) +
  geom_line(color = "steelblue", size = 0.8) +
  labs(
    title = "Development in Spread (NBP)",
    x = "Date",
    y = "Spread (bps)"
  ) +
  scale_x_date(date_breaks = "3 months", date_labels = "%b %Y") +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
    axis.text.y = element_text(size = 12),
    axis.title = element_text(size = 12)
  )


# ACF plot
acf(spread_data$Spread_Green)
acf(spread_data$Spread_NBP)

# ADF-test on spread
adf_green <- ur.df(spread_data$Spread_Green, type = "drift", selectlags = "AIC")
summary(adf_green)

adf_nbp <- ur.df(spread_data$Spread_NBP, type = "drift", selectlags = "AIC")
summary(adf_nbp)

# Clearly NON-STATIONARY

# ------------------- Test stationarity on NIBOR ----------------------------

# NIBOR: from plot, clearly not stationary in its original form
ts.plot(spread_data$NIBOR, main = "Development in NIBOR")



# ------------------- Take first diff ----------------------------

# Take first diff of spread to test if it's stationary
spread_data <- spread_data %>%
  mutate(
    d_spread_green = Spread_Green - lag(Spread_Green),
    d_spread_nbp = Spread_NBP - lag(Spread_NBP),
    d_NIBOR = NIBOR - lag(NIBOR)
  )


view(spread_data)

spread_data <- spread_data[-1, ]

view(spread_data)

# ------------------- Test stationarity again ----------------------------

# Plot of Time Series
ts.plot(spread_data$d_spread_green, main = "Development in Spread (Green)")
ts.plot(spread_data$d_spread_nbp, main = "Development in Spread (NBP)")

## Green
ggplot(spread_data, aes(x = PriceDate, y = d_spread_green)) +
  geom_line(color = "forestgreen", size = 0.8) +
  labs(
    title = "Development in Spread (dGreen)",
    x = "Date",
    y = "Spread (bps)"
  ) +
  scale_x_date(date_breaks = "3 months", date_labels = "%b %Y") +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
    axis.text.y = element_text(size = 12),
    axis.title = element_text(size = 12)
  )

## NBP
ggplot(spread_data, aes(x = PriceDate, y = d_spread_nbp)) +
  geom_line(color = "steelblue", size = 0.8) +
  labs(
    title = "Development in Spread (dNBP)",
    x = "Date",
    y = "Spread (bps)"
  ) +
  scale_x_date(date_breaks = "3 months", date_labels = "%b %Y") +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
    axis.text.y = element_text(size = 12),
    axis.title = element_text(size = 12)
  )

# ACF and PACF plot
acf(spread_data$d_spread_green) # Confirms AR(1)
acf(spread_data$d_spread_nbp) # Confirms AR(1)

pacf(spread_data$d_spread_green) # Confirms AR(1)
pacf(spread_data$d_spread_nbp) # Confirms AR(1)

# ADF test
# ADF-test på spread diff
adf_green <- ur.df(spread_data$d_spread_green, type = "drift", selectlags = "AIC")
summary(adf_green)

adf_nbp <- ur.df(spread_data$d_spread_nbp, type = "drift", selectlags = "AIC")
summary(adf_nbp)

# STATIONARY CONFIRMED

# ------------------- Test stationarity on NIBOR ----------------------------
# Plot diff to confirm stationarity
ts.plot(spread_data$d_NIBOR, main = "Development in d_NIBOR")
adf_NIBOR <- ur.df(na.omit(spread_data$d_NIBOR), type = "drift", selectlags = "AIC")
summary(adf_NIBOR)

# STATIONARY CONFIRMED

# ------------------- # Create lagged changes ----------------------------

# Create spread-changes and lags
spread_data <- spread_data %>%
  mutate(
    lag_d_spread_green = lag(d_spread_green),
    lag_d_spread_nbp = lag(d_spread_nbp),
    log_d_vix = log(VIX / lag(VIX))
  )

view(spread_data)
spread_data <- spread_data[-1, ]

# Convert spreads from bps to percent 
spread_data <- spread_data %>%
  mutate(
    Spread_Green_pct = Spread_Green / 100,
    Spread_NBP_pct = Spread_NBP / 100
  ) %>%
  mutate(
    d_spread_green = Spread_Green_pct - lag(Spread_Green_pct),
    d_spread_nbp   = Spread_NBP_pct - lag(Spread_NBP_pct),
    lag_d_spread_green = lag(d_spread_green),
    lag_d_spread_nbp   = lag(d_spread_nbp),
    log_d_vix = log(VIX / lag(VIX))
  ) %>%
  drop_na()

view(spread_data)

# ============================================
#        Green Index Spread Regression
# ============================================

# Run the OLS regression 
green_spread_model <- lm(d_spread_green ~ log_d_vix + + d_NIBOR+ lag_d_spread_green, data = spread_data)

# Newey-West standard errors
nw_se_green <- sqrt(diag(NeweyWest(green_spread_model)))

# Print with robust SE using stargazer
stargazer(green_spread_model,
          type = "text",
          se = list(nw_se_green),
          notes = "Robust standard errors are Newey-West (1987) adjusted",
          digits = 4)


# ============================================
#         NBP Index Spread Regression
# ============================================

# Run the OLS regression 
nbp_spread_model <- lm(d_spread_nbp ~ log_d_vix + + d_NIBOR + lag_d_spread_nbp, data = spread_data)

# Newey-West standard errors
nw_se_nbp <- sqrt(diag(NeweyWest(nbp_spread_model)))

# Print with robust SE using stargazer
stargazer(nbp_spread_model,
          type = "text",
          se = list(nw_se_nbp),
          notes = "Robust standard errors are Newey-West (1987) adjusted",
          digits = 4)


# ============================================
#               Period Split
# ============================================

# Ensure date format
spread_data$PriceDate <- as.Date(spread_data$PriceDate)

# Define periods
period1 <- spread_data %>%
  filter(PriceDate >= as.Date("2021-12-30") & PriceDate <= as.Date("2023-12-29"))

period2 <- spread_data %>%
  filter(PriceDate >= as.Date("2024-01-02") & PriceDate <= as.Date("2025-03-14"))

# ----------------- Green Spread Regressions -----------------

# Regression for Period 1
green_model_p1 <- lm(d_spread_green ~ log_d_vix + d_NIBOR + lag_d_spread_green, data = period1)
nw_se_p1 <- sqrt(diag(NeweyWest(green_model_p1)))

# Regression for Period 2
green_model_p2 <- lm(d_spread_green ~ log_d_vix + d_NIBOR + lag_d_spread_green, data = period2)
nw_se_p2 <- sqrt(diag(NeweyWest(green_model_p2)))

# Stargazer-output with both models
stargazer(green_model_p1, green_model_p2,
          type = "text",
          column.labels = c("Period 1", "Period 2"),
          se = list(nw_se_p1, nw_se_p2),
          title = "Regression: ΔSpread Green in Sub-Periods",
          notes = "Robust standard errors are Newey-West (1987) adjusted",
          digits = 4)


# ----------------- NBP Spread Regressions -----------------

# Regression for Period 1
nbp_model_p1 <- lm(d_spread_nbp ~ log_d_vix + d_NIBOR + lag_d_spread_nbp, data = period1)
nw_se_nbp_p1 <- sqrt(diag(NeweyWest(nbp_model_p1)))

# Regression for Period 2
nbp_model_p2 <- lm(d_spread_nbp ~ log_d_vix + d_NIBOR + lag_d_spread_nbp, data = period2)
nw_se_nbp_p2 <- sqrt(diag(NeweyWest(nbp_model_p2)))

# Stargazer-output with both models
stargazer(nbp_model_p1, nbp_model_p2,
          type = "text",
          column.labels = c("Period 1", "Period 2"),
          se = list(nw_se_nbp_p1, nw_se_nbp_p2),
          title = "Regression: ΔSpread NBP in Sub-Periods",
          notes = "Robust standard errors are Newey-West (1987) adjusted",
          digits = 4)


# ============================================
#    Testing OLS assumptions on all models 
# ============================================

# ----------------- Green Spread Regressions -----------------

# Autocorrelation
dwtest(green_spread_model) #OK
dwtest(green_model_p1) #OK
dwtest(green_model_p2) #OK

# Test for multicollinearity
vif(green_spread_model)   #OK
vif(green_model_p1)   #OK
vif(green_model_p2)   #OK

# Heteroscedasticity
bptest(green_spread_model)  #OK
bptest(green_model_p1)  #OK
bptest(green_model_p2)  #OK

# Normality in residuals
shapiro.test(resid(green_spread_model))  # Not normally
shapiro.test(resid(green_model_p1))  # Not normally
shapiro.test(resid(green_model_p2))  # Not normally

# Residual plot
plot(fitted(green_spread_model), resid(green_spread_model))
abline(h = 0, col = "forestgreen")
plot(fitted(green_model_p1), resid(green_model_p1))
abline(h = 0, col = "forestgreen")
plot(fitted(green_model_p2), resid(green_model_p2))
abline(h = 0, col = "forestgreen")

acf(resid(green_spread_model))
pacf(resid(green_spread_model))
acf(resid(green_model_p1))
pacf(resid(green_model_p1))
acf(resid(green_model_p2))
pacf(resid(green_model_p2))

# Q-Q plot
qqnorm(rstandard(green_spread_model), main = "Normal Q-Q Plot of Relative Returns")
qqline(rstandard(green_spread_model), col = "forestgreen", lwd = 2)
qqnorm(rstandard(green_model_p1), main = "Normal Q-Q Plot of Relative Returns")
qqline(rstandard(green_model_p1), col = "forestgreen", lwd = 2)
qqnorm(rstandard(green_model_p2), main = "Normal Q-Q Plot of Relative Returns")
qqline(rstandard(green_model_p2), col = "forestgreen", lwd = 2)


# ----------------- NBP Spread Regressions -----------------

# Autocorrelation
dwtest(nbp_spread_model) #OK
dwtest(nbp_model_p1) #OK
dwtest(nbp_model_p2) #OK

# Test for multicollinearity
vif(nbp_spread_model)   #OK
vif(nbp_model_p1)   #OK
vif(nbp_model_p2)   #OK

# Heteroscedasticity
bptest(nbp_spread_model)  #OK
bptest(nbp_model_p1)  #OK
bptest(nbp_model_p2)  #OK

# Normality in residuals
shapiro.test(resid(nbp_spread_model))  # Not normally
shapiro.test(resid(nbp_model_p1))  # Not normally
shapiro.test(resid(nbp_model_p2))  # Not normally

# Residual plot
plot(fitted(nbp_spread_model), resid(nbp_spread_model))
abline(h = 0, col = "steelblue")
plot(fitted(nbp_model_p1), resid(nbp_model_p1))
abline(h = 0, col = "steelblue")
plot(fitted(nbp_model_p2), resid(nbp_model_p2))
abline(h = 0, col = "steelblue")

acf(resid(nbp_spread_model))
pacf(resid(nbp_spread_model))
acf(resid(nbp_model_p1))
pacf(resid(nbp_model_p1))
acf(resid(nbp_model_p2))
pacf(resid(nbp_model_p2))

# Q-Q plot
qqnorm(rstandard(nbp_spread_model), main = "Normal Q-Q Plot of Relative Returns")
qqline(rstandard(nbp_spread_model), col = "steelblue", lwd = 2)
qqnorm(rstandard(nbp_model_p1), main = "Normal Q-Q Plot of Relative Returns")
qqline(rstandard(nbp_model_p1), col = "steelblue", lwd = 2)
qqnorm(rstandard(nbp_model_p2), main = "Normal Q-Q Plot of Relative Returns")
qqline(rstandard(nbp_model_p2), col = "steelblue", lwd = 2)


# ============================================
#               Present Results
# ============================================

# Stargazer with all periods green
stargazer(green_model_p1, green_model_p2, green_spread_model,
          type = "text",
          column.labels = c("Period 1", "Period 2", "Full Period"),
          se = list(nw_se_p1, nw_se_p2, nw_se_green),
          title = "Regression: ΔSpread Green in Sub-Periods",
          notes = "Robust standard errors are Newey-West (1987)-adjusted",
          digits = 4)

# Stargazer with all periods nbp
stargazer(nbp_model_p1, nbp_model_p2, nbp_spread_model,
          type = "text",
          column.labels = c("Period 1", "Period 2", "Full Period"),
          se = list(nw_se_nbp_p1, nw_se_nbp_p2, nw_se_nbp),
          title = "Regression: ΔSpread NBP in Sub-Periods",
          notes = "Robust standard errors are Newey-West (1987)-adjusted",
          digits = 4)


###########################################################################################################################

# ============================================
#     Bid-Ask Spread for the Green Index
# ============================================

# ------------ Bid-ask spread daily ----------------

# Ensure that the date is in the correct format
green_index$PriceDate <- as.Date(green_index$PriceDate)

# Remove any NAs
plot_data_bidask <- green_index %>%
  filter(!is.na(`Average bid-ask spread`))

# Plot
ggplot(plot_data_bidask, aes(x = PriceDate, y = `Average bid-ask spread`)) +
  geom_line(color = "forestgreen", size = 0.8) +
  labs(
    title = "Bid-Ask Spread Over Time",
    subtitle = "Green HY Index",
    x = "Date",
    y = "Bid-Ask Spread (bps)"
  ) +
  scale_y_continuous(
    breaks = seq(0.8, 2.0, by = 0.2),
    labels = scales::comma_format(accuracy = 0.1)
  )+
  scale_x_date(
    breaks = seq(as.Date("2021-12-01"), as.Date("2025-03-01"), by = "3 months"),
    date_labels = "%b %Y",
    limits = c(min(plot_data_bidask$PriceDate), as.Date("2025-03-31")),
    expand = c(0, 0)
  )+
  theme_minimal() +
  theme(
    plot.title = element_text(size = 13, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 11, hjust = 0.5),
    axis.title = element_text(size = 10),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )


# ------------ Bid-ask spread monthly ----------------

# Add spread in bps as a column 
green_index <- green_index %>%
  mutate(BidAskSpread_bps = `Average bid-ask spread` * 100)

# Create YearMonth
green_index$YearMonth <- floor_date(green_index$PriceDate, "month")

# Filter out NAs and calculate monthly average bid-ask spread
monthly_bid_ask <- green_index %>%
  filter(!is.na(BidAskSpread_bps)) %>%
  group_by(YearMonth) %>%
  summarise(AvgBidAskSpread_bps = mean(BidAskSpread_bps, na.rm = TRUE)) %>%
  ungroup()

# Plot
ggplot(monthly_bid_ask, aes(x = YearMonth, y = AvgBidAskSpread_bps)) +
  geom_line(color = "forestgreen", size = 1) +
  labs(
    title = "Monthly Bid-Ask Spread: Green HY Index",
    x = "Month",
    y = "Average Bid-Ask Spread (bps)"
  ) +
  scale_y_continuous(labels = comma_format(accuracy = 1)) +
  scale_x_date(
    breaks = seq(as.Date("2022-03-01"), as.Date("2025-03-01"), by = "3 months"),
    labels = date_format("%b %Y"),
    limits = c(as.Date("2022-01-01"), as.Date("2025-03-31")),
    expand = c(0, 0)
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 13, face = "bold", hjust = 0.5),
    axis.text.x = element_text(size = 14, angle = 45, hjust = 1),
    axis.title = element_text(size = 14),
    axis.text.y = element_text(size = 14),
    axis.text.x.top = element_text(size = 14)
  )


# ------------ Bid-ask spread across sectors ----------------

# Load indexes from excel
BidAsk_Spreads_Sectors <- read_excel("C:/Users/Talem/OneDrive - CBS - Copenhagen Business School/Master/BidAskSpreadsSectors.xlsx") ## my pc
# BidAsk_Spreads_Sectors <- read_excel("C:/Users/tary20ab/OneDrive - CBS - Copenhagen Business School/Master/BidAskSpreadsSectors.xlsx") ## cbs pc
View(BidAsk_Spreads_Sectors)
head(BidAsk_Spreads_Sectors)


# Convert spreads to basis points and format date
BidAsk_Spreads_Sectors <- BidAsk_Spreads_Sectors %>%
  mutate(
    `Avg bid-ask spread wo RE (bps)` = `Avg bid-ask spread wo RE` * 100,
    `Avg. Bid-ask Spread RE (bps)` = `Avg. Bid-ask Spread RE` * 100,
    `Original (bps)` = Original * 100,
    Date = as.Date(Date)
  )

view(BidAsk_Spreads_Sectors)

# Reshape to long format for ggplot
plot_bidask_daily <- BidAsk_Spreads_Sectors %>%
  select(Date, `Avg bid-ask spread wo RE (bps)`, `Avg. Bid-ask Spread RE (bps)`, `Original (bps)`) %>%
  pivot_longer(-Date, names_to = "Series", values_to = "BidAsk_bps")

# Rename factor levels for clearer legend labels
plot_bidask_daily <- plot_bidask_daily %>%
  mutate(Series = factor(Series,
                         levels = c("Avg bid-ask spread wo RE (bps)", 
                                    "Avg. Bid-ask Spread RE (bps)", 
                                    "Original (bps)"),
                         labels = c("Without Real Estate", 
                                    "Real Estate Only", 
                                    "Full Green Index")))


# Define custom colors
blue_palette <- c(
  "Without Real Estate" = "#1f77b4",     # Blue
  "Real Estate Only"    = "#08519c",     # Dark Blue
  "Full Green Index"    = "forestgreen"  # Green
)

# Plot daily bid-ask spread by sector
ggplot(plot_bidask_daily, aes(x = Date, y = BidAsk_bps, color = Series)) +
  geom_line(size = 0.9) +
  labs(
    title = "Daily Bid-Ask Spread by Sector",
    subtitle = "Green HY Index: With and Without Real Estate",
    x = "Date",
    y = "Bid-Ask Spread (bps)",
    color = NULL
  ) +
  scale_color_manual(values = blue_palette) +
  scale_y_continuous(labels = comma_format(accuracy = 1)) +
  scale_x_date(
    limits = c(as.Date("2021-12-30"), as.Date("2025-03-31")),
    breaks = seq(as.Date("2022-03-01"), as.Date("2025-03-01"), by = "3 months"),
    date_labels = "%b %Y",
    expand = c(0, 0)
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 11, hjust = 0.5),
    axis.title = element_text(size = 12),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "top",
    legend.text = element_text(size = 10)
  )


# Add YearMonth column
BidAsk_Spreads_Sectors <- BidAsk_Spreads_Sectors %>%
  mutate(YearMonth = floor_date(Date, "month"))

# Calculate monthly averages
monthly_bidask <- BidAsk_Spreads_Sectors %>%
  group_by(YearMonth) %>%
  summarise(
    `Avg bid-ask spread wo RE (bps)` = mean(`Avg bid-ask spread wo RE (bps)`, na.rm = TRUE),
    `Avg. Bid-ask Spread RE (bps)` = mean(`Avg. Bid-ask Spread RE (bps)`, na.rm = TRUE),
    `Original (bps)` = mean(`Original (bps)`, na.rm = TRUE)
  ) %>%
  pivot_longer(-YearMonth, names_to = "Series", values_to = "BidAsk_bps")

# Rename series for clean legend labels
monthly_bidask <- monthly_bidask %>%
  mutate(Series = factor(Series,
                         levels = c("Avg bid-ask spread wo RE (bps)",
                                    "Avg. Bid-ask Spread RE (bps)",
                                    "Original (bps)"),
                         labels = c("Without Real Estate",
                                    "Real Estate Only",
                                    "Full Green Index")))

# Color palette
blue_palette <- c(
  "Without Real Estate" = "#1f77b4",
  "Real Estate Only"    = "#08519c",
  "Full Green Index"    = "forestgreen"
)


# Plot monthly averages
ggplot(monthly_bidask, aes(x = YearMonth, y = BidAsk_bps, color = Series)) +
  geom_line(size = 1) +
  labs(
    title = "Monthly Avg. Bid-Ask Spread by Sector",
    subtitle = "Green HY Index: With and Without Real Estate",
    x = "Month",
    y = "Avg. Bid-Ask Spread (bps)",
    color = NULL
  ) +
  scale_color_manual(values = blue_palette) +
  scale_y_continuous(labels = comma_format(accuracy = 1)) +
  scale_x_date(
    limits = c(as.Date("2021-12-30"), as.Date("2025-03-31")),
    breaks = seq(as.Date("2022-03-01"), as.Date("2025-03-01"), by = "3 months"),
    date_labels = "%b %Y",
    expand = c(0, 0)
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 11, hjust = 0.5),
    axis.title = element_text(size = 12),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "top",
    legend.text = element_text(size = 10)
  )

ggplot(monthly_bidask, aes(x = YearMonth, y = BidAsk_bps, color = Series)) +
  geom_line(size = 1) +
  labs(
    x = "Month",
    y = "Avg. Bid-Ask Spread (bps)",
    color = NULL
  ) +
  scale_color_manual(values = blue_palette) +
  scale_y_continuous(labels = comma_format(accuracy = 1)) +
  scale_x_date(
    limits = c(as.Date("2021-12-30"), as.Date("2025-03-31")),
    breaks = seq(as.Date("2022-03-01"), as.Date("2025-03-01"), by = "3 months"),
    date_labels = "%b %Y",
    expand = c(0, 0)
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 11, hjust = 0.5),
    axis.title = element_text(size = 14),
    axis.text.x = element_text(size = 14, angle = 45, hjust = 1),
    legend.position = "top",
    legend.text = element_text(size = 14)
  )

