# ============================================
#             Preparing RScript
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
setwd('C:/Users/Talem/OneDrive - CBS - Copenhagen Business School/Master') ## my oc
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

# Increases the threshold for scientific notation
options(scipen = 999)  

# ============================================
#           Overview of Datasets
# ============================================

# Number of rows and columns for the indexes: 
n_green <- nrow(green_index)
n_green
c_green <- ncol(green_index)
c_green

n_nbp <- nrow(nbp_index)
n_nbp
c_nbp <- ncol(nbp_index)
c_nbp

# Overview of the data
str(green_index) ## Overview of data
str(nbp_index) ## Overview of data

summary(green_index) ## Gives the descriptive statistics of the dataset
summary(nbp_index) ## Gives the descriptive statistics of the dataset

# Check if there are any NA (missing) values
anyNA(green_index) #FALSE
anyNA(nbp_index) #FALSE

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

summary(combined_returns) ## Gives the descriptive statistics of the dataset

# Generate descriptive statistics table (daily, not in %)
stargazer(combined_returns, type = "text", title = "Descriptive Statistics (not in %)", digits = 4)


# ============================================
#         Plot of Index Development 
# ============================================

# Create dataset for plotting
green_plot <- data.frame(Date = green_index$PriceDate, IndexValue = green_index$IndexValue, Index = "Green HY Index")
nbp_plot <- data.frame(Date = nbp_index$PriceDate, IndexValue = nbp_index$IndexValue, Index = "NBP Nordic HY Index")

# Combine
combined_plot <- rbind(green_plot, nbp_plot)

# Ensure date format is correct
combined_plot$Date <- as.Date(green_index$PriceDate)

# Plot Index Value over Time
ggplot(combined_plot, aes(x = Date, y = IndexValue, color = Index)) +
  geom_line(size = 0.9) + 
  geom_hline(yintercept = 100, color = "gray60", linetype = "dashed", size = 0.4) +
  scale_color_manual(
    values = c("Green HY Index" = "forestgreen", "NBP Nordic HY Index" = "steelblue")
  ) +
  labs(
    x = "Date",
    y = "Index Value",
    color = NULL
  ) +
  scale_y_continuous(
    breaks = seq(95, 130, by = 5),
    labels = scales::comma_format(accuracy = 1)
  ) +
  scale_x_date(
    date_breaks = "3 months",
    date_labels = "%b %Y",
    limits = c(min(combined_plot$Date), as.Date("2025-03-31")),
    expand = c(0, 0)
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 12, face = "bold", hjust = 0.5, margin = margin(b = 10)),
    axis.title = element_text(size = 14),
    axis.text.x = element_text(size = 12, angle = 45, hjust = 1),  
    axis.text.y = element_text(size = 12),                        
    legend.position = "top",
    legend.direction = "horizontal",
    legend.title = element_blank(),
    legend.text = element_text(size = 14),
    panel.grid.major = element_line(color = "grey85", size = 0.3),
    panel.grid.minor = element_blank()
  )


# ============================================
#              Average Metrics
# ============================================

# Average coupon
green_avg_coupon <- mean(green_index$AverageCoupon)
nbp_avg_coupon <- mean(nbp_index$AverageCoupon)

# Average time to maturity
green_avg_ttm <- mean(green_index$AverageTtm)
nbp_avg_ttm <- mean(nbp_index$AverageTtm)

# Average modified duration
green_avg_md <- mean(green_index$AverageModifiedDuration)
nbp_avg_md <- mean(nbp_index$AverageModifiedDuration)

# Average credit duration
green_avg_cd <- mean(green_index$AverageCreditDuration)
nbp_avg_cd <- mean(nbp_index$AverageCreditDuration)

# Average spread
green_avg_spread <- mean(green_index$AverageSpread)
nbp_avg_spread <- mean(nbp_index$AverageSpread)

# Average yield
green_avg_yield <- mean(green_index$AverageYield)
nbp_avg_yield <- mean(nbp_index$AverageYield)

# Create a dataframe with all averages
averages_table <- data.frame(
  Variable = c("Average Coupon (%)", "Average Time to Maturity (years)", "Credit Duration", "Average Spread (bps)", "Average Yield (%)"),
  `Green HY Index` = round(c(green_avg_coupon, green_avg_ttm, green_avg_cd, green_avg_spread, green_avg_yield * 100), 2),
  `NBP Nordic HY Index` = round(c(nbp_avg_coupon, nbp_avg_ttm, nbp_avg_cd, nbp_avg_spread, nbp_avg_yield * 100), 2)
)

# Change column names
colnames(averages_table) <- c("Variable", "Green HY Index", "NBP Nordic HY Index")

# Present with Stargazer
stargazer(averages_table, type = "text", summary = FALSE,
          title = "Average Characteristics of the Green HY Index vs NBP Nordic HY Index",
          digits = 2)

# ======================================
#      CALCULATING MONTHLY RETURNS 
# ======================================

# Make sure dates are in Date format
combined_returns$Date <- as.Date(combined_returns$Date)

# Create a YearMonth variable
combined_returns$YearMonth <- floor_date(combined_returns$Date, "month")

# Calculate monthly total return
monthly_returns <- combined_returns %>%
  group_by(YearMonth) %>%
  summarise(
    Green_monthly_return = prod(1 + Green, na.rm = TRUE) - 1,
    NBP_monthly_return   = prod(1 + NBP, na.rm = TRUE) - 1,
    .groups = "drop"
  )

# Remove December 2021 since this date has zero return as it marks the start of the indices
monthly_returns <- monthly_returns %>%
  filter(YearMonth > as.Date("2021-12-01"))

view(monthly_returns)

summary(monthly_returns) ## Gives the descriptive statistics of the dataset


# ------------ Defining sub-periods ----------------

# Period 1: 31.12.2021 – 29.12.2023
period1_returns <- combined_returns %>%
  filter(Date >= as.Date("2021-12-31") & Date <= as.Date("2023-12-29")) %>%
  group_by(YearMonth = floor_date(Date, "month")) %>%
  summarise(
    Green_monthly_return = prod(1 + Green, na.rm = TRUE) - 1,
    NBP_monthly_return   = prod(1 + NBP, na.rm = TRUE) - 1,
    .groups = "drop"
  )

# Period 2: 31.01.2024 – 10.03.2025
period2_returns <- combined_returns %>%
  filter(Date >= as.Date("2024-01-31") & Date <= as.Date("2025-03-10")) %>%
  group_by(YearMonth = floor_date(Date, "month")) %>%
  summarise(
    Green_monthly_return = prod(1 + Green, na.rm = TRUE) - 1,
    NBP_monthly_return   = prod(1 + NBP, na.rm = TRUE) - 1,
    .groups = "drop"
  )


# ------------ Plot Monthly Returns: Green vs NBP ----------------

# Long format for plotting
plot_monthly_return <- monthly_returns %>%
  pivot_longer(cols = c(Green_monthly_return, NBP_monthly_return),
               names_to = "Index", values_to = "MonthlyReturn")

plot_monthly_return_clean <- plot_monthly_return %>%
  mutate(
    Index = case_when(
      Index == "Green_monthly_return" ~ "Green HY Index",
      Index == "NBP_monthly_return" ~ "NBP Nordic HY Index"
    )
  )

# Plot
ggplot(plot_monthly_return_clean, aes(x = YearMonth, y = MonthlyReturn, color = Index)) +
  geom_line(size = 0.9) +
  geom_hline(yintercept = 0, color = "gray60", linetype = "dashed", size = 0.4) +
  scale_color_manual(
    values = c("Green HY Index" = "forestgreen", "NBP Nordic HY Index" = "steelblue")
  ) +
  labs(
    title = "Monthly Returns: Green HY Index vs NBP Nordic HY Index",
    x = "Month",
    y = "Monthly Return",
    color = NULL
  ) +
  scale_y_continuous(
    labels = percent_format(accuracy = 0.01),
    breaks = seq(-0.03, 0.03, by = 0.01)
  ) +
  scale_x_date(
    date_breaks = "3 months",
    date_labels = "%b %Y",
    limits = c(min(plot_monthly_return_clean$YearMonth), as.Date("2025-03-31")),
    expand = c(0, 0)
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 12, face = "bold", hjust = 0.5, margin = margin(b = 10)),
    axis.title = element_text(size = 10),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "top",
    legend.direction = "horizontal",
    legend.title = element_blank(),
    legend.text = element_text(size = 10),
    panel.grid.major = element_line(color = "grey85", size = 0.3),
    panel.grid.minor = element_blank()
  )


# ------------ Boxplot (full period) ----------------

ggplot(plot_monthly_return_clean, aes(x = Index, y = MonthlyReturn, fill = Index)) +
  geom_boxplot(outlier.color = "red", outlier.size = 1.5) +
  scale_y_continuous(labels = percent_format(accuracy = 0.1)) +
  labs(
    title = "Distribution of Monthly Returns (full period)",
    x = NULL,
    y = "Monthly Return"
  ) +
  scale_fill_manual(values = c("Green HY Index" = "forestgreen", "NBP Nordic HY Index" = "steelblue")) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    legend.position = "none",
    axis.text.x = element_text(size = 10),
    axis.title.y = element_text(size = 10)
  )

# ------------ Boxplot (period 1) ----------------

# Long-format for Periode 1
plot_period1 <- period1_returns %>%
  pivot_longer(cols = c(Green_monthly_return, NBP_monthly_return),
               names_to = "Index", values_to = "MonthlyReturn") %>%
  mutate(Index = case_when(
    Index == "Green_monthly_return" ~ "Green HY Index",
    Index == "NBP_monthly_return" ~ "NBP Nordic HY Index"
  ))

# Boxplot
ggplot(plot_period1, aes(x = Index, y = MonthlyReturn, fill = Index)) +
  geom_boxplot(outlier.color = "red", outlier.size = 1.5) +
  scale_y_continuous(labels = percent_format(accuracy = 0.1)) +
  labs(
    title = "Distribution of Monthly Returns (period 1)",
    x = NULL,
    y = "Monthly Return"
  ) +
  scale_fill_manual(values = c("Green HY Index" = "forestgreen", "NBP Nordic HY Index" = "steelblue")) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    legend.position = "none",
    axis.text.x = element_text(size = 10),
    axis.title.y = element_text(size = 10)
  )

# ------------ Boxplot (period 2) ----------------

# Long-format for Periode 2
plot_period2 <- period2_returns %>%
  pivot_longer(cols = c(Green_monthly_return, NBP_monthly_return),
               names_to = "Index", values_to = "MonthlyReturn") %>%
  mutate(Index = case_when(
    Index == "Green_monthly_return" ~ "Green HY Index",
    Index == "NBP_monthly_return" ~ "NBP Nordic HY Index"
  ))

# Boxplot
ggplot(plot_period2, aes(x = Index, y = MonthlyReturn, fill = Index)) +
  geom_boxplot(outlier.color = "red", outlier.size = 1.5) +
  scale_y_continuous(labels = percent_format(accuracy = 0.1)) +
  labs(
    title = "Distribution of Monthly Returns (period 2)",
    x = NULL,
    y = "Monthly Return"
  ) +
  scale_fill_manual(values = c("Green HY Index" = "forestgreen", "NBP Nordic HY Index" = "steelblue")) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    legend.position = "none",
    axis.text.x = element_text(size = 10),
    axis.title.y = element_text(size = 10)
  )


# ================================================
#            DESCRIPTIVE STATISTICS
# ================================================

# ------------ Full period ----------------

# Calculate descriptive statistics for each index (monthly)
mean_green_monthly <- mean(monthly_returns$Green_monthly_return, na.rm = TRUE)
median_green_monthly <- median(monthly_returns$Green_monthly_return, na.rm = TRUE)
sd_green_monthly <- sd(monthly_returns$Green_monthly_return, na.rm = TRUE)
skew_green_monthly   <- skewness(monthly_returns$Green_monthly_return, na.rm = TRUE)
kurt_green_monthly   <- kurtosis(monthly_returns$Green_monthly_return, na.rm = TRUE)

mean_nbp_monthly <- mean(monthly_returns$NBP_monthly_return, na.rm = TRUE)
median_nbp_monthly <- median(monthly_returns$NBP_monthly_return, na.rm = TRUE)
sd_nbp_monthly <- sd(monthly_returns$NBP_monthly_return, na.rm = TRUE)
skew_nbp_monthly   <- skewness(monthly_returns$NBP_monthly_return, na.rm = TRUE)
kurt_nbp_monthly   <- kurtosis(monthly_returns$NBP_monthly_return, na.rm = TRUE)

n_obs <- nrow(monthly_returns)

# Create summary table 
summary_table_monthly_returns <- data.frame(
  Statistic = c("Nr of Obs", "Mean (%)", "Median (%)", "Std. Dev. (%)", "Skewness", "Kurtosis"),
  `Green HY Index` = round(c(
    n_obs,
    mean_green_monthly * 100,
    median_green_monthly * 100,
    sd_green_monthly * 100,
    skew_green_monthly,
    kurt_green_monthly
  ), 2),
  `NBP Nordic HY Index` = round(c(
    n_obs,
    mean_nbp_monthly * 100,
    median_nbp_monthly * 100,
    sd_nbp_monthly * 100,
    skew_nbp_monthly,
    kurt_nbp_monthly
  ), 2)
)

# Present with Stargazer
stargazer(summary_table_monthly_returns, type = "text",
          title = "Summary Statistics Full Period - Monthly Returns (%)",
          digits = 2,
          summary = FALSE)


# ------------ Period 1 ----------------

# Calculate descriptive statistics for each index (monthly)
mean_green_p1 <- mean(period1_returns$Green_monthly_return, na.rm = TRUE)
median_green_p1 <- median(period1_returns$Green_monthly_return, na.rm = TRUE)
sd_green_p1 <- sd(period1_returns$Green_monthly_return, na.rm = TRUE)
skew_green_p1 <- skewness(period1_returns$Green_monthly_return, na.rm = TRUE)
kurt_green_p1 <- kurtosis(period1_returns$Green_monthly_return, na.rm = TRUE)

mean_nbp_p1 <- mean(period1_returns$NBP_monthly_return, na.rm = TRUE)
median_nbp_p1 <- median(period1_returns$NBP_monthly_return, na.rm = TRUE)
sd_nbp_p1 <- sd(period1_returns$NBP_monthly_return, na.rm = TRUE)
skew_nbp_p1 <- skewness(period1_returns$NBP_monthly_return, na.rm = TRUE)
kurt_nbp_p1 <- kurtosis(period1_returns$NBP_monthly_return, na.rm = TRUE)

n_obs_p1 <- nrow(period1_returns)

# Create summary table
summary_table_p1 <- data.frame(
  Statistic = c("Nr of Obs", "Mean (%)", "Median (%)", "Std. Dev. (%)", "Skewness", "Kurtosis"),
  `Green HY Index` = round(c(
    n_obs_p1,
    mean_green_p1 * 100,
    median_green_p1 * 100,
    sd_green_p1 * 100,
    skew_green_p1,
    kurt_green_p1
  ), 2),
  `NBP Nordic HY Index` = round(c(
    n_obs_p1,
    mean_nbp_p1 * 100,
    median_nbp_p1 * 100,
    sd_nbp_p1 * 100,
    skew_nbp_p1,
    kurt_nbp_p1
  ), 2)
)

# Present with Stargazer
stargazer(summary_table_p1, type = "text",
          title = "Summary Statistics Period 1 – Monthly Returns (%)",
          digits = 2,
          summary = FALSE)


# ------------ Period 2 ----------------

# Calculate descriptive statistics for each index (monthly)
mean_green_p2 <- mean(period2_returns$Green_monthly_return, na.rm = TRUE)
median_green_p2 <- median(period2_returns$Green_monthly_return, na.rm = TRUE)
sd_green_p2 <- sd(period2_returns$Green_monthly_return, na.rm = TRUE)
skew_green_p2 <- skewness(period2_returns$Green_monthly_return, na.rm = TRUE)
kurt_green_p2 <- kurtosis(period2_returns$Green_monthly_return, na.rm = TRUE)

mean_nbp_p2 <- mean(period2_returns$NBP_monthly_return, na.rm = TRUE)
median_nbp_p2 <- median(period2_returns$NBP_monthly_return, na.rm = TRUE)
sd_nbp_p2 <- sd(period2_returns$NBP_monthly_return, na.rm = TRUE)
skew_nbp_p2 <- skewness(period2_returns$NBP_monthly_return, na.rm = TRUE)
kurt_nbp_p2 <- kurtosis(period2_returns$NBP_monthly_return, na.rm = TRUE)

n_obs_p2 <- nrow(period2_returns)

# Create summary table
summary_table_p2 <- data.frame(
  Statistic = c("Nr of Obs", "Mean (%)", "Median (%)", "Std. Dev. (%)", "Skewness", "Kurtosis"),
  `Green HY Index` = round(c(
    n_obs_p2,
    mean_green_p2 * 100,
    median_green_p2 * 100,
    sd_green_p2 * 100,
    skew_green_p2,
    kurt_green_p2
  ), 2),
  `NBP Nordic HY Index` = round(c(
    n_obs_p2,
    mean_nbp_p2 * 100,
    median_nbp_p2 * 100,
    sd_nbp_p2 * 100,
    skew_nbp_p2,
    kurt_nbp_p2
  ), 2)
)

# Present with Stargazer
stargazer(summary_table_p2, type = "text",
          title = "Summary Statistics – Period 2 (2024–2025) – Monthly Returns (%)",
          digits = 2,
          summary = FALSE)


# ============================================
#      Compound Annual Growth Rate (CAGR)
# ============================================

# ------------ Full period ----------------

# Ensure that the date is in the correct format
green_index$PriceDate <- as.Date(green_index$PriceDate)
nbp_index$PriceDate <- as.Date(nbp_index$PriceDate )

# Retrieve the first and last index value
V0_green <- first(green_index$IndexValue)
Vn_green <- last(green_index$IndexValue)

V0_nbp <- first(nbp_index$IndexValue)
Vn_nbp <- last(nbp_index$IndexValue)

# Number of years (based on dates)
n_years <- as.numeric(difftime(max(combined_returns$Date), min(combined_returns$Date), units = "days")) / 365.25

# Calculate CAGR
cagr_green <- (Vn_green / V0_green)^(1 / n_years) - 1
cagr_nbp   <- (Vn_nbp   / V0_nbp)^(1 / n_years) - 1

# Create table
cagr_table <- data.frame(
  Index = c("Green HY Index", "NBP Nordic HY Index"),
  CAGR = round(c(cagr_green, cagr_nbp) * 100, 2)  # in percent 
)

print(cagr_table)

# Present with stargazer
stargazer(cagr_table, type = "text", summary = FALSE,
          title = "Compound Annual Growth Rate (in %)",
          digits = 2)

# ------------ Period 1 ----------------
start_date_p1 <- as.Date("2021-12-30")
end_date_p1   <- as.Date("2023-12-29")
years_p1 <- as.numeric(difftime(end_date_p1, start_date_p1, units = "days")) / 365.25

V0_green_p1 <- green_index$IndexValue[green_index$PriceDate == start_date_p1]
Vn_green_p1 <- green_index$IndexValue[green_index$PriceDate == end_date_p1]

V0_nbp_p1 <- nbp_index$IndexValue[nbp_index$PriceDate == start_date_p1]
Vn_nbp_p1 <- nbp_index$IndexValue[nbp_index$PriceDate == end_date_p1]

cagr_green_p1 <- (Vn_green_p1 / V0_green_p1)^(1 / years_p1) - 1
cagr_nbp_p1   <- (Vn_nbp_p1 / V0_nbp_p1)^(1 / years_p1) - 1


# ------------ Period 2 ----------------
start_date_p2 <- as.Date("2024-01-02")
end_date_p2   <- as.Date("2025-03-10")
years_p2 <- as.numeric(difftime(end_date_p2, start_date_p2, units = "days")) / 365.25

V0_green_p2 <- green_index$IndexValue[green_index$PriceDate == start_date_p2]
Vn_green_p2 <- green_index$IndexValue[green_index$PriceDate == end_date_p2]

V0_nbp_p2 <- nbp_index$IndexValue[nbp_index$PriceDate == start_date_p2]
Vn_nbp_p2 <- nbp_index$IndexValue[nbp_index$PriceDate == end_date_p2]

cagr_green_p2 <- (Vn_green_p2 / V0_green_p2)^(1 / years_p2) - 1
cagr_nbp_p2   <- (Vn_nbp_p2 / V0_nbp_p2)^(1 / years_p2) - 1


# ------------ Combined CAGR Table ----------------

cagr_all <- data.frame(
  Period = c("Full Period", "Period 1", "Period 2"),
  `Green HY Index (%)` = round(c(cagr_green, cagr_green_p1, cagr_green_p2) * 100, 2),
  `NBP Nordic HY Index (%)` = round(c(cagr_nbp, cagr_nbp_p1, cagr_nbp_p2) * 100, 2)
)

# With stargazer:
stargazer(cagr_all, type = "text", summary = FALSE,
          title = "Compound Annual Growth Rate by Period (in %)",
          digits = 2)


# ================================================
#     FULL DESCRIPTIVE ANALYSIS - TOTAL RETURN
# ================================================

# Create a total summary overview table
combined_summary <- data.frame(
  Variable = c("Nr of Obs", "CAGR", "Mean (%)", "Median (%)", "Std.Dev (%)", "Skewness", "Kurtosis"),
  
  `Period 1 - Green` = round(c(
    n_obs_p1,
    cagr_green_p1 * 100,
    mean_green_p1 * 100,
    median_green_p1 * 100,
    sd_green_p1 * 100,
    skew_green_p1,
    kurt_green_p1
  ), 2),
  
  `Period 1 - NBP` = round(c(
    n_obs_p1,
    cagr_nbp_p1 * 100,
    mean_nbp_p1 * 100,
    median_nbp_p1 * 100,
    sd_nbp_p1 * 100,
    skew_nbp_p1,
    kurt_nbp_p1
  ), 2),
  
  `Period 2 - Green` = round(c(
    n_obs_p2,
    cagr_green_p2 * 100,
    mean_green_p2 * 100,
    median_green_p2 * 100,
    sd_green_p2 * 100,
    skew_green_p2,
    kurt_green_p2
  ), 2),
  
  `Period 2 - NBP` = round(c(
    n_obs_p2,
    cagr_nbp_p2 * 100,
    mean_nbp_p2 * 100,
    median_nbp_p2 * 100,
    sd_nbp_p2 * 100,
    skew_nbp_p2,
    kurt_nbp_p2
  ), 2),
  
  `Full Period - Green` = round(c(
    n_obs,
    cagr_green * 100,
    mean_green_monthly * 100,
    median_green_monthly * 100,
    sd_green_monthly * 100,
    skew_green_monthly,
    kurt_green_monthly
  ), 2),
  
  `Full Period - NBP` = round(c(
    n_obs,
    cagr_nbp * 100,
    mean_nbp_monthly * 100,
    median_nbp_monthly * 100,
    sd_nbp_monthly * 100,
    skew_nbp_monthly,
    kurt_nbp_monthly
  ), 2)
)

# Show with stargazer
stargazer(combined_summary, type = "text", summary = FALSE,
          title = "Summary Statistics by Period and Index", digits = 2)


# ============================================
#              Total Returns
# ============================================

# Calculate total return for the full period (from monthly_returns)
green_total_return <- prod(1 + monthly_returns$Green_monthly_return, na.rm = TRUE) - 1
nbp_total_return   <- prod(1 + monthly_returns$NBP_monthly_return, na.rm = TRUE) - 1

# Calculate total return for period 1
green_return_p1 <- prod(1 + period1_returns$Green_monthly_return, na.rm = TRUE) - 1
nbp_return_p1   <- prod(1 + period1_returns$NBP_monthly_return, na.rm = TRUE) - 1

# Calculate total return for period 2
green_return_p2 <- prod(1 + period2_returns$Green_monthly_return, na.rm = TRUE) - 1
nbp_return_p2   <- prod(1 + period2_returns$NBP_monthly_return, na.rm = TRUE) - 1

# Create table
return_table <- data.frame(
  Period = c("Period 1", "Period 2", "Full Period"),
  `Green HY Index (%)` = round(c(green_return_p1, green_return_p2, green_total_return) * 100, 2),
  `NBP Nordic HY Index (%)` = round(c(nbp_return_p1, nbp_return_p2, nbp_total_return) * 100, 2)
)

# Present with stargazer
stargazer(return_table, type = "text", summary = FALSE,
          title = "Total Return by Period (in %)", digits = 2)


# ============================================
#      Performance Measures (full period)
# ============================================

# ------------ TRACKING ERROR ----------------

# Calculate the monthly return differences between Green and NBP indices
green_nbp_diff_monthly <- monthly_returns$Green_monthly_return - monthly_returns$NBP_monthly_return
diff_p1 <- period1_returns$Green_monthly_return - period1_returns$NBP_monthly_return
diff_p2 <- period2_returns$Green_monthly_return - period2_returns$NBP_monthly_return

# Calculate monthly tracking error (standard deviation of differences)
tracking_error_monthly <- sd(green_nbp_diff_monthly, na.rm = TRUE)
te_p1_monthly <- sd(diff_p1, na.rm = TRUE)
te_p2_monthly <- sd(diff_p2, na.rm = TRUE)

# Create table for monthly tracking error (non-annualized), in %
tracking_error_monthly_table <- data.frame(
  Period = c("Period 1", "Period 2", "Full Period"),
  `Tracking Error (%)` = round(c(te_p1_monthly, te_p2_monthly, tracking_error_monthly) * 100, 2)
)

# Display with Stargazer
stargazer(tracking_error_monthly_table, type = "text", summary = FALSE,
          title = "Tracking Error per Period (Monthly, %)", digits = 2)

# Annualize the tracking errors (multiply by sqrt(12))
tracking_error_annualized <- tracking_error_monthly * sqrt(12)
te_p1_annual <- te_p1_monthly * sqrt(12)
te_p2_annual <- te_p2_monthly * sqrt(12)

# Create summary table
tracking_error_table <- data.frame(
  Period = c("Period 1", "Period 2", "Full Period"),
  `Tracking Error (%)` = round(c(te_p1_annual, te_p2_annual, tracking_error_annualized) * 100, 2)
)

# Display table with Stargazer
stargazer(tracking_error_table, type = "text", summary = FALSE,
          title = "Tracking Error per Period (Annualized, %)", digits = 2)


# ------------ EXCESS RETURNS (Monthly) ----------------

### Prepare risk-free rate
# Convert 'Time Period' to Date
rf$Date <- as.Date(rf$`Time Period`)

# Create YearMonth variable
rf$YearMonth <- floor_date(rf$Date, "month")

# Convert annual rate (in %) to decimal
rf$Rate_decimal <- rf$Rate / 100

# Compute monthly average risk-free rate
rf_monthly <- rf %>%
  group_by(YearMonth) %>%
  summarise(
    avg_rf_annual = mean(Rate_decimal, na.rm = TRUE),  # Average annual rate in decimal form
    .groups = "drop"
  )

# Convert to monthly rate
rf_monthly <- rf_monthly %>%
  mutate(
    RiskFree_monthly = avg_rf_annual / 12   # Divide by 12 to get monthly
  )

# Merge monthly risk-free rate with returns
monthly_returns <- left_join(monthly_returns, rf_monthly[, c("YearMonth", "RiskFree_monthly")], by = "YearMonth")

# Check: 
view(monthly_returns)


### Calculate Excess Returns
monthly_returns <- monthly_returns %>%
  mutate(
    Green_ExcessReturn = Green_monthly_return - RiskFree_monthly,
    NBP_ExcessReturn = NBP_monthly_return - RiskFree_monthly
  )

# View new data:
head(monthly_returns)


# ------------ SHARPE RATIO (monthly) ----------------

# Mean of excess return (monthly)
mean_excess_green_monthly <- mean(monthly_returns$Green_ExcessReturn, na.rm = TRUE)
mean_excess_nbp_monthly <- mean(monthly_returns$NBP_ExcessReturn, na.rm = TRUE)

# Std. dev of total return (monthly)
sd_green_monthly <- sd(monthly_returns$Green_monthly_return, na.rm = TRUE)
sd_nbp_monthly <- sd(monthly_returns$NBP_monthly_return, na.rm = TRUE)

# Annualized std.dev
sd_green_annualized <- sd_green_monthly * sqrt(12)
sd_nbp_annualized <- sd_nbp_monthly * sqrt(12)

# Sharpe ratios
SharpeRatio_green_monthly <- mean_excess_green_monthly / sd_green_annualized
SharpeRatio_nbp_monthly <- mean_excess_nbp_monthly / sd_nbp_annualized

# View
SharpeRatio_green_monthly
SharpeRatio_nbp_monthly

# Table with results
SharpeRatio_monthly_table <- data.frame(
  Index = c("Green HY Index", "NBP Nordic HY Index"),
  `Sharpe Ratio (Monthly)` = c(SharpeRatio_green_monthly, SharpeRatio_nbp_monthly)
)

# Show with stargazer
stargazer(SharpeRatio_monthly_table, type = "text", summary = FALSE,
          title = "Monthly Sharpe Ratio",
          digits = 3)


# ------------ SHARPE RATIO (annualized) ----------------

# Annualized Sharpe Ratio
SharpeRatio_green_annual <- mean_excess_green_monthly * 12 / sd_green_annualized
SharpeRatio_nbp_annual <- mean_excess_nbp_monthly * 12 / sd_nbp_annualized

# View
SharpeRatio_green_annual
SharpeRatio_nbp_annual

# Sharpe Ratio difference
sharpe_diff_full <- SharpeRatio_green_annual - SharpeRatio_nbp_annual

# Table
SharpeRatio_annual_table <- data.frame(
  Index = c("Green HY Index", "NBP Nordic HY Index", "Sharpe Ratio Difference"),
  `Sharpe Ratio (Annualized)` = c(SharpeRatio_green_annual, SharpeRatio_nbp_annual,sharpe_diff_full)
)

# Show with stargazer
stargazer(SharpeRatio_annual_table, type = "text", summary = FALSE,
          title = "Annualized Sharpe Ratio",
          digits = 3)


# ------------ INFORMATION RATIO (monthly) ----------------

# Mean of relative monthly return differences
mean_green_nbp_diff_monthly <- mean(green_nbp_diff_monthly, na.rm = TRUE)

# Monthly Information Ratio, using the tracking error from above
information_ratio_monthly <- mean_green_nbp_diff_monthly / tracking_error_monthly 

# View
information_ratio_monthly


# ------------ INFORMATION RATIO (annualized) ----------------

#  Multiply by sqrt(12) to annualize it
information_ratio_annualized <- information_ratio_monthly * sqrt(12)
information_ratio_annualized


# ============================================
#      Performance Measures (Period 1)
# ============================================

# ---------------- EXCESS RETURNS ----------------

# Define risk-free rate for period 1
rf_p1 <- rf_monthly %>%
  filter(YearMonth >= as.Date("2021-12-01") & YearMonth <= as.Date("2023-12-01"))

# Merge period 1 risk free rate with returns
period1_returns <- left_join(period1_returns, rf_p1[, c("YearMonth", "RiskFree_monthly")], by = "YearMonth") 

# Check: 
view (period1_returns)

# Calculate Excess Returns
period1_returns <- period1_returns %>%
  mutate(
    Green_ExcessReturn_p1 = Green_monthly_return - RiskFree_monthly,
    NBP_ExcessReturn_p1 = NBP_monthly_return - RiskFree_monthly
  )

# View new data:
head(period1_returns)


# ---------------- SHARPE RATIO ----------------
# Mean excess return
mean_excess_green_p1 <- mean(period1_returns$Green_ExcessReturn_p1, na.rm = TRUE)
mean_excess_nbp_p1   <- mean(period1_returns$NBP_ExcessReturn_p1, na.rm = TRUE)

# Std.dev of total return
sd_green_p1 <- sd(period1_returns$Green_monthly_return, na.rm = TRUE)
sd_nbp_p1   <- sd(period1_returns$NBP_monthly_return, na.rm = TRUE)

# Annualized std.dev
sd_green_p1_annual <- sd_green_p1 * sqrt(12)
sd_nbp_p1_annual   <- sd_nbp_p1 * sqrt(12)

# Sharpe Ratios (annualized)
sharpe_green_p1 <- mean_excess_green_p1 * 12 / sd_green_p1_annual
sharpe_nbp_p1   <- mean_excess_nbp_p1 * 12 / sd_nbp_p1_annual

sharpe_diff_p1 <- sharpe_green_p1 - sharpe_nbp_p1

# Table
sharpe_table_p1 <- data.frame(
  Index = c("Green HY Index", "NBP Nordic HY Index", "Sharpe Ratio Difference"),
  `Sharpe Ratio (Annualized)` = round(c(sharpe_green_p1, sharpe_nbp_p1, sharpe_diff_p1), 3)
)

stargazer(sharpe_table_p1, type = "text", summary = FALSE,
          title = "Annualized Sharpe Ratio - Period 1",
          digits = 3)

# ---------------- INFORMATION RATIO ----------------
mean_diff_p1 <- mean(diff_p1, na.rm = TRUE)
info_ratio_p1 <- mean_diff_p1 / te_p1_monthly
info_ratio_p1_annual <- info_ratio_p1 * sqrt(12)

# Output
cat("Information Ratio (Annualized) - Period 1:", round(info_ratio_p1_annual, 3), "\n")


# ============================================
#      Performance Measures (Period 2)
# ============================================

# ---------------- EXCESS RETURNS ----------------

# Define risk-free rate for period 2
rf_p2 <- rf_monthly %>%
  filter(YearMonth >= as.Date("2024-01-01") & YearMonth <= as.Date("2025-03-01"))

# Merge period 2 risk-free rate with returns
period2_returns <- left_join(period2_returns, rf_p2[, c("YearMonth", "RiskFree_monthly")], by = "YearMonth") 

# Check:
view(period2_returns)

# Calculate Excess Returns
period2_returns <- period2_returns %>%
  mutate(
    Green_ExcessReturn_p2 = Green_monthly_return - RiskFree_monthly,
    NBP_ExcessReturn_p2   = NBP_monthly_return - RiskFree_monthly
  )

# View new data:
head(period2_returns)


# ---------------- SHARPE RATIO ----------------
# Mean excess return
mean_excess_green_p2 <- mean(period2_returns$Green_ExcessReturn_p2, na.rm = TRUE)
mean_excess_nbp_p2   <- mean(period2_returns$NBP_ExcessReturn_p2, na.rm = TRUE)

# Std.dev of total return
sd_green_p2 <- sd(period2_returns$Green_monthly_return, na.rm = TRUE)
sd_nbp_p2   <- sd(period2_returns$NBP_monthly_return, na.rm = TRUE)

# Annualized std.dev
sd_green_p2_annual <- sd_green_p2 * sqrt(12)
sd_nbp_p2_annual   <- sd_nbp_p2 * sqrt(12)

# Sharpe Ratios (annualized)
sharpe_green_p2 <- mean_excess_green_p2 * 12 / sd_green_p2_annual
sharpe_nbp_p2   <- mean_excess_nbp_p2 * 12 / sd_nbp_p2_annual

sharpe_diff_p2 <- sharpe_green_p2 - sharpe_nbp_p2

# Table
sharpe_table_p2 <- data.frame(
  Index = c("Green HY Index", "NBP Nordic HY Index", "Sharpe Ratio Difference"),
  `Sharpe Ratio (Annualized)` = round(c(sharpe_green_p2, sharpe_nbp_p2, sharpe_diff_p2), 3)
)

stargazer(sharpe_table_p2, type = "text", summary = FALSE,
          title = "Annualized Sharpe Ratio - Period 2",
          digits = 3)

# ---------------- INFORMATION RATIO ----------------
mean_diff_p2 <- mean(diff_p2, na.rm = TRUE)
info_ratio_p2 <- mean_diff_p2 / te_p2_monthly
info_ratio_p2_annual <- info_ratio_p2 * sqrt(12)

# Output
cat("Information Ratio (Annualized) - Period 2:", round(info_ratio_p2_annual, 3), "\n")


# ===================================================
#  Summary Table: Tracking Error, Sharpe Ratio & IR
# ===================================================

# Create table with Green and NBP metrics separated
perf_table <- data.frame(
  Metric = c(
    "Tracking Error (%)",
    "Sharpe Ratio (Green, Annualized)",
    "Sharpe Ratio (NBP, Annualized)",
    "Information Ratio (Green vs NBP)"
  ),
  `Period 1` = round(c(
    te_p1_annual * 100,
    sharpe_green_p1,
    sharpe_nbp_p1,
    info_ratio_p1_annual
  ), 3),
  `Period 2` = round(c(
    te_p2_annual * 100,
    sharpe_green_p2,
    sharpe_nbp_p2,
    info_ratio_p2_annual
  ), 3),
  `Full Period` = round(c(
    tracking_error_annualized * 100,
    SharpeRatio_green_annual,
    SharpeRatio_nbp_annual,
    information_ratio_annualized
  ), 3)
)

# Present with stargazer
stargazer(perf_table, type = "latex", summary = FALSE,
          title = "Performance Summary by Period",
          digits = 3)


# ============================================
#      Fixing two periods for the indices
# ============================================

# Ensure that the dates are in the correct format
green_index$PriceDate <- as.Date(green_index$PriceDate)
nbp_index$PriceDate <- as.Date(nbp_index$PriceDate)

# Define periods
p1_start <- as.Date("2021-12-30")
p1_end   <- as.Date("2023-12-29")
p2_start <- as.Date("2024-01-02")
p2_end   <- as.Date("2025-03-10")

# Filter for each period
green_p1 <- green_index %>% filter(PriceDate >= p1_start & PriceDate <= p1_end)
green_p2 <- green_index %>% filter(PriceDate >= p2_start & PriceDate <= p2_end)

nbp_p1 <- nbp_index %>% filter(PriceDate >= p1_start & PriceDate <= p1_end)
nbp_p2 <- nbp_index %>% filter(PriceDate >= p2_start & PriceDate <= p2_end)


# ----------- Average Spread --------------

# Calculate average spreads
avg_spread_table <- data.frame(
  Period = c("Period 1", "Period 2", "Full Period"),
  `Green HY Index` = round(c(
    mean(green_p1$AverageSpread, na.rm = TRUE),
    mean(green_p2$AverageSpread, na.rm = TRUE),
    mean(green_index$AverageSpread, na.rm = TRUE)
  ), 2),
  `NBP Nordic HY Index` = round(c(
    mean(nbp_p1$AverageSpread, na.rm = TRUE),
    mean(nbp_p2$AverageSpread, na.rm = TRUE),
    mean(nbp_index$AverageSpread, na.rm = TRUE)
  ), 2)
)

# Display the table with Stargazer
stargazer(avg_spread_table, type = "text", summary = FALSE,
          title = "Average Spread by Period (in bps)",
          digits = 2)

# Correlation in spread between the two indices
cor_spread <- cor(green_index$AverageSpread,nbp_index$AverageSpread, use = "complete.obs", method = "pearson")
print(cor_spread)
cor_spread_p1 <- cor(green_p1$AverageSpread,nbp_p1$AverageSpread, use = "complete.obs", method = "pearson")
print(cor_spread_p1)
cor_spread_p2 <- cor(green_p2$AverageSpread,nbp_p2$AverageSpread, use = "complete.obs", method = "pearson")
print(cor_spread_p2)

# Create dataframe
spread_df <- data.frame(
  Date = green_index$PriceDate,
  Green_Spread = green_index$AverageSpread,
  NBP_Spread = nbp_index$AverageSpread
)

# Filter to remove 2021
spread_df <- spread_df %>%
  filter(Date >= as.Date("2022-01-01"))

# Convert to long format
spread_long <- spread_df %>%
  pivot_longer(cols = c(Green_Spread, NBP_Spread),
               names_to = "Index",
               values_to = "Spread")

# Plot
ggplot(spread_long, aes(x = Date, y = Spread, color = Index)) +
  geom_line(size = 1) +
  labs(
    title = "Spread Development Over Time",
    subtitle = "Green HY Index vs NBP Nordic HY Index",
    x = "Date",
    y = "Average Spread (bps)",
    color = NULL
  ) +
  scale_color_manual(
    values = c("Green_Spread" = "forestgreen", "NBP_Spread" = "steelblue"),
    labels = c("Green HY Index", "NBP Nordic HY Index")
  ) +
  scale_y_continuous(
    breaks = seq(300, 800, by = 100),
    limits = c(350, 800),
    labels = scales::comma_format(accuracy = 1)
  ) +
  scale_x_date(
    breaks = seq(as.Date("2021-12-01"), as.Date("2025-03-01"), by = "3 months"),
    date_labels = "%b %Y",
    limits = c(min(spread_long$Date), as.Date("2025-03-31")),
    expand = c(0, 0)
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 13, hjust = 0.5),
    plot.subtitle = element_text(size = 11, hjust = 0.5),
    legend.position = "top",
    legend.text = element_text(size = 10),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )


# ============================================
#       Development in Spread (monthly)
# ============================================

# ------------ Prepare monthly data ----------------

# Ensure correct date format and remove any invalid values
green_index$PriceDate <- as.Date(green_index$PriceDate)
nbp_index$PriceDate <- as.Date(nbp_index$PriceDate)

green_index <- green_index %>% filter(AverageSpread > 0)
nbp_index <- nbp_index %>% filter(AverageSpread > 0)

# Create YearMonth
green_index$YearMonth <- floor_date(green_index$PriceDate, "month")
nbp_index$YearMonth   <- floor_date(nbp_index$PriceDate, "month")

# Calculate monthly average for each index
green_monthly <- green_index %>%
  group_by(YearMonth) %>%
  summarise(
    Green_Spread = mean(AverageSpread, na.rm = TRUE)
  )

nbp_monthly <- nbp_index %>%
  group_by(YearMonth) %>%
  summarise(
    NBP_Spread = mean(AverageSpread, na.rm = TRUE)
  )

# Combine
combined_monthly <- left_join(green_monthly, nbp_monthly, by = "YearMonth")


# ------------ Spread Development ----------------

spread_long <- combined_monthly %>%
  pivot_longer(cols = c(Green_Spread, NBP_Spread),
               names_to = "Index", values_to = "Spread")

ggplot(spread_long, aes(x = YearMonth, y = Spread, color = Index)) +
  geom_line(size = 1) +
  labs(
    title = "Monthly Spread Development",
    subtitle = "Green HY Index vs NBP Nordic HY Index",
    x = "Month",
    y = "Average Spread (bps)",
    color = NULL
  ) +
  scale_color_manual(
    values = c("Green_Spread" = "forestgreen", "NBP_Spread" = "steelblue"),
    labels = c("Green HY Index", "NBP Nordic HY Index")
  ) +
  scale_y_continuous(labels = comma_format()) +
  scale_x_date(
    breaks = seq(as.Date("2021-12-01"), as.Date("2025-03-01"), by = "3 months"),
    date_labels = "%b %Y",
    limits = c(min(spread_long$YearMonth, na.rm = TRUE), as.Date("2025-03-31")),
    expand = c(0, 0)
  )+
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 13, hjust = 0.5),
    plot.subtitle = element_text(size = 11, hjust = 0.5),
    legend.position = "top",
    axis.text.x = element_text(angle = 45, hjust = 1)
  )


ggplot(spread_long, aes(x = YearMonth, y = Spread, color = Index)) +
  geom_line(size = 1) +
  labs(
    x = "Month",
    y = "Average Spread (bps)",
    color = NULL
  ) +
  scale_color_manual(
    values = c("Green_Spread" = "forestgreen", "NBP_Spread" = "steelblue"),
    labels = c("Green HY Index", "NBP Nordic HY Index")
  ) +
  scale_y_continuous(labels = comma_format()) +
  scale_x_date(
    breaks = seq(as.Date("2021-12-01"), as.Date("2025-03-01"), by = "3 months"),
    date_labels = "%b %Y",
    limits = c(min(spread_long$YearMonth, na.rm = TRUE), as.Date("2025-03-31")),
    expand = c(0, 0)
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 13, hjust = 0.5),
    plot.subtitle = element_text(size = 11, hjust = 0.5),
    axis.text.x = element_text(size = 14, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 14),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    legend.text = element_text(size = 14),
    legend.position = "top"
  )


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

ggplot(plot_data_bidask, aes(x = PriceDate, y = `Average bid-ask spread`)) +
  geom_line(color = "forestgreen", size = 0.8) +
  labs(
    x = "Date",
    y = "Bid-Ask Spread",
    color = NULL
  ) +
  scale_y_continuous(labels = comma_format()) +
  scale_x_date(
    breaks = seq(as.Date("2021-12-01"), as.Date("2025-03-01"), by = "3 months"),
    date_labels = "%b %Y",
    limits = c(min(spread_long$YearMonth, na.rm = TRUE), as.Date("2025-03-31")),
    expand = c(0, 0)
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 13, hjust = 0.5),
    plot.subtitle = element_text(size = 11, hjust = 0.5),
    axis.text.x = element_text(size = 14, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 14),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    legend.text = element_text(size = 14),
    legend.position = "top"
  )

# ------------ Bid-ask spread monthly ----------------

# Legg til ny kolonne i green_index med spread i bps
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


# ============================================
#    Correlation between spread and bid-ask
# ============================================

# ------------ Daily ----------------

## GREEN
# Daily correlation (Green HY Index)
daily_data_green_2 <- green_index %>%
  select(Date = PriceDate,
         Spread = AverageSpread,
         BidAsk  = `Average bid-ask spread`) %>%
  drop_na()

# Calculate correlation matrix
cor_matrix_daily_green_2 <- cor(daily_data_green_2[, -1], use = "complete.obs")
print(cor_matrix_daily_green_2)


# ------------ Monthly ----------------

# Add month
green_index$YearMonth <- floor_date(green_index$PriceDate, "month")

## GREEN
# Aggregate: take average per month
monthly_data_green_2 <- green_index %>%
  group_by(YearMonth) %>%
  summarise(
    Spread = mean(AverageSpread, na.rm = TRUE),
    BidAsk  = mean(`Average bid-ask spread`, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  drop_na()

# Calculate correlation matrix
cor_matrix_monthly_green_2 <- cor(monthly_data_green_2[, -1], use = "complete.obs")
print(cor_matrix_monthly_green_2)

# Stargazer
stargazer(cor_matrix_monthly_green_2, type = "text", title = "Correlation Matrix: Green HY Index", digits = 3)

