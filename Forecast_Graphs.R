library(tsibble)
library(fable)
library(feasts)
library(urca)
library(shiny)
library(lubridate)

##Note that code here follows from part (a) code (run that first)

full_data <- data_base %>%
  left_join(label_table, by = c("NAICS_5" = "naics_5"))

##Include 2024 since we're going by month
full_data <- full_data[full_data$NAICS_5 != 5211, ]
analysis_data <- full_data[,1:8]

##Collapse by month and year
analysis_data_m <- analysis_data %>%
  group_by(SYEAR, SMTH, LF_STAT, lmo_ind_code, aggregate_industry, lmo_detailed_industry) %>%
  summarise(X_COUNT_ = sum(X_COUNT_), .groups = "drop")

##View data
head(analysis_data_m)

##Total people surveyed each month
monthly_totals <- full_data %>%
  group_by(SYEAR, SMTH) %>%
  summarise(total_people = sum(X_COUNT_, na.rm = TRUE), .groups = "drop")

##Include monthly totals for proportions
analysis_data_m <- analysis_data_m %>%
  left_join(monthly_totals, by = c("SYEAR" = "SYEAR", "SMTH" = "SMTH"))

##Calculate proportion
analysis_data_m$X_PCT_ <- analysis_data_m$X_COUNT_/analysis_data_m$total_people

##Only care about employed people
model_data <- analysis_data_m %>%
  filter(LF_STAT == "Employed")

##Change data to time series format for analysis
ts_data <- model_data %>%
  mutate(Date = yearmonth(paste(SYEAR, SMTH, sep = "-"))) %>%
  as_tsibble(key = lmo_detailed_industry, index = Date) %>%
  arrange(lmo_detailed_industry, Date)

##Only important variables for analysis  
ts_data_a <- ts_data[,c(6,9,10)]

##Fit seasonal ARIMA to each industry, accounts for seasonal
##hiring/firing trends, etc.
model_fit <- ts_data_a %>%
  model(SARIMA = ARIMA(X_PCT_ ~ season()))

##Save for RShiny app
saveRDS(ts_data, "ts_data.rds")
saveRDS(model_fit, "model_fit.rds")

##Forecast the next 12 months
forecasts <- model_fit %>%
  forecast(h = "12 months")

##Create plots
plts <- forecasts %>%
  autoplot(ts_data, level = NULL) +
  facet_wrap(~ lmo_detailed_industry, scales = "free_y") +
  labs(title = "Seasonal-ARIMA Forecasting Proportion of Population Employed in each Industry") + 
  theme_minimal()

##Save plots as a pdf
ggsave("DANIEL_POLSON_forecast.pdf", plts, width = 35, height = 30, dpi = 300)

##Save predictions in excel sheet  
preds <- read.xlsx("forecast_template.xlsx")

##Get dates in ymd formatting
dates <- forecasts$Date
dates_start <- ymd(paste0(dates, "-01"))
dates_end <- ceiling_date(dates_start, "month") - days(1)

preds[1:nrow(forecasts), 1:3] <- 0

preds$`Date.(ymd.format)` <- dates_end
preds$lmo_detailed_industry <- forecasts$lmo_detailed_industry
preds$forecast <- forecasts$.mean

library(writexl)

##Export for part b)
write_xlsx(preds, "DANIEL_POLSON_forecast_template.xlsx")









