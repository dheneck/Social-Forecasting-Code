# Packages
library(readxl)
library(ggplot2)
library(forecast)
library(lubridate)
library(tidyr)
library(dplyr)
library(kableExtra)

sentiment <- read.csv("C:\\Users\\dhene\\Downloads\\tbqics.csv")

# Convert to a ts object
cs.ts <- ts(sentiment$ICS_ALL,
            start = c(1960, 1),  
            frequency = 4) 

autoplot(cs.ts) +
  labs(title = "Consumer Sentiment Over Time", y = "Sentiment Index", x = "Year")

# Training Period 
n_total <- length(cs.ts)         # Total number of observations
n_train <- floor(0.8 * n_total)  # 80% training
n_test  <- n_total - n_train     # 20% testing


# Split into training and test set
train_ts <- window(cs.ts, end = time(cs.ts)[n_train])
test_ts  <- window(cs.ts, start = time(cs.ts)[n_train + 1])

######################################
# Check Training Data
######################################

# ACF
ggtsdisplay(train_ts, main = "Consumer Sentiment Index Time Series (Training Set)")

# First difference
ggtsdisplay(diff(train_ts), main = "First Differenced Time Series")

# Decompose 
decomp <- stl(train_ts, s.window = 13)
autoplot(decomp, main = "STL Decomposition of Consumer Sentiment Index")

#######################################
# Baseline ARIMA Model
#######################################

# With parameters 
model_arima <- auto.arima(train_ts, 
                    stepwise = FALSE, 
                    approximation = FALSE)
forecasted_test_arima <- forecast(model, h = n_test)

###################################
# MAE and MAPE Comparisons 
###################################

# Forecast horizons
h <- length(test_ts)

# Final fitted models
model_arima_custom1 <- Arima(train_ts, order = c(1,0,3), seasonal = list(order = c(1,0,0), period = 4))
model_arima_custom2 <- Arima(train_ts, order = c(1,0,2), seasonal = list(order = c(1,0,0), period = 4))
model_arima_custom3 <- Arima(train_ts, order = c(1,0,4), seasonal = list(order = c(1,0,0), period = 4))
model_arima_custom4 <- Arima(train_ts, order = c(2,0,3), seasonal = list(order = c(1,0,0), period = 4))
model_arima_custom5 <- Arima(train_ts, order = c(1,0,3), seasonal = list(order = c(1,0,1), period = 4))
model_arima_custom6 <- Arima(train_ts, order = c(1,0,3), seasonal = list(order = c(1,0,2), period = 4))

# Forecasts
fc_arima_custom1 <- forecast(model_arima_custom1, h = h)
fc_arima_custom2 <- forecast(model_arima_custom2, h = h)
fc_arima_custom3 <- forecast(model_arima_custom3, h = h)
fc_arima_custom4 <- forecast(model_arima_custom4, h = h)
fc_arima_custom5 <- forecast(model_arima_custom5, h = h)
fc_arima_custom6 <- forecast(model_arima_custom6, h = h)

# Accuracy 
acc_arima_custom1 <- accuracy(fc_arima_custom1, test_ts)
acc_arima_custom2 <- accuracy(fc_arima_custom2, test_ts)
acc_arima_custom3 <- accuracy(fc_arima_custom3, test_ts)
acc_arima_custom4 <- accuracy(fc_arima_custom4, test_ts)
acc_arima_custom5 <- accuracy(fc_arima_custom5, test_ts)
acc_arima_custom6 <- accuracy(fc_arima_custom6, test_ts)

# Summary table
accuracy_table <- data.frame(
  Model = c("ARIMA_custom1", "ARIMA_custom2",
            "ARIMA_custom3", "ARIMA_custom4",  
            "ARIMA_custom5", "ARIMA_custom6"),
  MAE = c(acc_arima_custom1["Test set", "MAE"],
          acc_arima_custom2["Test set", "MAE"],
          acc_arima_custom3["Test set", "MAE"],
          acc_arima_custom4["Test set", "MAE"],
          acc_arima_custom5["Test set", "MAE"],
          acc_arima_custom6["Test set", "MAE"]),
  MAPE = c(acc_arima_custom1["Test set", "MAPE"],
           acc_arima_custom2["Test set", "MAPE"],
           acc_arima_custom3["Test set", "MAPE"],
           acc_arima_custom4["Test set", "MAPE"],
           acc_arima_custom5["Test set", "MAPE"],
           acc_arima_custom6["Test set", "MAPE"])
)

print(accuracy_table)

# Results
accuracy_table <- data.frame(
  Model = c("ARIMA(1,0,3)(1,0,1)[4]", "ARIMA(1,0,2)(1,0,0)[4]", 
            "ARIMA(1,0,4)(1,0,0)[4]", "ARIMA(2,0,3)(1,0,0)[4]", 
            "ARIMA(1,0,3)(1,0,0)[4]", "ARIMA(1,0,3)(1,0,2)[4]"),
  MAE   = c(12.24408, 12.41517, 12.37815, 12.44583, 11.96921, 12.70375),
  MAPE  = c(16.12879, 16.31202, 16.28240, 16.35122, 15.85799, 16.49879)
)

# Convert to long format 
accuracy_long <- accuracy_table %>%
  pivot_longer(cols = c("MAE", "MAPE"), names_to = "Metric", values_to = "Value")

# Plot Accuracy 
ggplot(accuracy_long, aes(x = Model, y = Value, fill = Metric)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.7), width = 0.6) +
  geom_text(aes(label = round(Value, 2)),
            position = position_dodge(width = 0.7),
            vjust = -0.6, size = 3.5, family = "roboto") +
  scale_fill_manual(values = c("skyblue4", "firebrick3")) +
  labs(y = "Error Value", x = "Model") +
  theme_minimal(base_family = "roboto") +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    axis.text.x = element_text(angle = 15, hjust = 1),
    legend.title = element_blank(),
    legend.position = "right"
  )

#######################################
# Final model fit
#######################################
# Forecast using final model (e.g., custom ARIMA)
fit_final <- Arima(train_ts, order = c(1, 0, 3), seasonal = list(order = c(1, 0, 0), period = 4))
fc_final <- forecast(fit_final, h = length(test_ts))

# Summary
summary(fit_final)

# Forecast table
kable(fc_final, main = "Point Forecasts for Test Period")

# Check Residuals
checkresiduals(fit_final)

# Combine training, forecast, and actuals into one plot
autoplot(fc_final, PI = TRUE) +
  autolayer(test_ts, series = "Actual") +
  labs(
       y = "Consumer Sentiment",
       x = "Time") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"),
        legend.position = "right")

########################################
# Final model fit (test set)
########################################

# Forecast the length of the test set
h <- length(test_ts)
fc <- forecast(fit_final, h = h)

# Create range
recent_data <- window(cs.ts, start = c(2012, 3))
forecast_range <- time(recent_data)

# Plot forecast and actuals for test data range
autoplot(recent_data) +
  autolayer(fc, series = "Forecast") +
  autolayer(test_ts, series = "Actual", linetype = "dashed", color = "black") +
  labs(
    x = "Time",
    y = "Consumer Sentiment"
  ) +
  scale_x_continuous(breaks = seq(2012, 2025, by = 1)) +  # Fixes x-axis to whole years
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold"),
    legend.title = element_blank()
  )

####################################### 
# MAE and MAPE for Final Model
#######################################

# Extract MAE, MAPE and RMSE
acc <- accuracy(fc, test_ts)
results <- acc[, c("MAE", "MAPE", "RMSE")]
rownames(results) <- c("Training", "Test")

# Print table
kable(round(results, 2), caption = "Forecast Accuracy Metrics for SARIMA(1,0,3)(1,0,0)[4]")
