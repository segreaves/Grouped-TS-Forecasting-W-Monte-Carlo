# Author: Simon Greaves
# Date: 2024-12-17

pkgs <- c("dplyr", "forecast", "ggplot2", "tidyquant", "lubridate")
inst <- lapply(pkgs, library, character.only = TRUE)

simulate_time_series <- function(n, id, start_date, min_value, max_value, growth_rate, weekday_effects, yearly_seasonal_comp, weekly_seasonal_comp) {
  # Daily data
  freq <- 365
  trend <- min_value + max_value / (1 + exp(-growth_rate * (seq_len(n))))
  # Yearly seasonality
  yearly_seasonal <- yearly_seasonal_comp * sin(2 * pi * seq_len(n) / freq)
  # Weekly seasonality
  weekly_seasonal <- weekly_seasonal_comp * sin(2 * pi * seq_len(n) / 7)
  seasonal <- yearly_seasonal + weekly_seasonal
  # Random noise
  noise <- rnorm(n, mean = 0, sd = 2)  # Reduced noise for daily data
  # Combine components
  y <- trend + seasonal + noise
  # Create date vector starting at start_date
  time <- seq(start_date, by = "day", length.out = n)
  # Set up weekday effects
  effects <- data.frame(weekday = c("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"),
                        effect = weekday_effects)
  data <- data.frame(date=time, value=y, id=id, weekday = wday(time, label = TRUE))
  data <- add_weekday_effect(df=data, effects) |>
    select(date, id, value)
  return(data)
}

add_weekday_effect <- function(df, weekday_effects) {
  df <- df |>
    left_join(weekday_effects, by = "weekday") |>
    mutate(value = value * runif(n(), effect - 0.025, effect + 0.025))
  return(df)
}

# Set seed
set.seed(123)
# Set up time series parameters
start_date <- as.Date("2020-01-01")
train_test_threshold <- as.Date("2024-01-01")
n <- 365 * 5
ts_1 <- simulate_time_series(n=n, id="1", start_date=start_date, min_value=100, max_value=200, growth_rate=0.0025, weekday_effects = c(1, 1, 1.08, 1, 1, 1, 1), yearly_seasonal_comp=5, weekly_seasonal_comp=5)
ts_2 <- simulate_time_series(n=n, id="2", start_date=start_date, min_value=200, max_value=100, growth_rate=0.005, weekday_effects = c(1, 1, 1, 1, 0.95, 1, 1), yearly_seasonal_comp=2, weekly_seasonal_comp=7)
ts_3 <- simulate_time_series(n=n, id="3", start_date=start_date, min_value=200, max_value=100, growth_rate=-0.005, weekday_effects = c(0.93, 1, 1, 1, 1, 1, 1), yearly_seasonal_comp=7, weekly_seasonal_comp=3)

# Merge all data into one dataframe
df <- rbind(ts_1, ts_2, ts_3)

# Plot the values
df |>
  ggplot(aes(x = date, y = value, group = id, color = id)) +
  geom_point() +
  labs(title = "Simulated Time Series", x = "", y = "Value",
       subtitle = "Three time series with different characteristics") +
  scale_y_continuous() +
  theme_tq()

# Aggregate into one dataframe
df_agg <- df |>
  group_by(date) |>
  summarize(value = sum(value)) |>
  mutate(train = date < train_test_threshold)

# Plot the grouped dataframe
df_agg |>
  ggplot(aes(x = date, y = value, group = train, color = train)) +
  geom_point() +
  labs(title = "Grouped Time Series", x = "", y = "Value",
       subtitle = "Sum of the three time series") +
  scale_y_continuous() +
  theme_tq()