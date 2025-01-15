# Author: Simon Greaves
# Date: 2024-12-16

pkgs <- c("Rcpp", "prophet", "purrr")
inst <- lapply(pkgs, library, character.only = TRUE)

# Individual clinic forecasting
forecast_series <- function(dat, series_id, forecast_horizon = 365) {
  # Add regressors
  start_date <- as.Date(min(dat$ds))
  dat <- dat |>
    mutate(weekday = wday(ds, label = TRUE),
           time_regressor = as.numeric(as.Date(ds) - start_date),
           time_regressor_log = log(time_regressor + 1))

  # Prepare the data for Prophet
  m <- prophet(daily.seasonality = FALSE,
               weekly.seasonality = TRUE,
               yearly.seasonality = TRUE,
               growth = 'linear',
               changepoint_range = 0.95,
               seasonality.mode="multiplicative") |>
    add_regressor('weekday') |>
    add_regressor('time_regressor') |>
    add_regressor('time_regressor_log') |>
    fit.prophet(dat |> select(ds, y, weekday, time_regressor, time_regressor_log))
  
  # Create future dataframe
  future <- make_future_dataframe(m, periods = forecast_horizon) |>
    mutate(weekday = wday(ds, label = TRUE),
           time_regressor = as.numeric(as.Date(ds) - start_date),
           time_regressor_log = log(time_regressor + 1))
  
  # Make forecast
  prophet_forecast <- predict(m, future)
  
  # Select only the columns we need and rename to match input data
  forecast_df <- prophet_forecast |>
    select(ds, yhat, yhat_lower, yhat_upper) |>
    rename(y = yhat) |>
    mutate(id = series_id,
           weekday = wday(ds, label = TRUE),
           train = FALSE,
           ds = as.Date(ds))
  
  # Reorder columns to match input data
  forecast_df <- forecast_df |>
    select(id, ds, y, train, weekday, yhat_lower, yhat_upper)
  
  return(forecast_df)
}

plot_series_forecast <- function(series_data, forecasts, series_id, start_date) {
  # Combine actual data and forecasts
  combined_data <- bind_rows(
    series_data |>
      filter(id == series_id & ds >= start_date) |>
      mutate(type = "Actual"),
    forecasts |>
      filter(id == series_id & ds >= start_date) |>
      mutate(type = "Forecast")) |>
    mutate(weekday = wday(ds, label = TRUE))
  
  ggplot(combined_data, aes(x = ds, y = y)) +
    geom_point(data = combined_data |> filter(type == "Actual"),
               aes(color = weekday), alpha = 0.6) +
    geom_line(data = combined_data |> filter(type == "Forecast"),
              color = "blue", size = 1, alpha = 0.5) +
    geom_ribbon(data = combined_data |> filter(type == "Forecast"),
                aes(ymin = yhat_lower, ymax = yhat_upper),
                fill = "cyan", alpha = 0.35) +
    labs(title = paste("Forecast for series", series_id),
         x = "Date", y = "Value", color = "Weekday") +
    theme_minimal() +
    theme(legend.position = "bottom")
}

# Prepare data for Prophet
df_prophet <- df |>
  mutate(train = date < train_test_threshold) |>
  rename(ds = date,
         y = value)

# Apply forecast function to each series
series_forecasts <- NULL
series_ids <- unique(df_prophet$id)
for (series_id in series_ids) {
  series_data <- df_prophet |>
    filter(train & id == series_id)
  series_forecasts <- series_forecasts |>
    bind_rows(forecast_series(series_data, series_id))
}

# Plot individual series from year 2023
plot_start_date <- as.Date("2023-01-01")
plot_series_forecast(df_prophet, series_forecasts, "1", start_date=plot_start_date)
plot_series_forecast(df_prophet, series_forecasts, "2", start_date=plot_start_date)
plot_series_forecast(df_prophet, series_forecasts, "3", start_date=plot_start_date)

# Function to generate one simulation
simulate_forecast <- function() {
  series_forecasts |>
    group_by(id, ds) |>
    mutate(simulated_y = rnorm(n(), y, (yhat_upper - yhat_lower) / 3.92)) |>
    group_by(ds) |>
    summarise(total_y = sum(simulated_y))
}

# Set the number for the Monte Carlo
n_simulations = 1000

# Run simulations
simulations <- map(1:n_simulations, ~ simulate_forecast()) |>
  bind_rows(.id = "simulation")

# Calculate aggregate forecast and confidence intervals
aggregate_forecast <- simulations |>
  group_by(ds) |>
  summarise(
    y = mean(total_y),
    yhat_lower = quantile(total_y, 0.025),
    yhat_upper = quantile(total_y, 0.975)) |>
  mutate(type = "Forecast",
         train = ds < train_test_threshold)

# Calculate actual aggregate totals
actual_totals <- df_prophet |>
  group_by(ds, train) |>
  summarise(y = sum(y)) |>
  mutate(type = "Actual",
         train = ds < train_test_threshold)

# Combine actual totals and aggregate forecast
combined_data <- bind_rows(
  actual_totals,
  aggregate_forecast) |>
  mutate(
    yhat_lower = ifelse(is.na(yhat_lower), 0, yhat_lower),
    yhat_upper = ifelse(is.na(yhat_upper), 0, yhat_upper)) |>
  group_by(ds, train, type) |>
  summarize(y = sum(y),
            yhat_lower = sum(yhat_lower),
            yhat_upper = sum(yhat_upper)) |>
  mutate(weekday = wday(ds, label = TRUE))


# Plot data: Keep only the last year of test data to reduce clutter
plot_data <- combined_data |> filter(ds >= plot_start_date)
# Plot daily
ggplot(plot_data, aes(x = ds, y = y)) +
  geom_point(data = plot_data |> filter(type == "Actual"),
             aes(color = weekday), alpha = 0.6) +
  geom_ribbon(data = plot_data |> filter(type == "Forecast"),
              aes(ymin = yhat_lower, ymax = yhat_upper),
              fill = "cyan", alpha = 0.8) +
  geom_line(data = plot_data |> filter(type == "Forecast"),
            color = "blue", size = 1, alpha = 0.5) +
  labs(title = "Aggregate Forecast vs Actuals (daily)",
       x = "Date", y = "Value") +
  theme_minimal()

# Aggregate simulation data weekly
combined_simulations_weekly <- simulations |>
  mutate(wk = floor_date(ds, "week", 1)) |>
  group_by(simulation, wk) |>
  summarise(
    days = n(),
    y = sum(total_y)) |>
  filter(days == 7)

# Plot weekly aggregate forecast with CIs
plot_data_wk <- combined_data_weekly |> filter(wk >= plot_start_date)
ggplot(plot_data_wk, aes(x = wk, y = y)) +
  geom_point(data = plot_data_wk |> filter(type == "Actual"),
             alpha = 0.6) +
  geom_line(data = plot_data_wk |> filter(type == "Forecast"),
            color = "blue", size = 1, alpha = 0.5) +
  geom_ribbon(data = plot_data_wk |> filter(type == "Forecast"),
              aes(ymin = yhat_lower, ymax = yhat_upper),
              fill = "cyan", alpha = 0.4) +
  labs(title = "Aggregate Forecast vs Actuals (weekly)",
       x = "Date", y = "Value") +
  theme_minimal()
