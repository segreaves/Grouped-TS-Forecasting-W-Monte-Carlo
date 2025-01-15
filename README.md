# Grouped Time Series Forecast Confidence Intervals With Monte Carlo Simulation
Using Monte Carlo simulation to create confidence intervals for grouped time series forecasts.

![image](https://github.com/user-attachments/assets/98d3883f-1b37-40f1-a190-d8c37cb84f52)

This is an R project where I implement Monte Carlo simulation to aggregate time series forecasts together to produce a grouped forecast with corresponding confidence intervals.
This is something that is not straight forward as confidence intervals of grouped forecasts have no direct relationship with the confidence intervals of the individual forecasts. This methos also allows for confidence intervals that respect correlations between time series, as long as these correlations are reproduced in the Monte Carlo simulation step.

It involves the following steps:

- Simulating time series data: Generate sample time series with distinct trends, seasonality, randomness, and weekday effects to represent real-world scenarios.

- Forecasting individual series: Use Facebook's Prophet model to forecast each series, capturing trends and seasonality while providing confidence intervals.

- Simulating grouped forecasts: Apply Monte Carlo simulation by sampling from the forecast distributions of each series and aggregating the samples repeatedly to form a distribution for the total.

This article goes through the project step-by-step:
https://medium.com/@simongreaves/grouped-time-series-forecast-confidence-intervals-with-monte-carlo-simulation-31cb9313ac9f
