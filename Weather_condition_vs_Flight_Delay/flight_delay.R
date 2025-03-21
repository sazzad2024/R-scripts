# Load necessary libraries
library(nycflights13)
library(dplyr)
library(tidyr)
library(ggplot2)



# Load the weather data
data(weather)

head(weather)

# Remove rows with missing values in relevant columns
weather_clean <- weather %>%
  drop_na(wind_speed, wind_gust, precip, visib)

head(weather_clean)
# Calculate daily averages
daily_weather <- weather_clean %>%
  group_by(year, month, day) %>%
  summarise(
    avg_wind_speed = mean(wind_speed, na.rm = TRUE),
    avg_wind_gust = mean(wind_gust, na.rm = TRUE),
    avg_precip = mean(precip, na.rm = TRUE),
    avg_visib = mean(visib, na.rm = TRUE)  #
  ) %>%
  ungroup()


head(daily_weather)

# Find the day with the highest average wind speed
max_wind_speed_day <- daily_weather %>%
  filter(avg_wind_speed == max(avg_wind_speed))

# Find the day with the highest average wind gust
max_wind_gust_day <- daily_weather %>%
  filter(avg_wind_gust == max(avg_wind_gust))

# Find the day with the highest average precipitation
max_precip_day <- daily_weather %>%
  filter(avg_precip == max(avg_precip))

# Print the results
print("Day with the highest average wind speed:")
print(max_wind_speed_day)

print("Day with the highest average wind gust:")
print(max_wind_gust_day)

print("Day with the highest average precipitation:")
print(max_precip_day)


# Load the flights data
data(flights)

head(flights)

# Calculate daily average departure delays
daily_delays <- flights %>%
  group_by(year, month, day) %>%
  summarise(avg_dep_delay = mean(dep_delay, na.rm = TRUE)) %>%
  ungroup()

head(daily_delays)



# Join the daily weather and daily delays data

daily_combined <- daily_weather %>%
  left_join(daily_delays, by = c("year", "month", "day"))

head(daily_combined)

# Calculate correlations
correlation_wind_speed <- cor(daily_combined$avg_wind_speed, daily_combined$avg_dep_delay, use = "complete.obs")
correlation_wind_gust <- cor(daily_combined$avg_wind_gust, daily_combined$avg_dep_delay, use = "complete.obs")
correlation_precip <- cor(daily_combined$avg_precip, daily_combined$avg_dep_delay, use = "complete.obs")
correlation_visib <- cor(daily_combined$avg_visib, daily_combined$avg_dep_delay, use = "complete.obs")

# Print the correlation results
print(paste("Correlation between average wind speed and departure delays:", correlation_wind_speed))
print(paste("Correlation between average wind gust and departure delays:", correlation_wind_gust))
print(paste("Correlation between average precipitation and departure delays:", correlation_precip))
print(paste("Correlation between average visibility and departure delays:", correlation_visib))


ggplot(daily_combined, aes(x = avg_wind_speed, y = avg_dep_delay)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(title = "Wind Speed vs. Departure Delays", x = "Average Wind Speed", y = "Average Departure Delay")