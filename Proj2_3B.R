#### this is straight out of chatgpt with going forward and backward couple of times. I think this is what they want?
# feel free to change/rename the function or object names. I'll change mine too after breakfast! 
## Also we have to add some description how and why we did this function.

# Function to generate a histogram of log-transformed rainfall
plot_rainfall_distribution <- function(city_input, year_input) {
  
  # Filter the data for the given city and year
  filtered_data <- df %>%
    filter(city_name == city_input, year_only == year_input)
  
  # Error handling if no data exists for the input city and year
  if (nrow(filtered_data) == 0) {
    stop("No data available for the specified city and year.")
  }
  
  # Log-transform the rainfall data (log1p to handle zero rainfall values)
  filtered_data <- filtered_data %>%
    mutate(log_rainfall = log1p(rainfall))
  
  # Create a histogram of log-transformed rainfall
  ggplot(filtered_data, aes(x = log_rainfall)) +
    geom_histogram(bins = 20, fill = "blue", color = "black") +
    labs(
      title = paste("Rainfall Distribution (Log Scale) in", city_input, "for", year_input),
      x = "Rainfall in log scale",
      y = "Frequency",
      caption = "Data source: TidyTuesday"
    ) +
    theme_minimal()
}

# Example call
plot_rainfall_distribution("PERTH", 1967)
plot_rainfall_distribution("SYDNEY", 2018)
plot_rainfall_distribution("BRISBANE", 2018)
plot_rainfall_distribution("PERTH", 2018)
