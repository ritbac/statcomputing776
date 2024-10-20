# Cathy, I wrote this code like your setup, if you just paste in your rmd file and run, it should work.
# also the colors are different for me and subtitle is different. 
# Step 2: Create the plot
ggplot( data = df_filt, mapping =  aes(x = date, y = temperature)) +
  geom_line(aes(color = temp_type)) +
  facet_wrap(~ city_name, scales = "free_y") +  # Facet by city_name
  scale_color_manual(values = c("min" = "blue", "max" = "red")) + 
  labs(
    title = "Max and Min Temperatures Over Time (2014 and onwards)",
    subtitle = "Trends in maximum and minimum temperatures for each city",
    x = "Date",
    y = "Temperature (°C)",
    color = "Temp type",
    caption = "Data source: df dataset from Part 2"
  ) +
  theme_minimal() +  # Use a minimal theme
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 12),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 12),
    legend.position = "bottom"
  )