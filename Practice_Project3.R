install.packages("nycflights13")
library("nycflights13")

getwd()

x <- ymd("2012-01-01", tz = "") ## Midnight
y <- dmy_hms("9 Jan 2011 11:34:21", tz = "")
x - y ## this works

########################### 1A ######################

# goal is to explore the sales dataset
library("tidyverse")

# Step 1: Remove patterns like (US)[51] from 'released' column
# Regular expression to match any text inside parentheses followed by brackets
sales$released_clean <- str_remove(sales$released, "\\s*\\([^\\)]+\\)\\[[^\\]]+\\]")

# Step 2: Convert the cleaned 'released_clean' column to Date class
# Assumes the format is "Month Day, Year"
sales$released_date <- mdy(sales$released_clean)

# View the updated dataset
head(sales)

# Step 1: Inspect the unique levels in the 'country' column
unique(sales$country)

# Step 2: Collapse rare factor levels using fct_lump, keeping only the most common countries
# The 'n' argument defines how many most common levels to keep
sales$country_factor <- fct_lump(sales$country, n = 5) # Keeps the top 5 countries, collapses others to "Other"

# View the updated dataset with the new 'country_factor' column
head(sales)

# Transform the 'sales' column into millions
sales$sales_millions <- sales$sales / 1e6

# View the updated dataset
head(sales)

# Filter the dataset to keep only rows where 'country_factor' is UK, US, or World
sales_filtered <- sales %>% 
  filter(country_factor %in% c("UK", "US", "World"))

# View the filtered dataset
head(sales_filtered)

print(sales_filtered)

################################### 1B ######################################

# Filter the dataset to keep only rows where 'country_factor' is US
sales_us <- sales_filtered %>%
  filter(country_factor == "US")

# Calculate the current year
current_year <- year(Sys.Date())

# Create the 'years_since_release' column
sales_us <- sales_us %>%
  mutate(years_since_release = floor(current_year - year(released_date)))
# Summarize the years since release
summary_stats <- sales_us %>%
  summarise(
    most_recent = max(years_since_release, na.rm = TRUE),
    oldest = min(years_since_release, na.rm = TRUE),
    median_years = median(years_since_release, na.rm = TRUE)
  )

# View the summary statistics
print(summary_stats)

# Step 1: Keep only album sales from the US
sales_us <- sales_filtered %>%
  filter(country_factor == "US")

# Step 2: Create the 'years_since_release' column
current_year <- year(Sys.Date())
sales_us <- sales_us %>%
  mutate(years_since_release = floor(current_year - year(released_date)))

# Step 3: Calculate summary statistics
summary_stats <- sales_us %>%
  summarise(
    most_recent = max(years_since_release, na.rm = TRUE),
    oldest = min(years_since_release, na.rm = TRUE),
    median_years = median(years_since_release, na.rm = TRUE)
  )

# View the summary statistics
print(summary_stats)