################# PROJECT 3 #######################

# saving the data 
# the follwing codes will run the command repeatedly even if the data is already present in my pc
b_lyrics <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-29/beyonce_lyrics.csv")
ts_lyrics <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-29/taylor_swift_lyrics.csv")
sales <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-29/sales.csv")

# to solve repeated download we'll run the following commands
library("here")
rds_files <- c("b_lyrics.RDS", "ts_lyrics.RDS", "sales.RDS")
## Check whether we have all 3 files
if (any(!file.exists(here("data", rds_files)))) {
  ## If we don't, then download the data
  b_lyrics <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-29/beyonce_lyrics.csv")
  ts_lyrics <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-29/taylor_swift_lyrics.csv")
  sales <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-29/sales.csv")
  
  ## Then save the data objects to RDS files
  saveRDS(b_lyrics, file = here("data", "b_lyrics.RDS"))
  saveRDS(ts_lyrics, file = here("data", "ts_lyrics.RDS"))
  saveRDS(sales, file = here("data", "sales.RDS"))
}

# loading the datasets 
b_lyrics <- readRDS(here("data", "b_lyrics.RDS"))
ts_lyrics <- readRDS(here("data", "ts_lyrics.RDS"))
sales <- readRDS(here("data", "sales.RDS"))

##################################################################################################### 
###                                                                                               ###
###                                        PART 1                                                 ###
###                                                                                               ###
#####################################################################################################

########################### 1A ######################

# goal is to explore the sales dataset
library("tidyverse")

view(sales)

# searching for (US)[51] and removing them
regular_expression <- "\\s*\\([^\\)]+\\)\\[[^\\]]+\\]"

#creating a new colum 
sales$released_date <- str_remove(sales$released, regular_expression)
view(sales)

# checking the format
class(sales$released_date)

# converting character to date format
sales$date <- mdy(sales$released_date)

# checking if it turned into date
class(sales$date)

# checking the country var
table(sales$country)
sales %>% count(country)

# recoding the 'country' column for consistency
sales <- sales %>%
  mutate(country = recode(country,
                          "FR" = "France",
                          "FRA" = "France",
                          "WW" = "World",
                          "AUS" = "Australia",
                          "CAN" = "Canada",
                          "JPN" = "Japan",
                          "UK" = "UK",
                          "US" = "US"))

# checking if it worked
table(sales$country)

# Creating country factor
sales$country_factor <- fct_lump_n(sales$country, n = 3)

# checking if it worked
summary(sales$country_factor)

# transforming sales into millions of dollars
sales$sales_in_millions <- sales$sales / 1000000

# checking if it worked
table(sales$sales_in_millions)
view(sales)

# keeping only album sales from the UK, the US or the World.
sales_top <- sales %>%
  filter(country_factor %in% c("UK", "US", "World"))

# checking if it worked
view(sales_top)
print(sales_top)


################################### 1B ######################################

# keeping only album sales from the US 
sales_us <- sales %>%
  filter(country_factor == "US")
# checking if it worked
view(sales_us)

# creating the 'years_since_release' column
current_year <- year(Sys.Date())
sales_us <- sales_us %>%
  mutate(years_since_release = floor(current_year - year(date)))

view(sales_us)

# calculating summary statistics
summary_stats <- sales_us %>%
  group_by(artist) %>%
  summarise(
    most_recent = min(years_since_release),
    oldest = max(years_since_release),
    median_years = median(years_since_release)
  )

# View the summary statistics
print(summary_stats)


################################### 1C ######################################

library("ggplot2")

# Calculating the total album sales for each artist and for each country
total_album_sales <- sales_top %>%
  group_by(artist, country_factor) %>%
  summarise ( "total_sales" = sum(sales_in_millions))

print(total_album_sales)

# creating percent stacked bar chart

total_album_sales <- total_album_sales %>%
  mutate( "percentage_sales" = total_sales/ sum(total_sales) * 100)

album_sales_stacked <- ggplot(total_album_sales, 
                              aes(x = artist, y = percentage_sales, fill = country_factor)) +
  geom_bar(position = "fill", stat = "identity") +
  labs(
    title = "Percentage of Total Album Sales by Country",
    subtitle = "The Percent Stacked Barchart shows the percentage of album sales \nby Beyoncé and Taylor Swift in different parts of the world",
    x = "Artist",
    y = "Percentage of Album Sales",
    fill = "Country",
    caption = "Data source: TidyTuesday") +
  theme_gray() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 12),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 10, face = "bold"),
    strip.text = element_text(face = "bold")
  ) 

print(album_sales_stacked)

################################### 1D ######################################

# considering only world sales

sales_world <- sales_top %>%
  filter( country_factor == "World" )

view(sales_world)

sales_world_plot <- ggplot(sales_world, aes(x = sales_in_millions , y = fct_reorder(.f = title, .x = sales_in_millions), fill = artist)) +
  geom_bar(stat = "identity") +
  labs(
    title = "Sales of Studio Albums (World Sales)",
    subtitle = "This Barchart shows the album titles and their sales in million dollars by Beyoncé and Taylor Swift",
    x = "Sales (in millions)",
    y = "Album Title",
    fill = "Artist",
    caption = "Data source: TidyTuesday"
  ) +
  theme_grey() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 12),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 10, face = "bold"),
    strip.text = element_text(face = "bold")
  ) 

print(sales_world_plot)

################################### 1E ######################################

# converting the mdy to year only
sales_top <- sales_top %>%
  mutate(released_year = year(date))

view(sales_top)

#creating scatter plots
sales_scatter_plot <- ggplot(sales_top, aes(x = released_year, y = sales_in_millions, color = artist)) +
  geom_point(size = 3, alpha = 0.7) +  # Add points, size and transparency
  labs(
    title = "Studio Album Sales by Release Year",
    subtitle = "This Scatter Plot shows the sales of studio albums in millions of dollars \nby year released by Beyoncé and Taylor Swift",
    x = "Release Year",
    y = "Sales (in millions)",
    color = "Artist",
    caption = "Data source: TidyTuesday"
  ) +
  facet_grid(rows = vars(country_factor), scales = "free_y") +
  ylim(0, 12) +
  theme_grey() +  
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 12),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 10, face = "bold"),
    strip.text = element_text(face = "bold", size = 12)
  )

# Print the scatter plot
print(sales_scatter_plot)

##################################################################################################### 
###                                                                                               ###
###                                        PART 2                                                 ###
###                                                                                               ###
#####################################################################################################

################################### 2A ######################################

install.packages("tidytext")
library("tidytext")
library("stringr")

# Using "lines" token to tokenize lyrics column

ts_lyrics_tidy <- ts_lyrics %>%
  unnest_tokens(
    output=line, #creates new column 'line'
    input= Lyrics, #replaces 'Lyrics' with output 'line'
    token = "lines")  

# lines containing "hello"
hello_lines <- ts_lyrics_tidy %>%
  filter(str_detect(line, "hello")) 

print(hello_lines)

# the total number of lines containing "hello"
count_hello <- nrow(hello_lines) 
print(count_hello)

# lines containing "goodbye"
goodbye_lines <- ts_lyrics_tidy %>%
  filter(str_detect(line, "goodbye"))

print(goodbye_lines)

# total number of lines containing "goodbye"
count_goodbye <- nrow(goodbye_lines)
print(count_goodbye)


################################### 2B ######################################
#Repeat part 2A with b_lyrics

# lines containing "hello"
 hello_lines_b <- b_lyrics %>%
  filter(str_detect(line, "hello")) 

print(hello_lines_b)

# total number of lines containing "hello"
count_hello_b <- nrow(hello_lines_b) 
print(count_hello_b)

#lines containing "goodbye"
goodbye_lines_b <- b_lyrics %>%
  filter(str_detect(line, "goodbye"))

print(goodbye_lines_b)

# total number of lines containing "goodbye"
count_goodbye_b <- nrow(goodbye_lines_b)
print(count_goodbye_b)

################################### 2C ######################################

# Tokenizing lyrical lines by words
b_lyrics_tidy <- b_lyrics %>%
  unnest_tokens(
    output = word,  # creates a new column 'word'
    input = line,   # tokenize the 'line' column
    token = "words" # token type is words
  )

# View the tokenized dataset
print(b_lyrics_tidy)

# Load stopwords from the tidytext package
data("stop_words")

# Remove stopwords from the tokenized lyrics
b_lyrics_no_stopwords <- b_lyrics_tidy %>%
  anti_join(stop_words, by = "word")  # Filter out stopwords

# View the dataset without stopwords
print(b_lyrics_no_stopwords)

# Calculate the total number of occurrences for each word
word_count <- b_lyrics_no_stopwords %>%
  count(word, sort = TRUE)  # Count occurrences and sort in descending order

# View the word counts
print(word_count)

# Load the "bing" sentiment lexicon from the tidytext package
bing_lexicon <- get_sentiments("bing")

# Join the word count data with the bing sentiment lexicon
word_count_with_sentiment <- word_count %>%
  inner_join(bing_lexicon, by = "word")  # Merge based on the word column

# View the data with sentiment column
print(word_count_with_sentiment)

# Sort the data from most frequent to least frequent words
word_count_with_sentiment_sorted <- word_count_with_sentiment %>%
  arrange(desc(n))  # Sort in descending order of the word count

# View the sorted data
print(word_count_with_sentiment_sorted)

# Keep only the top 25 most frequent words
top_25_words <- word_count_with_sentiment_sorted %>%
  slice_head(n = 25)  # Select the first 25 rows

# View the top 25 words
print(top_25_words)

# Create a bar plot for the top words with their frequency
top_words_plot <- ggplot(top_25_words, aes(x = n, y = fct_reorder(word, n), fill = sentiment)) +
  geom_bar(stat = "identity") +
  labs(
    title = "Top 25 Most Frequent Words in Lyrics",
    subtitle = "haven't thought anything yet",
    x = "Frequency",
    y = "Words",
    fill = "Sentiment"
  ) +
  theme_grey() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 12, face = "bold"),
    legend.position = "right"
  )

# Print the plot
print(top_words_plot)


install.packages("wordcloud")
library("wordcloud")

# Create a word cloud of the top 25 most frequent words
wordcloud(
  words = top_25_words$word,   # Words to be plotted
  freq = top_25_words$n,       # Frequencies of the words
  min.freq = 1,                # Minimum frequency to include in the cloud
  max.words = 25,              # Maximum number of words to show
  random.order = FALSE,        # Plot words in order of frequency
  rot.per = 0.35,              # Proportion of words that are rotated
  scale = c(10, 0.5),           # Range of text size
  colors = brewer.pal(8, "Dark2")  # Color palette
)

################################### 2D ######################################

library(dplyr)
library(tidytext)

ts_lyrics_tidy <- ts_lyrics %>%
  unnest_tokens(
    output = word,       # creates new column 'word'
    input = Lyrics,      # replaces 'Lyrics' with output 'word'
    token = "words"      # tokenizing by words
  )

data("stop_words")  # Load stop words

ts_lyrics_cleaned <- ts_lyrics_tidy %>%
  anti_join(stop_words, by = "word")  # Remove stopwords

word_counts_ts <- ts_lyrics_cleaned %>%
  count(word, sort = TRUE)  # Count occurrences of each word

sentiment_bing <- get_sentiments("bing")

word_sentiment_ts <- word_counts_ts %>%
  left_join(sentiment_bing, by = "word")  # Add sentiment

word_sentiment_sorted_ts <- word_sentiment_ts %>%
  arrange(desc(n))  # Sort by frequency

top_25_words_ts <- word_sentiment_sorted_ts %>%
  top_n(25, n)  # Get top 25 words

print(top_25_words_ts)


# Create a bar plot for the top 25 most frequent words in Taylor Swift's lyrics
top_words_plot_ts <- ggplot(top_25_words_ts, aes(x = n, y = fct_reorder(word, n), fill = sentiment)) +
  geom_bar(stat = "identity") +
  labs(
    title = "Top 25 Most Frequent Words in Taylor Swift's Lyrics",
    subtitle = "Analyzing sentiment in her lyrics",
    x = "Frequency",
    y = "Words",
    fill = "Sentiment"
  ) +
  theme_grey() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 12, face = "bold"),
    legend.position = "right"
  )

# Print the plot
print(top_words_plot_ts)


wordcloud(
  words = top_25_words_ts$word,
  freq = top_25_words_ts$n,
  min.freq = 1,
  max.words = 25,
  random.order = FALSE,
  rot.per = 0.35,
  scale = c(4, 0.5),
  colors = brewer.pal(8, "Dark2")
)


################################### 2E ######################################

# Tokenize each lyrical line by words
ts_lyrics_tidy <- ts_lyrics %>%
  unnest_tokens(
    output = word,      # New column named 'word'
    input = Lyrics,     # Column to tokenize
    token = "words"     # Tokenizing by words
  )

# Print the first few rows of the tidy dataset
print(head(ts_lyrics_tidy))

# Remove stopwords
ts_lyrics_clean <- ts_lyrics_tidy %>%
  anti_join(stop_words)  # Using the built-in stop_words dataset

# Print the first few rows of the cleaned dataset
print(head(ts_lyrics_clean))

# Calculate total number of occurrences for each word in the lyrics for each album
word_counts <- ts_lyrics_clean %>%
  count(Album, word, sort = TRUE)  # Counting words and sorting by frequency

# Print the first few rows of the word counts dataset
print(head(word_counts))

install.packages("textdata")
library("textdata")

# Get the AFINN sentiment lexicon
afinn <- get_sentiments("afinn")

# Join the word counts with the AFINN lexicon to add sentiment scores
word_counts_with_sentiment <- word_counts %>%
  inner_join(afinn, by = "word")

# Print the first few rows of the updated dataset
print(head(word_counts_with_sentiment))

print(word_counts_with_sentiment)

# Summarize to get average sentiment score for each album
average_sentiment <- word_counts_with_sentiment %>%
  group_by(album) %>%
  summarise(average_sentiment = mean(value, na.rm = TRUE)) # Assuming 'value' is the column with sentiment scores

# Join with the album sales data frame
combined_data <- sales_us %>%
  left_join(average_sentiment, by = "title") # Adjust "title" if your column name is different

# Print the combined data frame
print(combined_data)
######## 7
# Tokenizing lyrics by words while keeping album information
ts_lyrics_tidy <- ts_lyrics %>%
  unnest_tokens(
    output = word,  # creates a new column 'word'
    input = Lyrics, # the original lyrics
    token = "words"
  ) 
#%>%
  #select(album, word)  # Select the album column

# Remove stopwords
ts_lyrics_clean <- ts_lyrics_tidy %>%
  anti_join(stop_words, by = "word")

# Calculate total word counts per album and word
word_counts_with_sentiment_ts <- ts_lyrics_clean %>%
  count(album, word, sort = TRUE) %>%
  left_join(get_sentiments("afinn"), by = "word") # Add sentiment scores

# Ensure you keep the necessary columns after counting
word_counts_with_sentiment_ts <- ts_lyrics_clean %>%
  group_by(Album, word) %>% # Group by album and word
  summarise(n = n(), .groups = 'drop') %>% # Count occurrences while keeping the grouping
  left_join(get_sentiments("afinn"), by = "word") # Join with sentiment lexicon

# Print the resulting data frame
print(word_counts_with_sentiment_ts)


# Now calculate the average sentiment for each album
average_sentiment_per_album <- word_counts_with_sentiment %>%
  group_by(album) %>%
  summarise(average_sentiment = mean(value, na.rm = TRUE))  # Using 'value' for the sentiment score

# Join with the album sales data frame (filtered for US sales)
combined_data <- sales_us %>%
  left_join(average_sentiment_per_album, by = "album") # Ensure you match on the correct column

# Print the combined data frame
print(combined_data)









