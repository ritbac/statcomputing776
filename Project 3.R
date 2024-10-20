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
    plot.title = element_text(size = 13, face = "bold"),
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
    plot.title = element_text(size = 13, face = "bold"),
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
    plot.title = element_text(size = 13, face = "bold"),
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

# 1.Tokenizing lyrical lines by words
b_lyrics_tokenized <- b_lyrics %>%
  unnest_tokens(
    output = word,  # creates a new column 'word'
    input = line,   # tokenize the 'line' column
    token = "words" # token type is words
  )

# View the tokenized dataset
print(b_lyrics_tokenized)
view(b_lyrics_tokenized)

# 2. Load stopwords from the tidytext package
data("stop_words")

# Remove stopwords from the tokenized lyrics
b_lyrics_no_stopwords <- b_lyrics_tokenized %>%
  anti_join(stop_words)  # Filter out stopwords

# View the dataset without stopwords
print(b_lyrics_no_stopwords)
view(b_lyrics_no_stopwords)

# 3. Calculate the total number of occurrences for each word
b_lyrics_no_stopwords_word_count <- b_lyrics_no_stopwords %>%
  count(word)  # Count occurrences and sort in descending order

# View the word counts
print(b_lyrics_no_stopwords_word_count)
view(b_lyrics_no_stopwords_word_count)

# 4. Load the "bing" sentiment lexicon from the tidytext package
bing_lexicon <- get_sentiments("bing")

# Join the word count data with the bing sentiment lexicon
b_lyrics_no_stopwords_word_count_sentiment <- b_lyrics_no_stopwords_word_count %>%
  inner_join(bing_lexicon)  # Merge based on the word column

# View the data with sentiment column
print(b_lyrics_no_stopwords_word_count_sentiment)
view(b_lyrics_no_stopwords_word_count_sentiment)

# 5. Sort the data from most frequent to least frequent words
b_lyrics_no_stopwords_word_count_sentiment_sorted <- b_lyrics_no_stopwords_word_count_sentiment %>%
  arrange(desc(n))  # Sort in descending order of the word count

# View the sorted data
print(b_lyrics_no_stopwords_word_count_sentiment_sorted)
view(b_lyrics_no_stopwords_word_count_sentiment_sorted)

# 6. Keep only the top 25 most frequent words
b_lyrics_top_25_words <- b_lyrics_no_stopwords_word_count_sentiment_sorted %>%
  slice_head(n = 25)  # Select the first 25 rows

# 7. View the top 25 words
print(b_lyrics_top_25_words)
view(b_lyrics_top_25_words)

# 8. Create a bar plot for the top words with their frequency
b_lyrics_top_25_words_plot <- ggplot(b_lyrics_top_25_words, aes(x = n, y = fct_reorder(word, n), fill = sentiment)) +
  geom_bar(stat = "identity") +
  labs(
    title = "Top 25 Most Frequent Words in Beyoncé's Lyrics",
    subtitle = "Love is the most used word followed by crazy and top",
    x = "Frequency",
    y = "Words",
    fill = "Sentiment",
    caption = "Data source: TidyTuesday"
  ) +
  theme_grey() +
  theme(
    plot.title = element_text(size = 13, face = "bold"),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 11, face = "bold"),
    legend.position = "right"
  )

# Print the plot
print(b_lyrics_top_25_words_plot)


install.packages("wordcloud")
library("wordcloud")

# 9. Create a word cloud of the top 25 most frequent words
wordcloud(
  words = b_lyrics_top_25_words$word,
  freq = b_lyrics_top_25_words$n,
  max.words = 100,
  random.order = TRUE,
  random.color = FALSE,
  scale = c(7, 0.5),
  colors = brewer.pal(8, "Dark2") 
)

################################### 2D ######################################

library(dplyr)
library(tidytext)

# 1.Tokenizing lyrical lines by words
ts_lyrics_tokenized <- ts_lyrics %>%
  unnest_tokens(
    output = word,       # creates new column 'word'
    input = Lyrics,      # replaces 'Lyrics' with output 'word'
    token = "words"      # tokenizing by words
  )

# View the tokenized dataset
print(ts_lyrics_tokenized)
view(ts_lyrics_tokenized)

# 2. Load stopwords from the tidytext package
data("stop_words")

# Remove stopwords from the tokenized lyrics
ts_lyrics_no_stopwords <- ts_lyrics_tokenized %>%
  anti_join(stop_words)  # Filter out stopwords

# View the dataset without stopwords
print(ts_lyrics_no_stopwords)
view(ts_lyrics_no_stopwords)

# 3. Calculate the total number of occurrences for each word
ts_lyrics_no_stopwords_word_count <- ts_lyrics_no_stopwords %>%
  count(word)  # Count occurrences and sort in descending order

# View the word counts
print(ts_lyrics_no_stopwords_word_count)
view(ts_lyrics_no_stopwords_word_count)

# 4. Load the "bing" sentiment lexicon from the tidytext package
bing_lexicon <- get_sentiments("bing")

# Join the word count data with the bing sentiment lexicon
ts_lyrics_no_stopwords_word_count_sentiment <- ts_lyrics_no_stopwords_word_count %>%
  inner_join(bing_lexicon)  # Merge based on the word column

# View the data with sentiment column
print(ts_lyrics_no_stopwords_word_count_sentiment)
view(ts_lyrics_no_stopwords_word_count_sentiment)

# 5. Sort the data from most frequent to least frequent words
ts_lyrics_no_stopwords_word_count_sentiment_sorted <- ts_lyrics_no_stopwords_word_count_sentiment %>%
  arrange(desc(n))  # Sort in descending order of the word count

# View the sorted data
print(ts_lyrics_no_stopwords_word_count_sentiment_sorted)
view(ts_lyrics_no_stopwords_word_count_sentiment_sorted)

# 6. Keep only the top 25 most frequent words
ts_lyrics_top_25_words <- ts_lyrics_no_stopwords_word_count_sentiment_sorted %>%
  slice_head(n = 25)  # Select the first 25 rows

# 7. View the top 25 words
print(ts_lyrics_top_25_words)
view(ts_lyrics_top_25_words)

# 8. Create a bar plot for the top words with their frequency
ts_lyrics_top_25_words_plot <- ggplot(ts_lyrics_top_25_words, aes(x = n, y = fct_reorder(word, n), fill = sentiment)) +
  geom_bar(stat = "identity") +
  labs(
    title = "Top 25 Most Frequent Words in Taylor Swift's Lyrics",
    subtitle = "Love is the most used word followed by bad and shake",
    x = "Frequency",
    y = "Words",
    fill = "Sentiment",
    caption = "Data source: TidyTuesday"
  ) +
  theme_grey() +
  theme(
    plot.title = element_text(size = 13, face = "bold"),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 11, face = "bold"),
    legend.position = "right"
  )

# Print the plot
print(ts_lyrics_top_25_words_plot)


install.packages("wordcloud")
library("wordcloud")

# 9. Create a word cloud of the top 25 most frequent words
wordcloud(
  words = ts_lyrics_top_25_words$word,
  freq = ts_lyrics_top_25_words$n,
  max.words = 100,
  random.order = TRUE,
  random.color = FALSE,
  scale = c(7, 0.5),
  colors = brewer.pal(8, "Dark2") 
)


################################### 2E ######################################

# 1.Tokenizing lyrical lines by words
ts_lyrics_tokenized <- ts_lyrics %>%
  unnest_tokens(
    output = word,       # creates new column 'word'
    input = Lyrics,      # replaces 'Lyrics' with output 'word'
    token = "words"      # tokenizing by words
  )

# View the tokenized dataset
print(ts_lyrics_tokenized)
view(ts_lyrics_tokenized)

# 2. Load stopwords from the tidytext package
data("stop_words")

# Remove stopwords from the tokenized lyrics
ts_lyrics_no_stopwords <- ts_lyrics_tokenized %>%
  anti_join(stop_words)  # Filter out stopwords

# View the dataset without stopwords
print(ts_lyrics_no_stopwords)
view(ts_lyrics_no_stopwords)

# 3. Calculate the total number for each word in the lyrics for each Album.
ts_album_word_count <- ts_lyrics_no_stopwords %>%
  count(Album, word, sort = TRUE)   # Count occurrences and sort in descending order

# View the word counts
print(ts_album_word_count)
view(ts_album_word_count)

# 4. Get the AFINN sentiment lexicon
afinn_lexicon <- get_sentiments("afinn")

# Join the word counts with the AFINN lexicon to add sentiment scores
ts_album_word_count_sentiment <- ts_album_word_count %>%
  inner_join(afinn, by = "word")

# Print the first few rows of the updated dataset
print(ts_album_word_count_sentiment)
view(ts_album_word_count_sentiment)

# 5. Summarize to get average sentiment score for each album
ts_album_average_sentiment <- ts_album_word_count_sentiment %>%
  group_by(Album) %>%
  summarise(average_sentiment = mean(value, na.rm = TRUE)) # Assuming 'value' is the column with sentiment scores

# 6. auto printing to see the wrangled tibble data frame
print(ts_album_average_sentiment)
view(ts_album_average_sentiment)

# 7. Join with the album sales data frame
sales_us <- sales_us %>%
  rename(Album = title)

view(sales_us)

# correcting reputation to Reputation
ts_album_average_sentiment$Album <- 
  replace(ts_album_average_sentiment$Album, ts_album_average_sentiment$Album == "reputation", "Reputation")

# View to see if it worked
view(ts_album_average_sentiment)

# joining the data
sales_us_joined <- sales_us %>%
  inner_join(ts_album_average_sentiment, by = "Album") # Adjust "title" if your column name is different

# Print the combined data frame
view(sales_us_joined)

# 8. creating a plot of average sentiment score
# converting the mdy to year only
sales_us_joined <- sales_us_joined %>%
  mutate(released_year = year(date))

view(sales_us_joined)

# creating scatter plot
sales_us_joined_scatter <- ggplot(sales_us_joined,
      aes(x=released_year,y= average_sentiment, size = sales_in_millions, label= Album))+
  geom_point( alpha = 0.7)+
  ylim(-1,1)+
  labs(
    title = "Taylor Swift's Lyrics Analyzed by Sentiment Scores \nand Sales by Album Release Date",
    x = "Album Release Year",
    y = "Average Sentiment Score",
    size = "Sales (in millions)",
    caption = "Data source: TidyTuesday"
  ) +
  theme_grey() +
  theme(
    plot.title = element_text(size = 13, face = "bold"),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 11, face = "bold"),
    legend.position = "right"
    )

print(sales_us_joined_scatter)

# 9. Adding a horizontal line at y-intercept=0
sales_us_joined_scatter <- ggplot(sales_us_joined,
                                  aes(x = released_year, y = average_sentiment, size = sales_in_millions, label = Album)) +
  geom_point(alpha = 0.7) +
  ylim(-1, 1) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +  # Add horizontal line
  labs(
    title = "Taylor Swift's Lyrics Analyzed by Sentiment Scores \nand Sales by Album Release Date",
    x = "Album Release Year",
    y = "Average Sentiment Score",
    size = "Sales (in millions)",
    caption = "Data source: TidyTuesday"
  ) +
  theme_grey() +
  theme(
    plot.title = element_text(size = 13, face = "bold"),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 11, face = "bold"),
    legend.position = "right"
  )

print(sales_us_joined_scatter)

# 10. adding subtitle to interprete the plot
sales_us_joined_scatter <- ggplot(sales_us_joined,
                                  aes(x = released_year, y = average_sentiment, size = sales_in_millions, label = Album)) +
  geom_point(alpha = 0.7) +
  ylim(-1, 1) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +  # Add horizontal line
  labs(
    title = "Taylor Swift's Lyrics Analyzed by Sentiment Scores \nand Sales by Album Release Date",
    subtitle = "Albums released before 2010 had average sentiment scores above 0 \nwith higher sales, but after 2010 most albums have average sentiment \nscores below 0 with comparatively less sales",
    x = "Album Release Year",
    y = "Average Sentiment Score",
    size = "Sales (in millions)",
    caption = "Data source: TidyTuesday"
  ) +
  theme_grey() +
  theme(
    plot.title = element_text(size = 13, face = "bold"),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 11, face = "bold"),
    legend.position = "right"
  )

print(sales_us_joined_scatter)

