x <- 1
print(x)

x <-  ## Incomplete expression
  
  
x <- 5  ## nothing printed
x       ## auto-printing occurs
vector(mode = "numeric", length = 4)
vector(mode = "logical", length = 4)
vector(mode = "character", length = 4)

x <- sqrt(2) ^ 2
x
0/0
1/0
0/0

y <- c(1.7, "a")
typeof(y)
y <- c(TRUE, 2)
typeof(y)
y <- c("a", TRUE)
x <- 0:6
class(x)
as.numeric(x)
as.logical(x)
as.character(x)

m <- matrix(1:6, nrow = 2, ncol = 3) 
m
attributes(m)

x <- 1:3
y <- 10:12
cbind(x, y)
rbind(x, y)

x <- vector("list", length = 5)
x

x <- factor(c("yes", "yes", "no", "yes", "no")) 
x
attributes(x)

## Create a vector with NAs in it
x <- c(1, 2, NA, 10, 3)  
## Return a logical vector indicating which elements are NA
is.na(x)    

## Now create a vector with both NA and NaN values
x <- c(1, 2, NaN, NA, 4)
is.na(x)

is.nan(x)

x <- data.frame(foo = 1:4, bar = c(T, T, F, F)) 
x
attributes(x)

# try it yourself

library(tidyverse)
library(palmerpenguins)
penguins 

f <- function() {
  # this is the function body
  hello <- "Hello, world!\n"
  cat(hello)
}
f()

hello <- "Hello, world!\n"

print(hello)
cat(hello)


###############################project1 theke asnsi

if (!require("tidytuesdayR", quietly = TRUE)) {
  install.packages("tidytuesdayR")
}

## Install the remotes package if you don't have it
if (!require("remotes", quietly = TRUE)) {
  install.packages("remotes")
}
## Install the tidytuesdayR package (from GitHub) if you don't have it
if (!require("tidytuesdayR", quietly = TRUE)) {
  remotes::install_github("dslc-io/tidytuesdayR")
}


library("here")
library("tidyverse")
library("tidytuesdayR")

tuesdata <- tidytuesdayR::tt_load("2020-01-07")
rainfall <- tuesdata$rainfall
temperature <- tuesdata$temperature


if (!file.exists(here("data", "tuesdata_rainfall.RDS"))) {
  tuesdata <- tidytuesdayR::tt_load("2020-01-07")
  rainfall <- tuesdata$rainfall
  temperature <- tuesdata$temperature
  
  # save the files to RDS objects
  saveRDS(tuesdata$rainfall, file = here("data", "tuesdata_rainfall.RDS"))
  saveRDS(tuesdata$temperature, file = here("data", "tuesdata_temperature.RDS"))
}

if (!file.exists(here("data", "rainfall.RDS"))) {
  url_csv <- "https://github.com/rfordatascience/tidytuesday/blob/master/data/2020/2020-01-07/rainfall.csv"
  rainfall <- readr::read_csv(url_csv)
  
  # save the file to RDS objects
  saveRDS(rainfall, file = here("data", "rainfall.RDS"))
}

rainfall <- readRDS(here("data", "rainfall.RDS"))
as_tibble(rainfall)

#####################################################################################











