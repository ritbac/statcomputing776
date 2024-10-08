## ------------------------------------------------------------------------------------------------------------------------
inches <- c(69, 62, 66, 70, 70, 73, 67, 73, 67, 70)


## ------------------------------------------------------------------------------------------------------------------------
inches * 2.54


## ------------------------------------------------------------------------------------------------------------------------
inches - 69


## ------------------------------------------------------------------------------------------------------------------------
x <- 1:10
y <- 1:10
x + y


## ------------------------------------------------------------------------------------------------------------------------
x <- 1:10
sqrt(x)


## ------------------------------------------------------------------------------------------------------------------------
y <- 1:10
x * y


## ------------------------------------------------------------------------------------------------------------------------
## Check that x and y have the same length
stopifnot(length(x) == length(y))
## Create our result object
result <- vector(mode = "integer", length = length(x))
## Loop through each element of x and y, calculate the sum,
## then store it on 'result'
for (i in seq_along(x)) {
  result[i] <- x[i] + y[i]
}
## Check that we got the same answer
identical(result, x + y)


## ------------------------------------------------------------------------------------------------------------------------
my_sum <- function(a, b) {
  a + b
}

## Same but with an extra check to make sure that 'a' and 'b'
## have the same lengths.
my_sum <- function(a, b) {
  ## Check that a and b are of the same length
  stopifnot(length(a) == length(b))
  a + b
}


## ------------------------------------------------------------------------------------------------------------------------
#' Title
#'
#' @param a
#' @param b
#'
#' @return
#' @export
#'
#' @examples
my_sum <- function(a, b) {
  ## Check that a and b are of the same length
  stopifnot(length(a) == length(b))
  a + b
}


## ------------------------------------------------------------------------------------------------------------------------
#' Title
#'
#' Description
#'
#' Details
#'
#' @param a What is `a`?
#' @param b What is `b`?
#'
#' @return What does the function return?
#' @export ## Do we want to share this function? yes!
#'
#' @examples
#' ## How do you use this function?
my_sum <- function(a, b) {
  ## Check that a and b are of the same length
  stopifnot(length(a) == length(b))
  a + b
}


## ------------------------------------------------------------------------------------------------------------------------
#' Sum two vectors
#'
#' This function does the element wise sum of two vectors.
#'
#' It really is just an example function that is powered by the `+` operator
#' from [base::Arithmetic].
#'
#' @param a An `integer()` or `numeric()` vector of length `L`.
#' @param b An `integer()` or `numeric()` vector of length `L`.
#'
#' @return An `integer()` or `numeric()` vector of length `L` with
#' the element-wise sum of `a` and `b`.
#' @export
#'
#' @examples
#' ## Generate some input data
#' x <- 1:10
#' y <- 1:10
#'
#' ## Perform the element wise sum
#' my_sum(x, y)
my_sum <- function(a, b) {
  ## Check that a and b are of the same length
  stopifnot(length(a) == length(b))
  a + b
}


## ------------------------------------------------------------------------------------------------------------------------
library("testthat")
test_that("my_sum works", {
  x <- seq_len(10)
  expect_equal(my_sum(x, x), x + x)
  
  expect_error(my_sum(x, seq_len(5)))
})


## ------------------------------------------------------------------------------------------------------------------------
#| eval: false
## ## Install biocthis if you don't have it
## if (!require("BiocManager", quietly = TRUE)) {
##     install.packages("BiocManager")
## }
## 
## BiocManager::install("biocthis")
## 
## ## Create an empty R package that is also an
## ## RStudio project
## usethis::create_package("~/Desktop/sum776")
## 
## ## On the new RStudio window, create the
## ## scripts that will guide you into making a package
## biocthis::use_bioc_pkg_templates()


## ------------------------------------------------------------------------------------------------------------------------
#| eval: false
## remotes::install_github("lcolladotor/sum776")


## ------------------------------------------------------------------------------------------------------------------------
## Check the arguments to mapply()
args(mapply)

## Apply mapply() to our function my_sum() with the inputs 'x' and 'y'
mapply(sum776::my_sum, x, y)

## Or write an anynymous function that is:
## * not documented
## * not tested
## * not shared
##
## :(
mapply(function(a, b) {
  a + b
}, x, y)


## ------------------------------------------------------------------------------------------------------------------------
#| message: false
library("purrr") ## part of tidyverse


## ------------------------------------------------------------------------------------------------------------------------
## Check the arguments of map2_int()
args(purrr::map2_int)

## Apply our function my_sum() to our inputs
purrr::map2_int(x, y, sum776::my_sum)

## You can also use anonymous functions
purrr::map2_int(x, y, function(a, b) {
  a + b
})

## purrr even has a super short formula-like syntax
## where .x is the first input and .y is the second one
purrr::map2_int(x, y, ~ .x + .y)

## This formula syntax has nothing to do with the objects 'x' and 'y'
purrr::map2_int(1:2, 3:4, ~ .x + .y)


## ------------------------------------------------------------------------------------------------------------------------
lapply


## ------------------------------------------------------------------------------------------------------------------------
x <- list(a = 1:5, b = rnorm(10))
x
lapply(x, mean)


## ------------------------------------------------------------------------------------------------------------------------
purrr::map_dbl(x, mean)


## ------------------------------------------------------------------------------------------------------------------------
purrr::map(x, mean)


## ------------------------------------------------------------------------------------------------------------------------
x <- list(a = 1:4, b = rnorm(10), c = rnorm(20, 1), d = rnorm(100, 5))
lapply(x, mean)


## ------------------------------------------------------------------------------------------------------------------------
x <- 1:4
lapply(x, runif)


## ------------------------------------------------------------------------------------------------------------------------
purrr::map(x, runif)


## ------------------------------------------------------------------------------------------------------------------------
x <- 1:4
lapply(x, runif, min = 0, max = 10)


## ------------------------------------------------------------------------------------------------------------------------
purrr::map(x, runif, min = 0, max = 10)


## ------------------------------------------------------------------------------------------------------------------------
x <- list(a = matrix(1:4, 2, 2), b = matrix(1:6, 3, 2))
x


## ------------------------------------------------------------------------------------------------------------------------
lapply(x, function(elt) {
  elt[, 1]
})


## ------------------------------------------------------------------------------------------------------------------------
f <- function(elt) {
  elt[, 1]
}
lapply(x, f)


## ------------------------------------------------------------------------------------------------------------------------
x <- list(a = 1:4, b = rnorm(10), c = rnorm(20, 1), d = rnorm(100, 5))
lapply(x, mean)


## ------------------------------------------------------------------------------------------------------------------------
sapply(x, mean)


## ------------------------------------------------------------------------------------------------------------------------
purrr::map(x, mean)
purrr::map_dbl(x, mean)


## ------------------------------------------------------------------------------------------------------------------------
str(split)


## ------------------------------------------------------------------------------------------------------------------------
x <- c(rnorm(10), runif(10), rnorm(10, 1))
f <- gl(3, 10) # generate factor levels
f


## ------------------------------------------------------------------------------------------------------------------------
split(x, f)


## ------------------------------------------------------------------------------------------------------------------------
lapply(split(x, f), mean)


## ------------------------------------------------------------------------------------------------------------------------
library("datasets")
head(airquality)


## ------------------------------------------------------------------------------------------------------------------------
s <- split(airquality, airquality$Month)
str(s)


## ------------------------------------------------------------------------------------------------------------------------
lapply(s, function(x) {
  colMeans(x[, c("Ozone", "Solar.R", "Wind")])
})


## ------------------------------------------------------------------------------------------------------------------------
sapply(s, function(x) {
  colMeans(x[, c("Ozone", "Solar.R", "Wind")])
})


## ------------------------------------------------------------------------------------------------------------------------
sapply(s, function(x) {
  colMeans(x[, c("Ozone", "Solar.R", "Wind")],
           na.rm = TRUE
  )
})


## ------------------------------------------------------------------------------------------------------------------------
purrr::map(s, function(x) {
  colMeans(x[, c("Ozone", "Solar.R", "Wind")], na.rm = TRUE)
})


## ------------------------------------------------------------------------------------------------------------------------
purrr::map_dfc(s, function(x) {
  colMeans(x[, c("Ozone", "Solar.R", "Wind")], na.rm = TRUE)
})


## ------------------------------------------------------------------------------------------------------------------------
## Make sure we get data.frame / tibble outputs for each element
## of the list
purrr:::map(s, function(x) {
  tibble::as_tibble(colMeans(x[, c("Ozone", "Solar.R", "Wind")], na.rm = TRUE))
})

## Now we can combine them with list_cbind()
purrr:::map(s, function(x) {
  tibble::as_tibble(colMeans(x[, c("Ozone", "Solar.R", "Wind")], na.rm = TRUE))
}) %>% purrr::list_cbind()

## And we can then add the actual variable it came from with mutate()
purrr:::map(s, function(x) {
  tibble::as_tibble(colMeans(x[, c("Ozone", "Solar.R", "Wind")], na.rm = TRUE))
}) %>%
  purrr::list_cbind() %>%
  dplyr::mutate(Variable = c("Ozone", "Solar.R", "Wind"))


## ------------------------------------------------------------------------------------------------------------------------
## Sadly map_dfr() is now superseded (aka not recommended)
purrr:::map_dfr(s, function(x) {
  colMeans(x[, c("Ozone", "Solar.R", "Wind")], na.rm = TRUE)
})

## This is how we would have added back the Month variable
purrr:::map_dfr(s, function(x) {
  colMeans(x[, c("Ozone", "Solar.R", "Wind")], na.rm = TRUE)
}) %>%
  dplyr:::mutate(Month = as.integer(names(s)))


## ------------------------------------------------------------------------------------------------------------------------
## Get data.frame / tibble outputs, but with each variable as a separate
## column. Here we used the t() or transpose() function.
purrr:::map(s, function(x) {
  tibble::as_tibble(t(colMeans(x[, c("Ozone", "Solar.R", "Wind")], na.rm = TRUE)))
})

## Now we can row bind each of these data.frames / tibbles into a
## single one
purrr:::map(s, function(x) {
  tibble::as_tibble(t(colMeans(x[, c("Ozone", "Solar.R", "Wind")], na.rm = TRUE)))
}) %>% purrr::list_rbind()

## Then with mutate, we can add the Month back
purrr:::map(s, function(x) {
  tibble::as_tibble(t(colMeans(x[, c("Ozone", "Solar.R", "Wind")], na.rm = TRUE)))
}) %>%
  purrr::list_rbind() %>%
  dplyr:::mutate(Month = as.integer(names(s)))


## ------------------------------------------------------------------------------------------------------------------------
## group_by() is in a way splitting our input data.frame / tibble by
## our variable of interest. Then summarize() helps us specify how we
## want to use that data, before it's all put back together into a
## tidy tibble.
airquality %>%
  dplyr::group_by(Month) %>%
  dplyr::summarize(
    Ozone = mean(Ozone, na.rm = TRUE),
    Solar.R = mean(Solar.R, na.rm = TRUE),
    Wind = mean(Wind, na.rm = TRUE)
  )


## ------------------------------------------------------------------------------------------------------------------------
str(tapply)


## ------------------------------------------------------------------------------------------------------------------------
## Simulate some data
x <- c(rnorm(10), runif(10), rnorm(10, 1))
## Define some groups with a factor variable
f <- gl(3, 10)
f
tapply(x, f, mean)


## ------------------------------------------------------------------------------------------------------------------------
tapply(x, f, range)


## ------------------------------------------------------------------------------------------------------------------------
split(x, f) %>% purrr::map_dbl(mean)
split(x, f) %>% purrr::map(range)


## ------------------------------------------------------------------------------------------------------------------------
str(apply)


## ------------------------------------------------------------------------------------------------------------------------
x <- matrix(rnorm(200), 20, 10)
head(x)
apply(x, 2, mean) ## Take the mean of each column


## ------------------------------------------------------------------------------------------------------------------------
apply(x, 1, sum) ## Take the mean of each row


## ------------------------------------------------------------------------------------------------------------------------
#| eval: false
## apply(x, 2, mean)


## ------------------------------------------------------------------------------------------------------------------------
#| eval: false
## apply(x, 1, sum)


## ------------------------------------------------------------------------------------------------------------------------
array_branch(x, 2) %>% map_dbl(mean)
array_branch(x, 1) %>% map_dbl(sum)


## ------------------------------------------------------------------------------------------------------------------------
x <- matrix(rnorm(200), 20, 10)
head(x)

## Get row quantiles
apply(x, 1, quantile, probs = c(0.25, 0.75))


## ------------------------------------------------------------------------------------------------------------------------
array_branch(x, 1) %>%
  map(quantile, probs = c(0.25, 0.75)) %>%
  map(~ as.data.frame(t(.x))) %>%
  list_rbind()


## ------------------------------------------------------------------------------------------------------------------------
sumsq <- function(mu, sigma, x) {
  sum(((x - mu) / sigma)^2)
}


## ------------------------------------------------------------------------------------------------------------------------
x <- rnorm(100) ## Generate some data
sumsq(mu = 1, sigma = 1, x) ## This works (returns one value)


## ------------------------------------------------------------------------------------------------------------------------
sumsq(1:10, 1:10, x) ## This is not what we want


## ------------------------------------------------------------------------------------------------------------------------
vsumsq <- Vectorize(sumsq, c("mu", "sigma"))
vsumsq(1:10, 1:10, x)

## The details are a bit complicated though
## as we can see below
vsumsq


## ------------------------------------------------------------------------------------------------------------------------
options(width = 120)
sessioninfo::session_info()
