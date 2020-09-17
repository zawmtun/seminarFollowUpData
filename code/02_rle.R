# -------------------------------------------------------------------------
# 01_rle.R
#
# Author: Zaw Myo Tun
# Date: 17 Sep 2020
# Project: SLING seminaR Workshop
# Title: To demonstrate run length encoding and its use case
# -------------------------------------------------------------------------

# Run Length Encoding

# Generate dummy binary vectors based on binomial distribution
a <- rbinom(n = 10, size = 1, prob = 0.5)
b <- rbinom(n = 100, size = 1, prob = 0.5)

# RLE
a
x <- rle(a)
x
inverse.rle(x)
identical(b, inverse.rle(y))

b
y <- rle(b)
y
inverse.rle(y)
identical(b, inverse.rle(y))

# A function to capture the sequence of value changes in a vector
my_seq <- function(x) {
  r <- rle(x)
  rep(seq_along(r$values), times = r$lengths)
}

a
my_seq(a)

# Deconstructing the function
# > rep()
rep(1, times = 10)
rep(1:4, times = 10)
rep(1:4, times = c(3, 3, 2, 2))

# > seq_along()
d <- c(3, 2, 5, 7)
length(d)
1:length(d) # indices

e <- vector("integer", 0)
e
1:length(e)

seq_along(d)
seq_along(e)

# > Put them together
a
x <- rle(a)
x
rep(seq_along(x$values), times = x$lengths)

my_seq <- function(x) {
  r <- rle(x)
  rep(seq_along(r$values), times = r$lengths)
}

my_seq(a)


library(dplyr)
dat_ue <- readr::read_csv(here::here("data", "derived_data", "unequal_interval.csv"))

dat_ue

dat_ue %>%
  group_by(id) %>%
  mutate(s = my_seq(result))
