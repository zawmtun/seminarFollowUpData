# -------------------------------------------------------------------------
# 01_dummy_data.R
#
# Author: Zaw Myo Tun
# Date: 15 Aug 2020
# Project: SLING seminaR Workshop
# Title: Managing follow-up data
# -------------------------------------------------------------------------

set.seed(2020)
freq <- sample(5:10, 15, replace = TRUE)
id <- rep(seq_len(length(freq)), times = freq)
fu <- unlist(sapply(freq, seq_len))
result <- unlist(sapply(
  freq,
  function(n) sample(x = c("pos", "neg"),
                     size = n,
                     replace = TRUE,
                     prob = c(.2, .8)),
  simplify = TRUE
))
exposed <- rep(
  sample(c(0, 1), 15, replace = TRUE),
  times = freq
  )

d_ue <- lubridate::dmy("2-12-11") + unlist(
  mapply(function(x, y) x + cumsum(sample(seq_len(30), y)),
         sample(seq_len(90), length(freq)), freq)
)

d_e <- lubridate::dmy("2-12-11") + unlist(
  mapply(function(x, y) x + seq_len(y),
         sample(seq_len(90), length(freq)), freq)
)

# Follow-up dates with unequal interval
dat_ue <- data.frame(id = id, exposed = exposed, d = d_ue, fu = fu, result = result)
# Follow-up dates with equal interval
dat_e <- data.frame(id = id, exposed = exposed, d = d_e, fu = fu, result = result)

readr::write_csv(dat_ue, here::here("data", "derived_data", "unequal_interval.csv"))
readr::write_csv(dat_e, here::here("data", "derived_data", "equal_interval.csv"))
