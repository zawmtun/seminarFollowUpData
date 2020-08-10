library(dplyr)
library(tidyr)


# Dummy data set ----------------------------------------------------------

set.seed(2020)
freq <- sample(5:10, 10, replace = TRUE)
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

d_ue <- lubridate::dmy("2-12-11") + unlist(
  mapply(function(x, y) x + cumsum(sample(seq_len(30), y)),
         sample(seq_len(90), length(freq)), freq)
)

d_e <- lubridate::dmy("2-12-11") + unlist(
  mapply(function(x, y) x + seq_len(y),
         sample(seq_len(90), length(freq)), freq)
)

# Follow-up dates with unequal interval
dat_ue <- data.frame(id = id, d = d_ue, fu = fu, result = result)
# Follow-up dates with equal interval
dat_e <- data.frame(id = id, d = d_e, fu = fu, result = result)


# 1. Remove patients with a positive result at the first follow-up --------

# Method 1

first_pos <- dat_ue %>%
  group_by(id) %>%
  slice(1) %>%
  filter(result == "pos")

dat_ue_1 <- dat_ue %>%
  filter(!id %in% first_pos$id)

# Method 2

dat_ue_1 <- dat_ue %>%
  group_by(id) %>%
  filter(!cumany(row_number() == 1 & result == "pos")) %>%
  ungroup()

# 2. Identify first positive and remove the following rows ----------------

dat_ue_2 <- dat_ue_1 %>%
  group_by(id) %>%
  filter(cumsum(result == "pos") <= 1) %>%
  ungroup()


# 3. Calculate the follow-up duration in days -----------------------------

fu_duration <- dat_ue_2 %>%
  group_by(id) %>%
  mutate(start = min(d),
         end = max(d),
         pos_result = max(result == "pos")) %>%
  distinct(id, start, end, pos_result) %>%
  mutate(duration = difftime(end, start, units = "days"))


# 4. Calculate time taken to change results -------------------------------
# Note a person may switch between pos and neg multiple times
# Calculate (1) the time taken for each change and
# (2) the time between the last change and last follow-up date

dat_ue_3 <- dat_ue_1 %>%
  group_by(id) %>%
  mutate(
    flag = cumsum(result != lead(result)),
    flag = lag(flag, default = 0)
  ) %>%
  group_by(id, flag) %>%
  mutate(d1 = min(d)) %>%
  group_by(id) %>%
  mutate(d1 = if_else(fu == max(fu), d, d1)) %>%
  distinct(id, flag, d1, .keep_all = TRUE) %>%
  mutate(d2 = lead(d1),
         result = lead(result),
         duration = difftime(d2, d1, units = "days")) %>%
  slice(-n()) %>%
  select(id, d1, d2, result, duration)


# For dataset with equal fullow-up intervals ------------------

# Convert long to wide and remove patients with first positive ------------

dat_e_1 <- dat_e %>%
  pivot_wider(names_from = fu,
              values_from = c(d, result)) %>%

  # Remove patients with a positive result at the first follow-up
  filter(result_1 != "pos")
