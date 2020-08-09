

# Dummy data set

dummy_date <- lubridate::dmy("2-12-11") + seq_len(90)
gen_date <- function(d, length) d + cumsum(sample(seq_len(30), length))

dat <- data.frame(
  id = rep(1:4, times = c(5, 8, 5, 10)),
  d = as.Date(unlist(mapply(gen_date, sample(dummy_date, 4), c(5, 8, 5, 10))), origin = "1970-01-01"),
  result = unlist(lapply(
    c(5, 8, 5, 10),
    function(x) sample(x = c("pos", "neg"), size = x, replace = TRUE, prob = c(.1, .9))
  ))
)


# identify positive participants
pos_id <- dat %>%
  filter(result == "pos") %>%
  distinct(id)

pos <- dat %>%
  arrange(id, d) %>%

  # Filter positive participants
  filter(id %in% pos_id$id) %>%

  group_by(id) %>%
  mutate(
    instance = row_number(),
    flag = cumsum(result != lead(result)),
    flag = lag(flag, default = 0),
    d1 = if_else(flag != 0 & flag != lag(flag), lag(d), NA_real_),
    d2 = d1 + (d - d1)/2,
    d2 = if_else(flag == 0, min(d), d2)
  ) %>%

  group_by(id, flag) %>%
  mutate(d2 = min(d2, na.rm = TRUE)) %>%
  rename(start = d2) %>%

  group_by(id) %>%
  mutate(across(c(instance, result, flag, start), lead),
         lastdate = max(d),
         end = lead(start),
         end = if_else(is.na(end), lastdate, end)) %>%
  slice(-n()) %>%

  group_by(id, flag) %>%
  mutate(start = min(start),
         end = max(end)) %>%
  ungroup() %>%

  distinct(id, result, start, end, .keep_all = TRUE) %>%
  select(-c(d, flag, d1, lastdate)) %>%
  mutate(fu_time = difftime(end, start, units = "days"),
         fu_time = as.double(fu_time))
