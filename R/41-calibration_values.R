get_targets <- function(fle) {
  df_tmp <- as_tibble(readRDS(fle)) %>%
    filter(time == max(time)) %>%
    mutate(
      prev.B = i.B / (s.B + i.B),
      dx.B = i_dx.B / i.B,
      tx.B = i_tx.B / i_dx.B,
      sup_dx.B = i_sup.B / i_dx.B,
      sup_tx.B = i_sup.B / i_tx.B,
      sup_dur.B = i_sup_dur.B / i_sup.B,
      prev.H = i.H / (s.H + i.H),
      dx.H = i_dx.H / i.H,
      tx.H = i_tx.H / i_dx.H,
      sup_dx.H = i_sup.H / i_dx.H,
      sup_tx.H = i_sup.H / i_tx.H,
      sup_dur.H = i_sup_dur.H / i_sup.H,
      prev.W = i.W / (s.W + i.W),
      dx.W = i_dx.W / i.W,
      tx.W = i_tx.W / i_dx.W,
      sup_dx.W = i_sup.W / i_dx.W,
      sup_tx.W = i_sup.W / i_tx.W,
      sup_dur.W = i_sup_dur.W / i_sup.W,
      prep_retained6m = s_prep_6m / s_prep
    ) %>%
    select(all_of(names(targets)))
}

library(tidyverse)
library(future.apply)
plan(multiprocess, workers = 4)

source("R/utils-targets.R")
filenames <- fs::dir_ls("/media/BACKUP/home_backup/SFO_choose_restart_HIGH/out")

df_targets <- bind_rows(future_lapply(filenames, get_targets))

fmtr <- scales::label_percent(accuracy = 0.1)

df_outcomes <- df_targets %>%
  select(-c(prep_any12m, prep_retained6m)) %>%
  pivot_longer(everything()) %>%
  group_by(name) %>%
  summarise(
    q1 = quantile(value, 0.25, na.rm = TRUE),
    med = median(value, na.rm = TRUE),
    q3 = quantile(value, 0.75, na.rm = TRUE)
  ) %>%
  separate(name, into = c("name", "pop"), sep = "\\.") %>%
  mutate(clean_val = glue::glue("{fmtr(med)} [{fmtr(q1)}, {fmtr(q3)}]")) %>%
  select(-c(q1, med, q3)) %>%
  pivot_wider(names_from = pop, values_from = clean_val)

df_true <- tibble(
  name = names(targets),
  value = targets
)

df_true <- df_true %>%
  filter(! name %in% c("prep_any12m", 'prep_retained6m')) %>%
  separate(name, into = c("name", "pop"), sep = "\\.") %>%
  pivot_wider(names_from = pop, values_from = value)
