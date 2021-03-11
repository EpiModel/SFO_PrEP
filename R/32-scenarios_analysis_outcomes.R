library(fs)
library(tidyverse)
theme_set(theme_light())

process_outcomes <- function(fle) {
  n_roll <- 13
  prep_start <- 52 * (65) + 1
  ana_beg <- prep_start + 4 * 52
  p_ana_range <- c(ana_beg, ana_beg + 10 * 52)
  s_ana_range <- c(prep_start, prep_start + 10 * 52)
  base_scenarios <- c("s_0", "p_025_sfo")

  e_ls <- readRDS(fle)
  df <- e_ls$data
  infos <- e_ls$infos
  job <- str_split(infos$job_name, "_")[[1]][3]
  range <- if (job == "p") p_ana_range else s_ana_range

  ehe_5y <- range[1] + 52 * 5

  # First triming
  df_b <- filter(df, between(time, range[1], range[2]))
  rm(df, e_ls)

  # Elements required for the outcomes at year 10
  df_at <- df_b %>%
    filter(between(time, range[2] - n_roll, range[2])) %>%
    group_by(scenario, batch, sim) %>%
    summarise(
      s = mean(s, na.rm = TRUE),
      prev = mean(i / s, na.rm = TRUE),
      incid = mean(incid, na.rm = TRUE),
      time_on_prep = mean(median_time_on_prep, na.rm = TRUE) / 52 * 12
    )

  # Elements required for the outcomes at year 5
  df_at5y <- df_b %>%
    filter(between(time, ehe_5y - n_roll, ehe_5y)) %>%
    group_by(scenario, batch, sim) %>%
    summarise(incid = mean(incid, na.rm = TRUE))

  df_at_base <- df_at %>%
    filter(scenario %in% base_scenarios) %>%
    ungroup() %>%
    select(-c(scenario, batch, sim)) %>%
    summarise(across(everything(), median))

  # Elements required for the outcomes cumualtive over year 10
  df_over <- df_b %>%
    group_by(scenario, batch, sim) %>%
    summarise(
      cum_incid = sum(incid, na.rm = TRUE),
      mean_s = mean(s, na.rm = TRUE),
      py_s_prep = sum(s_prep, na.rm = TRUE) / 52
    )

  df_over_base <- df_over %>%
    filter(scenario %in% base_scenarios) %>%
    ungroup() %>%
    select(-c(scenario, batch, sim)) %>%
    summarise(across(everything(), median))

  # Merge of the base scenarios
  df_base <- bind_cols(df_over_base, df_at_base)

  # At outcomes
  df_at <- df_at %>%
    group_by(scenario, batch, sim) %>%
    summarise(
      prev = prev,
      time_on_prep = time_on_prep,
      incid_reduction = (df_base$incid - incid) / df_base$incid,
      incid = incid / s * 5200
    )

  # At 5y outcomes
  df_at5y <- df_at5y %>%
    group_by(scenario, batch, sim) %>%
    summarise(incid_reduction5y = (df_base$incid - incid) / df_base$incid)

  # Over outcomes
  # Do I need to calculate stuff using mean_s, mean_s_prep, cum_i (sexDist style)
  df_over <- df_over %>%
    group_by(scenario, batch, sim) %>%
    summarise(
      nia =  (df_base$cum_incid - cum_incid),
      pia = nia / df_base$cum_incid,
      nnt = (py_s_prep - df_base$py_s_prep) / (df_base$cum_incid - cum_incid)
    )

  # Merge outcomes
  df_tot <- left_join(df_at, df_over, by = c("scenario", "batch", "sim"))
  df_tot <- left_join(df_tot, df_at5y, by = c("scenario", "batch", "sim"))

  # Calculate median SI95% for each outcome
  df_tot <- df_tot %>%
    group_by(scenario) %>%
    select(-c(batch, sim)) %>%
    summarise(across(
      everything(),
      list(
        p025 = ~ quantile(.x, 0.025, na.rm = TRUE),
        p500 = ~ quantile(.x, 0.5, na.rm = TRUE),
        p975 = ~ quantile(.x, 0.975, na.rm = TRUE)
      ),
      .names = "{col}___{fn}"
    ))

  df_tot <- df_tot %>%
    mutate(
      prep_cov = str_extract(scenario, "\\d+") %>%
        str_pad(4, "right", "0") %>%
        as.numeric() / 1e3,
      job = str_split(infos$job_name, "_")[[1]][3]
    )

  df_tot
}

scenario_files <- dir_ls("out/scenario_processed/")

df_outcomes <- map_dfr(scenario_files, process_outcomes)

df_outcomes %>%
  filter(scenario %in% c("s_0")) %>%
  select(starts_with("incid"))

saveRDS(df_outcomes, "out/df_outcomes.rds")


process_raw_nia_pia <- function(fle) {
  prep_start <- 52 * (65) + 1
  ana_beg <- prep_start + 4 * 52
  p_ana_range <- c(ana_beg, ana_beg + 10 * 52)
  s_ana_range <- c(prep_start, prep_start + 10 * 52)
  base_scenarios <- c("s_0", "p_025_sfo")

  e_ls <- readRDS(fle)
  df <- e_ls$data
  infos <- e_ls$infos
  job <- str_split(infos$job_name, "_")[[1]][3]
  range <- if (job == "p") p_ana_range else s_ana_range

  # First triming
  df_b <- filter(df, between(time, range[1], range[2]))
  rm(df, e_ls)

  # Elements required for the outcomes cumualtive over year 10
  df_over <- df_b %>%
    group_by(scenario, batch, sim) %>%
    summarise(
      cum_incid = sum(incid, na.rm = TRUE),
      mean_s = mean(s, na.rm = TRUE),
      py_s_prep = sum(s_prep, na.rm = TRUE) / 52
    )

  df_base <- df_over %>%
    filter(scenario %in% base_scenarios) %>%
    ungroup() %>%
    select(-c(scenario, batch, sim)) %>%
    summarise(across(everything(), median))

  # Over outcomes
  # Do I need to calculate stuff using mean_s, mean_s_prep, cum_i (sexDist style)
  df_tot <- df_over %>%
    group_by(scenario, batch, sim) %>%
    summarise(
      nia =  (df_base$cum_incid - cum_incid),
      pia = nia / df_base$cum_incid,
      nnt = (py_s_prep - df_base$py_s_prep) / nia
    )

  df_tot <- df_tot %>%
    mutate(
      prep_cov = str_extract(scenario, "\\d+") %>%
        str_pad(4, "right", "0") %>%
        as.numeric() / 1e3,
      job = str_split(infos$job_name, "_")[[1]][3]
    )

  df_tot
}

scenario_files <- dir_ls("out/scenario_processed/")

df_raw_nia_pia <- map_dfr(scenario_files, process_raw_nia_pia)

saveRDS(df_raw_nia_pia, "out/df_raw_nia_pia.rds")
