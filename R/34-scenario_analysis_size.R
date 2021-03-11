library(fs)
library(tidyverse)
library(RcppRoll)
theme_set(theme_light())

process_flows <- function(fle) {
  n_roll <- 13
  prep_start <- 52 * (65) + 1
  ana_beg <- prep_start + 4 * 52
  range <- c(prep_start, prep_start + 10 * 52)

  e_ls <- readRDS(fle)
  df <- e_ls$data
  infos <- e_ls$infos
  job <- str_split(infos$job_name, "_")[[1]][3]
  range <- c(prep_start, prep_start + 10 * 52)

  # First triming
  df_b <- df %>%
    filter(
      scenario %in% c("s_0", "s_025_sfo", "s_05", "s_075")
    ) %>%
    mutate(
      sflow_si = flow_si,
      sflow_itx = flow_itx,
      sflow_txi = flow_txi,
      ## sflow_si = 1e6 * flow_si / (s * i),
      ## sflow_itx = flow_itx / (i - i_tx),
      ## sflow_txi = flow_txi / i_tx,
      s = s,
      i = i - i_tx,
      tx = i_tx
    ) %>%
    select(scenario, time, s, i, tx, starts_with("sflow")) %>%
    group_by(scenario, time) %>%
    summarise(across(
      everything(),
      list(
        p025 = ~ quantile(.x, 0.025, na.rm = TRUE),
        p500 = ~ quantile(.x, 0.5, na.rm = TRUE),
        p975 = ~ quantile(.x, 0.975, na.rm = TRUE)
      ),
      .names = "{col}___{fn}"
    )) %>%
    mutate(job = str_split(infos$job_name, "_")[[1]][3])

  df_b
}

scenario_files <- dir_ls("out/scenario_processed/", regexp = "_s.\\.rds")
df_size <- map_dfr(scenario_files, process_flows)

saveRDS(df_size, "out/df_size.rds")

prep_start <- 52 * (65) + 1
prep_stop <- prep_start + 1 * 52

df_size %>%
  filter(time == prep_stop, job != "ss", scenario %in% c("s_0", "s_025_sfo")) %>%
  select(job, scenario, s___p500, i___p500, tx___p500)

df_size %>%
  filter(
    between(time, prep_stop, prep_stop + 52),
    scenario %in% c("s_0", "s_05"),
    job != "sh"
  ) %>%
  select(
    job, scenario, time,
    sflow_si___p500,
    sflow_itx___p500,
    sflow_txi___p500
  ) %>%
  group_by(job, scenario) %>%
  summarise(across(everything(), ~ sum(.x, na.rm = T)))
