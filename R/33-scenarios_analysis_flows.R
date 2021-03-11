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
      scenario %in% c("s_0", "s_025", "s_05", "s_075")
    ) %>%
    mutate(
      ## inflow_sup = (flow_si + flow_suppi) / (s + i_sup),
      ## inflow_tx = (flow_si + flow_txi) / (s + i_tx),
      ## flow_i = flow_si + flow_txi - flow_itx,
      flow_si = flow_si / s,
      flow_itx = flow_itx / (i - i_tx),
      flow_txi = flow_txi / i_tx,
      flow_isupp = flow_isupp / (i - i_sup),
      flow_suppi = flow_suppi / i_sup
    )

  df_flow <- df_b %>%
    group_by(scenario, batch, sim, time) %>%
    summarise(across(matches("(in)?flow_"), ~ mean(.x, na.rm = TRUE))) %>%
    arrange(time) %>%
    mutate(across(
      matches("(in)?flow_"),
      ~ roll_meanl(.x, n_roll, fill = NA))
    ) %>%
    filter(
      time %in% c(
        prep_start,
        prep_start + 2 * 52,
        prep_start + 6 * 52,
        prep_start + 10 * 52
      )
    )

  df_flow_base <- df_flow %>%
    filter(scenario == "s_0") %>%
    group_by(scenario, time) %>%
    summarise(across(matches("(in)?flow_"), ~ median(.x, na.rm = TRUE)))

  ## # diff time by time using s_0 as ref
  ## df_flow_diff <- df_flow
  ## for (t in df_flow_base$time) {
  ##   for (col in names(df_flow_base)[- c(1,2)]) {
  ##     df_flow_diff[df_flow_diff$time == t, col] <-
  ##       (df_flow_diff[df_flow_diff$time == t, col] -
  ##       df_flow_base[df_flow_base$time == t,][[col]]) /
  ##       df_flow_base[df_flow_base$time == t,][[col]]
  ##   }
  ## }

  # diff using t0 as ref on each scenario
  df_flow_diff <- df_flow %>%
    group_by(scenario, batch, sim) %>%
    arrange(time) %>%
    mutate(across(matches("(in)?flow_"), ~ (.x - .x[1]) / .x[1]))

  df_flow_tot <- df_flow_diff %>%
    group_by(scenario, time) %>%
    select(-c(batch, sim)) %>%
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

  df_flow_tot
}

scenario_files <- dir_ls("out/scenario_processed/", regexp = "_s.\\.rds")
df_flow_tot <- map_dfr(scenario_files, process_flows)

saveRDS(df_flow_tot, "out/df_flow.rds")
