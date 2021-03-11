library(tidyverse)
library(furrr)
source("R/utils-ana.R")
theme_set(theme_light())
plan(multiprocess, workers = 2)

process_one <- function(fle, btch, infos, start) {
  df <- if (is.null(infos$df_keep)) as_tibble(readRDS(fle)) else readRDS(fle)

  df <- df %>%
    select(c(
      sim, time,
      starts_with(c("i", "s", "linked", "prep_", "flow",
                    "median_time_on"))
    )) %>%
    filter(time >= start)

  cols_k <- names(df) %>%
    str_subset("[^\\.(H|B|W)]$")

  df <- select(df, all_of(cols_k))

  ## # pivot with pop
  ## id_cols <- c("sim", "time")
  ## names(df) <- clean_pop_names(names(df), id_cols)
  ## id_cols <- c(id_cols, "pop")

  ## df <- df %>%
  ##   pivot_longer(-any_of(id_cols)) %>%
  ##   separate(name, into = c("name", "pop"), sep = "___") %>%
  ##   pivot_wider(
  ##     id_cols = c(all_of(id_cols)),
  ##     names_from = "name",
  ##     values_from = value
  ##   )

  df <- df %>%
    mutate(
      prev    = i / (s + i),
      dx      = i_dx / i,
      tx      = i_tx / i_dx,
      sup     = i_sup / i,
      sup_dx  = i_sup / i_dx,
      sup_tx  = i_sup / i_tx,
      sup_dur = i_sup_dur / i_sup
    )

  mutate(df, batch = btch)
}

# One or many job_names
job_names <- paste0("SFO_scenarios_", c("p", "sl", "ss", "sh"))

job_names <- job_names[4]
## jn <- job_names[3]

# Read targets
prep_start <- 52 * (65) + 1
ana_beg <- prep_start + 4 * 52

for (jn in job_names) {
  job <- list()
  infos <- readRDS(fs::path("out/remote_jobs/", jn, "job_info.rds"))
  job$infos <- infos

  out_dir <- fs::path(infos$paths$local_job_dir, "out")
  sim_files <- fs::dir_ls(out_dir, regexp = "\\d*.rds")
  batches <- as.numeric(str_extract(sim_files, "\\d+"))

  job$data <- future_map2_dfr(sim_files, batches, process_one,
                              infos = infos, start = prep_start)

  job$data$scenario <- names(infos$updaterss)[job$data$batch]
  saveRDS(job, paste0("out/scenario_processed/", jn, ".rds"))

  rm(job)
  gc()
}

## df <- jobs[[1]]$data
## glimpse(df)

## df %>%
##   group_by(time) %>%
##   summarise(y = median(s_prep / s_prep_elig, na.rm = T)) %>%
##   ggplot(aes(x = time, y = y)) +
##     geom_line() +
##     geom_vline(xintercept = prep_start) +
##     geom_vline(xintercept = ana_beg) +
##     geom_vline(xintercept = ana_beg + 4 * 52)
