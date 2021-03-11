library(data.table)
library(tidyverse)

# One or many job_names
job_names <- c()
job_last_n <- 1 # if not NULL, get last N jobs. Otherwise, use job_names

if (!is.null(job_last_n)) {
  job_names <- tail(readLines("out/remote_jobs/last_jobs"), job_last_n)
}

jobs <- list()

# Read targets
source("R/utils-targets.R")

for (job in job_names) {
  jobs[[job]] <- list()
  infos <- readRDS(fs::path("out/remote_jobs/", job, "job_info.rds"))
  jobs[[job]]$infos <- infos

  out_dir <- fs::path(infos$paths$local_job_dir, "out")

  sim_files <- fs::dir_ls(out_dir, regexp = "\\d*.rds")
  jobs[[job]]$data <- data.table()
  btch <- 0
  for (fle in sim_files) {
    btch <- btch + 1
    sim <- readRDS(fle)
    dff <- if (is.null(infos$df_keep)) as.data.frame(sim) else sim

    setDT(dff)
    dff[, batch := btch]
    ## dff <- dff[, .SD, .SDcols = c("batch", "sim", "time", names(targets))]
    # do some transforms here (or not but risk memory overflow)
    #

    jobs[[job]]$data <- rbind(jobs[[job]]$data, dff)
  }
}

param_proposals <- jobs[[1]]$infos$param_proposals
df <- jobs[[1]]$data

df <- df %>%
  group_by(batch, sim) %>% #, time) %>%
  summarise(
    prev.B          = median(i.B / (s.B + i.B), na.rm = TRUE),
    linked3m.B      = median(linked3m.B, na.rm = TRUE),
    tx.B            = median(i_tx.B / i.B, na.rm = TRUE),
    sup_tx.B        = median(i_sup.B / i_tx.B, na.rm = TRUE),
    sup_dur.B       = median(i_sup_dur.B / i.B, na.rm = TRUE),
    prev.H          = median(i.H / (s.H + i.H), na.rm = TRUE),
    linked3m.H      = median(linked3m.H, na.rm = TRUE),
    tx.H            = median(i_tx.H / i.H, na.rm = TRUE),
    sup_tx.H        = median(i_sup.H / i_tx.H, na.rm = TRUE),
    sup_dur.H       = median(i_sup_dur.H / i.H, na.rm = TRUE),
    prev.W          = median(i.W / (s.W + i.W), na.rm = TRUE),
    linked3m.W      = median(linked3m.W, na.rm = TRUE),
    tx.W            = median(i_tx.W / i.W, na.rm = TRUE),
    sup_tx.W        = median(i_sup.W / i_tx.W, na.rm = TRUE),
    sup_dur.W       = median(i_sup_dur.W / i.W, na.rm = TRUE),
    prep_retained6m = median(s_prep_6m / s_prep, na.rm = TRUE),
    prep_any12m     = median(prep_any12m, na.rm = TRUE)
  )

tgts_names <- intersect(names(df), names(targets))

df_comp <- df %>%
  ## filter(time == max(time)) %>%
  select(c("batch", "sim", tgts_names))

for (nms in tgts_names) {
  df_comp[[nms]] <- df_comp[[nms]] - targets[[nms]]
}

df_comp <- df_comp %>%
  mutate(score = (prev.B^2 + prev.H^2 + prev.W^2)) %>%
  arrange(score)

df_comp %>%
  select(batch, sim, prev.B, prev.H, prev.W)


df_comp %>%
  pull(batch) %>%
  head(10) %>%
param_proposals[.]
