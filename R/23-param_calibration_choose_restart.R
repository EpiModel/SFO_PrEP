library(data.table)
library(future.apply)
plan(multiprocess, workers = 4)

# One or many job_names
job <- c(
  ## "SFO_choose_restart_low"
  "SFO_choose_restart_SFO"
  ## "SFO_choose_restart_HIGH"
)

process_rds <- function(fle) {
  sim <- readRDS(fle)
  dff <- as.data.table(sim)
  dff[, batch := fs::path_file(fle)]
  dff[time == max(time), .(
    batch = fs::path_file(fle),
    sim = sim,
    prev.B          = i.B / (s.B + i.B),
    linked3m.B      = linked3m.B,
    tx.B            = i_tx.B / i.B,
    sup_tx.B        = i_sup.B / i_tx.B,
    sup_dur.B       = i_sup_dur.B / i_sup.B,
    prev.H          = i.H / (s.H + i.H),
    linked3m.H      = linked3m.H,
    tx.H            = i_tx.H / i.H,
    sup_tx.H        = i_sup.H / i_tx.H,
    sup_dur.H       = i_sup_dur.H / i_sup.H,
    prev.W          = i.W / (s.W + i.W),
    linked3m.W      = linked3m.W,
    tx.W            = i_tx.W / i.W,
    sup_tx.W        = i_sup.W / i_tx.W,
    sup_dur.W       = i_sup_dur.W / i_sup.W
    ## prep_retained6m = median(s_prep_6m / s_prep, na.rm = TRUE),
    ## prep_any12m     = median(prep_any12m, na.rm = TRUE)
  )]
}

# Read targets
source("R/utils-targets.R")

infos <- readRDS(fs::path("out/remote_jobs/", job, "job_info.rds"))
out_dir <- fs::path(infos$paths$local_job_dir, "out")
sim_files <- fs::dir_ls(out_dir, regexp = "\\d*.rds")
param_proposals <- infos$param_proposals

ldf <- future_lapply(sim_files, process_rds)
df <- rbindlist(ldf)

saveRDS(df, fs::path("out/remote_jobs/", job, "df_choose.rds"))

# for the non SFO ones
if (job %in% c("SFO_choose_restart_low",  "SFO_choose_restart_HIGH"))
  targets <- targets[grepl("(linked)|(sup_tx)", names(targets))]
tgts_names <- intersect(names(df), names(targets))

library(tidyverse)

df_comp <- df %>%
  ## filter(time == max(time)) %>%
  select(c(batch, sim, any_of(tgts_names)))

for (nms in tgts_names) {
  df_comp[[nms]] <- df_comp[[nms]] - targets[[nms]]
}

df_comp[["score"]] <- 0

for (nms in tgts_names) {
  df_comp[["score"]] <- df_comp[["score"]] + (df_comp[[nms]]/sd(df_comp[[nms]]))^2
  ## df_comp[["score"]] <- df_comp[["score"]] + df_comp[[nms]]^2
  ## df_comp[["score"]] <- df_comp[["score"]] + abs(df_comp[[nms]]/sd(df_comp[[nms]]))
}

df_comp %>%
arrange(score)


library(EpiModel)
sims <- readRDS("out/est/restart/low_sim122.rds")
sim <- get_sims(sims, 1)
saveRDS(sim, "out/est/restart/restart_low.rds")

sims <- readRDS("out/est/restart/sfo_sim547.rds")
sim <- get_sims(sims, 8)
saveRDS(sim, "out/est/restart/restart_sfo.rds")

sims <- readRDS("out/est/restart/high_sim113.rds")
sim <- get_sims(sims, 10)
saveRDS(sim, "out/est/restart/restart_high.rds")
