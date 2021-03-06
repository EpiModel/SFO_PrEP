source("R/utils-slurm_prep_helpers.R") # requires `purrr`
source("R/utils-slurm_wf.R")
test_simulation <- FALSE

# Set slurm parameters ---------------------------------------------------------
batch_per_set <- 20      # How many 28 replications to do per parameter
steps_to_keep <- NULL# 15 * 52 # Steps to keep in the output df. If NULL, return sim obj
partition <- "ckpt"     # On hyak, either ckpt or csde
job_name <- "SFO_scenarios_sh"
ssh_host <- "hyak_mox"
ssh_dir <- "gscratch/EpiModel_SFO/"

# Options passed to slurm_wf
slurm_ressources <- list(
  partition = partition,
  job_name = job_name,
  account = if (partition == "csde") "csde" else "csde-ckpt",
  n_cpus = 28,
  memory = 5 * 1e3, # in Mb and PER CPU
  walltime = 15
)

# Set orig, param, init, control -----------------------------------------------
#
lnt <- TRUE # if FALSE: set `require.lnt` to FALSE and adjust ` prep.start.prob`
source("R/utils-params.R", local = TRUE)

## orig <- readRDS("out/est/restart/restart_low.rds")
## orig <- readRDS("out/est/restart/restart_sfo.rds")
orig <- readRDS("out/est/restart/restart_high.rds")

control <- control_msm(
  start = orig$control$nsteps + 1,
  nsteps = orig$control$nsteps + 1 + 52 * 14,
  nsims = 28,
  ncores = 28,
  save.nwstats = FALSE,
  initialize.FUN = reinit_msm,
  save.clin.hist = FALSE,
  verbose = FALSE
)

param <- orig$param
param$param_updaters <- list(list(at = 0, param = list()))

# Scenarios --------------------------------------------------------------------
source("R/utils-scenarios.R")

## scenarios <- scenarios[grepl("^p_", names(scenarios))]
scenarios <- scenarios[grepl("^s_", names(scenarios))]

# Automatic --------------------------------------------------------------------
#
updaters <- rep(scenarios, batch_per_set)
sim_nums <- seq_along(updaters)

# Required directories
paths <- make_job_paths(job_name, ssh_dir, ssh_host)
# Ensure that no job with this name is present
if (fs::dir_exists(paths$local_job_dir))
  stop("Folder: '", paths$local_job_dir,
       "' exists. Change `job_name` or delete the folder")

info <- list()
info$paths <- paths
info$job_name <- job_name
info$ssh_host <- ssh_host
info$root_dir <- fs::path(paths$jobs_dir, job_name, paths$slurm_wf)
info$df_keep <- steps_to_keep
info$updaterss <- updaters

slurm_wf_tmpl_dir("inst/slurm_wf/", info$root_dir, force = T)

shared_res <- list(
  partition = partition,
  account = if (partition == "csde") "csde" else "csde-ckpt",
  n_cpus = 28,
  memory = 5 * 1e3 # in Mb and PER CPU
)

slurm_wf_Map(
  info$root_dir,
  resources = slurm_ressources,
  FUN = run_netsim_updaters_fun,
  sim_num = sim_nums,
  updaters = updaters,
  scenario = names(updaters),
  MoreArgs = list(orig = orig, param = param, init = init, control = control,
                  info = info)
)

if (test_simulation) {
  control$nsims <- 2
  control$ncores <- 2
  control$verbose <- FALSE
  n <- 2

  run_netsim_updaters_fun(
    updaters[[n]], sim_nums[[n]], scenario = names(updaters)[[n]],
    orig, param, init, control, info
  )

  sim <- readRDS(glue::glue("remote_jobs/{job_name}/slurm/out/sim{n}.rds"))
  df <- as.data.frame(sim)

  library(tidyverse)

  df %>%
    filter(time > 3300) %>%
    group_by(time) %>%
    summarise(y = median(s_prep / s_prep_elig, na.rm = TRUE)) %>%
    ggplot(aes(x = time, y = y)) + geom_line() + geom_vline(xintercept = 3589)
}


# Create out dir and save params
fs::dir_create(fs::path(paths$local_out, paths$jobs_dir))
saveRDS(info, fs::path(paths$remote_job_dir, "job_info.rds"))
# move slurm to out and cleanup
fs::file_move(paths$remote_job_dir, fs::path(paths$local_out, paths$jobs_dir))
fs::dir_delete(paths$jobs_dir)


scp_send_script <- c(
  "#!/bin/sh",
  "",
  paste0("ssh ", info$ssh_host, " \"mkdir -p '", info$ssh_host, ":",
         fs::path(paths$ssh_proj, paths$jobs_dir) ,"'\""),
  paste0("rsync -vr --exclude '", "out/*", "' '",
         paths$local_job_dir, "' '",
         info$ssh_host, ":", fs::path(paths$ssh_proj, paths$jobs_dir, "'"))
  )

scp_get_script <- c(
  "#!/bin/sh",
  "",
  paste0("rsync -vur '",
         info$ssh_host, ":", fs::path(paths$ssh_job_dir, paths$slurm_out),
         "' '", paths$local_job_dir, "'")
)

writeLines(scp_send_script, fs::path(paths$local_job_dir, "send_to_ssh.sh"))
writeLines(scp_get_script, fs::path(paths$local_job_dir, "get_from_ssh.sh"))

write(job_name, file = fs::path(paths$local_out, paths$jobs_dir, "last_jobs"),
       append = TRUE)
