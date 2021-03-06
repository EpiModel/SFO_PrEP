source("R/utils-slurm_prep_helpers.R") # requires `purrr`
source("R/utils-slurm_wf.R")
test_simulation <- FALSE

# Set slurm parameters ---------------------------------------------------------
batch_per_set <- 700     # How many 28 replications to do per parameter
steps_to_keep <- NULL # Steps to keep in the output df. If NULL, return sim obj
partition <- "ckpt"     # On hyak, either ckpt or csde
job_name <- "SFO_choose_restart_low"
ssh_host <- "hyak_mox"
ssh_dir <- "gscratch/EpiModel_SFO/"

# Options passed to slurm_wf
slurm_ressources <- list(
  partition = partition,
  job_name = job_name,
  account = if (partition == "csde") "csde" else "csde-ckpt",
  n_cpus = 28,
  memory = 5 * 1e3, # in Mb and PER CPU
  walltime = 60
)

# Set orig, param, init, control -----------------------------------------------
#
lnt <- TRUE # if FALSE: set `require.lnt` to FALSE and adjust ` prep.start.prob`
source("R/utils-params.R", local = TRUE)

control <- control_msm(
  nsteps = 52 * 65, # add 4 for prep
  nsims = 28,
  ncores = 28,
  save.nwstats = FALSE,
  save.clin.hist = FALSE,
  verbose = FALSE
)

# Parameters to test -----------------------------------------------------------
#

#trans.scale = c(1.61, 0.836, 0.622)
#c(1.7, 0.8, 0.73333333)
## param_proposals <- list(
##   trans.scale = seq_cross( # 4^3 values to test; See utils-slurm_prep_helpers.R
##     c(1.4, 0.75, 0.7),
##     c(1.6, 0.85, 0.8),
##     length.out = 4
##   )
## )

param_proposals <- list(
  tx.halt.part.prob = list(
    ## param$tx.halt.part.prob * 0 # tx/i = .92 | sup/i = .68
    ## param$tx.halt.part.prob * 1 # tx/i = .84 | sup/i = .61
    param$tx.halt.part.prob * 2.4  # tx/i = .69 | sup/i = .50
  )
)

# Use this line to run only the default values
## param_proposals <- list(base_params__ = TRUE)

# Cross all possible combinations? Can grow super fast
test_all_combination <- FALSE
# Finalize param_proposal list
if (test_all_combination) {
  param_proposals <- purrr::cross(param_proposals)
} else {
  param_proposals <- transpose_ragged(param_proposals)
}

# Automatic --------------------------------------------------------------------
#
param_proposals <- rep(param_proposals, batch_per_set)
sim_nums <- seq_along(param_proposals)

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
info$param_proposals <- param_proposals

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
  FUN = run_netsim_fun,
  sim_num = sim_nums,
  param_proposal = param_proposals,
  MoreArgs = list(orig = orig, param = param, init = init, control = control,
                  info = info)
)

if (test_simulation) {
  control$nsteps = 3 * 52
  control$nsims = 1
  control$ncores = 1
  control$verbose = TRUE

  run_netsim_fun(
    param_proposals[[1]], sim_nums[[1]],
    orig, param, init, control, info
  )
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

write(job_name, file = fs::path(paths$local_out, paths$jobs_dir, "last_jobs"))
