slurm_sim <- function(orig, param, init, control, batch,
         grp_btch = 0, trial_vals = NULL, trial_updaters = NULL) {

  library(EpiModelHIV)

  out_dir <- "slurm_wf/out/"

  if (! file.exists(paste0(out_dir, grp_btch)))
    dir.create(paste0(out_dir, grp_btch), recursive = TRUE)

  if (!is.null(trial_vals) && length(trial_vals) > 0)
    param[names(trial_vals)] <- lapply(trial_vals, function(x) x[[grp_btch]])

  if (!is.null(trial_updaters) && length(trial_updaters) > 0)
    param$param_updaters <- list(trial_updaters[[grp_btch]])

  sim <- netsim(orig, param, init, control)
  saveRDS(sim, paste0(out_dir, "/", grp_btch, "/sim", batch, ".rds"))
}

slurm_scenario <- function(orig, param, init, control,
                           scenario, scenario_name, scenario_start,
                           repl_num = 1, n_steps = 52) {
  library(EpiModelHIV)
  out_dir <- paste0("slurm_wf/out/", scenario_name, "/")

  if (! file.exists(out_dir))
    dir.create(out_dir, recursive = TRUE)

  if (!is.null(scenario) && length(scenario) > 0)
    param[names(scenario)] <- scenario

  if (is.null(param$param_updaters)) param$param_updaters <- list()

  param$param_updaters <- c(
    param$param_updaters,
    list(
      list(at = scenario_start,
           params = scenario)
    )
  )

  sim <- netsim(orig, param, init, control)

  library(data.table)
  library(fs)

  dt <- as.data.table(sim)
  dt <- dt[, .SD[(.N - n_steps + 1):.N] , by = "sim"
     ][, c("sim_id", "param_grp") :=
           .(paste0(..repl_num, "--", sim), scenario_name)][]

  saveRDS(dt, paste0(out_dir, "sim", repl_num, ".rds"))
}

slurm_scenario_combine <- function(sims_path = "slurm_wf/out") {
  library(data.table)
  library(fs)
  library(future.apply)
  plan(multiprocess)

  rdss <- dir_ls(sims_path, regexp = "[.]rds$", recurse = TRUE)
  dts <- future_lapply(rdss, readRDS)

  out <- rbindlist(dts)

  saveRDS(out, paste0(sims_path, "/scenarios_out.rds"))
  file_delete(rdss)
}

slurm_combine <- function(sims_path = "slurm_wf/out", n_steps = 1) {
  library(data.table)
  library(fs)
  source("R/utils-sim_choice.R")

  # Merge all simulations
  df_name <- paste0(sims_path, "/big_sim.rds")

  library(future.apply)
  plan(multiprocess) # requires a lot of ram (~400 files)

  df <- create_big_sim_df(sims_path, n_steps = n_steps)
  saveRDS(df, df_name, compress = TRUE)
}

slurm_art_counterfact <- function(orig, param, init, control,
                                  art_lvl, art_stop,
                                  repl_num = 1, n_steps = 13) {
  library(EpiModelHIV)
  out_dir <- paste0("slurm_wf/out/", art_lvl, "/")
  out_dt <- "slurm_wf/out/dt/"

  if (! file.exists(out_dir))
    dir.create(out_dir, recursive = TRUE)

  if (! file.exists(out_dt))
    dir.create(out_dt, recursive = TRUE)

  param$tx.halt.part.prob <- art_stop

  sim <- netsim(orig, param, init, control)


  saveRDS(sim, paste0(out_dir, "sim", repl_num, ".rds"))

  library(data.table)

  dt <- as.data.table(sim)
  dt <- dt[, .SD[(.N - n_steps + 1):.N] , by = "sim"
           ][, c("sim_id", "param_grp") :=
                 .(paste0(..repl_num, "--", sim), art_lvl)][]

  saveRDS(dt, paste0(out_dt, "/dt_", repl_num, ".rds"))
}

slurm_art_counterfact_combine <- function() {
  library(data.table)
  library(future.apply)
  plan(multiprocess)

  out_dir <- "slurm_wf/out/"

  ls_sims <- list.files(paste0(out_dir, "dt/"))

  dt_ls <- lapply(ls_sims, function(f) {
    readRDS(paste0(out_dir, "dt/", f))
  })

  dt <- rbindlist(dt_ls)

  saveRDS(dt, paste0(out_dir, "dt.rds"), compress = "xz")
}

slurm_scenario_csv <- function(orig, param, init, control,
                               id, prep_stop, prep_start, repl_num,
                               scenario_start, art_folder, n_steps = 1) {

  library(EpiModelHIV)
  out_dir <- paste0(art_folder, "/", id, "/")

  if (! file.exists(out_dir))
    dir.create(out_dir, recursive = TRUE)

  # scenarios parameters
  param$prep.discont.rate = param$prep.discont.rate * prep_stop
  param$prep.start.prob = param$prep.start.prob * prep_start

  sim <- netsim(orig, param, init, control)

  sim <- truncate_sim(sim, control$nsteps - n_steps)
  sim$network <- NULL
  sim$attr <- NULL
  sim$temp <- NULL
  sim$el <- NULL
  sim$p <- NULL

  saveRDS(sim, paste0(out_dir, "sim", repl_num, ".rds"), compress = FALSE)
}

slurm_scenario_csv_combine <- function(sims_path) {
  library(EpiModel)
  library(fs)

  out_dir <- path_dir(sims_path)
  scenarios_dir <- dir_ls(sims_path)

  for (scen_dir in scenarios_dir) {
    sim_files <- dir_ls(scen_dir, pattern = ".rds")

    art_prep_lvl <- path_split(scen_dir)[[1]]
    art_prep_lvl <- paste0(
      art_prep_lvl[length(art_prep_lvl) - c(1, 0)],
      collapse = "_"
    )

    for (i in seq_along(sim_files)) {
      sim <- readRDS(sim_files[[i]])

      if (i == 1) {
        out <- sim
      } else {
        out <- merge(out, sim, param.error = FALSE)
      }
    }

    saveRDS(out, paste0(out_dir, "/", art_prep_lvl,  ".rds"), compress = TRUE)
  }

}

slurm_scenario_interv_csv <- function(orig, param, init, control,
                                      id, prep_stop, prep_start, repl_num,
                                      scenario_start, art_folder, n_steps = 1) {

  library(EpiModelHIV)
  out_dir <- paste0(art_folder, "/", id, "/")

  if (! file.exists(out_dir))
    dir.create(out_dir, recursive = TRUE)

  # scenarios parameters
  param$param_updaters <- c(
    param$param_updaters,
    list(
      list(at = scenario_start,
           params = list(
             "prep.discont.rate" = param$prep.discont.rate * prep_stop,
             "prep.start.prob" = param$prep.start.prob * prep_start
           ))
    )
  )

  sim <- netsim(orig, param, init, control)

  sim <- truncate_sim(sim, control$nsteps - n_steps)
  sim$network <- NULL
  sim$attr <- NULL
  sim$temp <- NULL
  sim$el <- NULL
  sim$p <- NULL

  saveRDS(sim, paste0(out_dir, "sim", repl_num, "_interv.rds"), compress = FALSE)
}
