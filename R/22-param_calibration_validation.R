library(data.table)
library(tidyverse)
library(RcppRoll)
theme_set(theme_light())

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

  sim_files <- fs::dir_ls(out_dir, regexp = "\\d+.rds")
  jobs[[job]]$data <- tibble()
  for (fle in sim_files) {
    btch <- as.numeric(str_extract(fs::path_file(fle), "[0-9]+"))
    sim <- readRDS(fle)
    dff <- if (is.null(infos$df_keep)) as.data.frame(sim) else sim

    dff$batch <- btch

    jobs[[job]]$data <- bind_rows(jobs[[job]]$data, dff)
  }
}

param_proposals <- jobs[[1]]$infos$param_proposals

source("R/utils-ana.R")
df <- jobs[[1]]$data
df_ana <- df %>%
  select(c(batch, sim, time, starts_with(c("i", "s", "linked", "prep_", "flow"))))

id_cols <- c("sim", "batch", "time")
names(df_ana) <- clean_pop_names(names(df_ana), id_cols)
id_cols <- c(id_cols, "pop")

df_ana <- df_ana %>%
  pivot_longer(-any_of(id_cols)) %>%
  separate(name, into = c("name", "pop"), sep = "___") %>%
  pivot_wider(
    id_cols = c(all_of(id_cols)),
    names_from = "name",
    values_from = value
  )

df_ana <- df_ana %>%
  mutate(
    prev    = i / (s + i),
    dx      = i_dx / i,
    tx      = i_tx / i_dx,
    sup     = i_sup / i,
    sup_dx  = i_sup / i_dx,
    sup_tx  = i_sup / i_tx,
    sup_dur = i_sup_dur / i_sup
  )

## df_ana$scenario <- rep(c(0, 1, 2.4), 8)[as.numeric(df_ana$batch)]
scenario <- map_chr(param_proposals, ~ paste0(.x$trans.scale, collapse = "-"))
df_ana$scenario <- scenario[as.numeric(df_ana$batch)]

df_ana %>%
  filter(
    pop != "ALL",
    time == max(time)
  ) %>%
  group_by(scenario, pop) %>%
  summarise(sup = median(prev)) %>%
  pivot_wider(id_cols = scenario, names_from = pop, values_from = sup) %>%
  mutate(
    B = B - 0.318,
    H = H - 0.273,
    W = W - 0.24,
    score = B^2 + H^2 + W^2
  ) %>%
  arrange(score)

df_ana %>%
  filter(pop == "ALL") %>%
  ## filter(scenario == 1) %>%
  ## group_by(scenario, time) %>%
  group_by(scenario, time, pop) %>%
  summarise(y = median(prev)) %>%
  ggplot(aes(x = time, y = y, col = as.character(scenario))) +
  ## ggplot(aes(x = time, y = y, col = as.character(pop))) +
  ## geom_vline(xintercept = 52 * 65 + 1) +
  ## geom_vline(xintercept = 52 * 69 + 1) +
  geom_line()

df_ana %>%
  group_by(scenario) %>%
  filter(time > max(time) - 6) %>%
  summarise(mean(flow_suppi))
