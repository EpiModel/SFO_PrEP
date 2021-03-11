library(tidyverse)
df_outcomes <- readRDS("out/df_outcomes.rds")
df_flow <- readRDS("out/df_flow.rds")

# T1 ---------------------------------------------------------------------------
#
# get ART coverage
files <- c(
  "out/est/restart/restart_low.rds",
  "out/est/restart/restart_sfo.rds",
  "out/est/restart/restart_high.rds"
)

sims <- lapply(files, readRDS)

for (sim in sims) {
  as_tibble(sim) %>%
    filter(time > max(time - 3 * 52)) %>%
    mutate(art_cov = i_tx / i_dx) %>%
    summarise(across(
        art_cov,
        .fns = list(
          med = median,
          q1 = ~quantile(.x, prob = 0.25),
          q3 = ~quantile(.x, prob = 0.75)
        )
        )) %>%
    print()
}

# T2 ---------------------------------------------------------------------------

# prev-0.1%, incid-0.01, PIA-0.1%, NNT-0.1, time_on_PrEP-0.1
fmts <- list()
fmts[["prev"]] <- scales::label_percent(0.1)
fmts[["incid"]] <- scales::label_number(0.01)
fmts[["pia"]] <- scales::label_percent(0.1)
fmts[["nnt"]] <- scales::label_number(1)
fmts[["time_on_prep"]] <- scales::label_number(0.1)
fmts[["nia"]] <- scales::label_number(1)

format_t2 <- function(med, low, high, name) {
  f <- fmts[[name]]
  str_c(f(med), " (", f(low), ", ", f(high), ")")
}

t2_dat <- df_outcomes %>%
  filter(job == "p") %>%
  select(c(scenario,
      starts_with(c("prev", "incid__", "pia", "nnt", "time", "nia")))) %>%
  pivot_longer(cols = -scenario) %>%
  separate(name, into = c("name", "quantile"), sep = "___") %>%
  pivot_wider(names_from = quantile, values_from = value) %>%
  mutate(cleaned = pmap_chr(list(p500, p025, p975, name), format_t2)) %>%
  select(scenario, name, cleaned) %>%
  pivot_wider(names_from = name, values_from = cleaned) %>%
  arrange(scenario) %>%
  mutate(across(c(pia, nnt, nia), .fns = ~if_else(scenario == "p_025_sfo", "-", .x)))

if (!fs::dir_exists("out/table_dats")) fs::dir_create("out/table_dats")
write_csv(t2_dat, "out/table_dats/t2_dat.csv")

# EHE interv -------------------------------------------------------------------

# prev-0.1%, incid-0.01, PIA-0.1%, NNT-0.1, time_on_PrEP-0.1
f <- scales::label_percent(1)

format_ehe <- function(med, low, high) {
  str_c(f(med), " (", f(low), ", ", f(high), ")")
}

ehe_int_dat <- df_outcomes %>%
  filter(job == "p") %>%
  select(c(scenario, starts_with(c("incid_reduction")))) %>%
  pivot_longer(cols = -scenario) %>%
  separate(name, into = c("name", "quantile"), sep = "___") %>%
  pivot_wider(names_from = quantile, values_from = value) %>%
  mutate(cleaned = pmap_chr(list(p500, p025, p975), format_ehe)) %>%
  select(scenario, name, cleaned) %>%
  pivot_wider(names_from = name, values_from = cleaned) %>%
  arrange(scenario) %>%
  select(scenario, incid_reduction5y, incid_reduction)

if (!fs::dir_exists("out/table_dats")) fs::dir_create("out/table_dats")
write_csv(ehe_int_dat, "out/table_dats/ehe_int_dat.csv")


# T3 ---------------------------------------------------------------------------

# prev-0.1%, incid-0.01, PIA-0.1%, NNT-0.1, time_on_PrEP-0.1
job_names <- c(
  "sl" = "0sl",
  "ss" = "1ss",
  "sh" = "2sh"
)

names_names <- c(
  "incid" = "0incid",
  "nia" = "1nia",
  "pia" = "2pia",
  "nnt" = "3nnt"
)

fmts <- list()
fmts[["0incid"]] <- scales::label_number(0.01)
fmts[["1nia"]] <- scales::label_number(1)
fmts[["2pia"]] <- scales::label_percent(1)
fmts[["3nnt"]] <- scales::label_number(1)

format_t3 <- function(med, low, high, name) {
  f <- fmts[[name]]
  str_c(f(med), "\n(", f(low), ", ", f(high), ")")
}

t3_dat <- df_outcomes %>%
  filter(job != "p") %>%
  mutate(job = job_names[job]) %>%
  select(c(job, scenario, starts_with(c("pia", "nia", "nnt")))) %>%
  pivot_longer(cols = -c(job, scenario)) %>%
  separate(name, into = c("name", "quantile"), sep = "___") %>%
  mutate(name = names_names[name]) %>%
  pivot_wider(names_from = quantile, values_from = value) %>%
  mutate(
    jname = paste0(job, "_", name),
    cleaned = pmap_chr(list(p500, p025, p975, name), format_t3)
  ) %>%
  select(scenario, jname, cleaned) %>%
  arrange(jname) %>%
  pivot_wider(names_from = jname, values_from = cleaned) %>%
  arrange(scenario) %>%
  mutate(across(matches(c("pia", "nnt", "nia")),
                .fns = ~if_else(scenario == "s_0", "-", .x)))

if (!fs::dir_exists("out/table_dats")) fs::dir_create("out/table_dats")
write_csv(t3_dat, "out/table_dats/t3_dat.csv")

# T3 appendix (full) -----------------------------------------------------------

# prev-0.1%, incid-0.01, PIA-0.1%, NNT-0.1, time_on_PrEP-0.1
job_names <- c(
  "sl" = "0sl",
  "ss" = "1ss",
  "sh" = "2sh"
)

names_names <- c(
  "incid" = "0incid",
  "nia" = "1nia",
  "pia" = "2pia",
  "nnt" = "3nnt"
)

fmts <- list()
fmts[["0incid"]] <- scales::label_number(0.01)
fmts[["1nia"]] <- scales::label_number(1)
fmts[["2pia"]] <- scales::label_percent(1)
fmts[["3nnt"]] <- scales::label_number(1)

format_t3 <- function(med, low, high, name) {
  f <- fmts[[name]]
  str_c(f(med), "\n(", f(low), ", ", f(high), ")")
}

t3_dat <- df_outcomes %>%
  filter(job != "p") %>%
  mutate(job = job_names[job]) %>%
  select(c(job, scenario, starts_with(c("incid__", "pia", "nnt", "nia")))) %>%
  pivot_longer(cols = -c(job, scenario)) %>%
  separate(name, into = c("name", "quantile"), sep = "___") %>%
  mutate(name = names_names[name]) %>%
  pivot_wider(names_from = quantile, values_from = value) %>%
  mutate(
    jname = paste0(job, "_", name),
    cleaned = pmap_chr(list(p500, p025, p975, name), format_t3)
  ) %>%
  select(scenario, jname, cleaned) %>%
  arrange(jname) %>%
  pivot_wider(names_from = jname, values_from = cleaned) %>%
  arrange(scenario) %>%
  mutate(across(matches(c("pia", "nnt", "nia")),
                .fns = ~if_else(scenario == "s_0", "-", .x)))

if (!fs::dir_exists("out/table_dats")) fs::dir_create("out/table_dats")
write_csv(t3_dat, "out/table_dats/t3_app_dat.csv")

# EHE counterfactual -----------------------------------------------------------

# prev-0.1%, incid-0.01, PIA-0.1%, NNT-0.1, time_on_PrEP-0.1
f <- scales::label_percent(1)

job_names <- c(
  "sl" = "0sl",
  "ss" = "1ss",
  "sh" = "2sh"
)

names_names <- c(
  "incid_reduction5y" = "0incid_reduction5y",
  "incid_reduction" = "1incid_reduction"
)

ehe_count_dat <- df_outcomes %>%
  filter(job != "p") %>%
  mutate(job = job_names[job]) %>%
  select(c(job, scenario, starts_with(c("incid_reduction")))) %>%
  pivot_longer(cols = -c(job, scenario)) %>%
  separate(name, into = c("name", "quantile"), sep = "___") %>%
  mutate(name = names_names[name]) %>%
  arrange(job, scenario, name) %>%
  pivot_wider(names_from = quantile, values_from = value) %>%
  mutate(cleaned = pmap_chr(list(p500, p025, p975), format_ehe)) %>%
  select(job, scenario, name, cleaned) %>%
  pivot_wider(names_from = name, values_from = cleaned) %>%
  pivot_wider(names_from = job, values_from = -c(job, scenario))

ehe_count_dat <- ehe_count_dat[, c(1, 2, 5, 3, 6, 4, 7)]

if (!fs::dir_exists("out/table_dats")) fs::dir_create("out/table_dats")
write_csv(ehe_count_dat, "out/table_dats/ehe_count_dat.csv")

# Flows dat --------------------------------------------------------------------
f <- scales::label_percent(0.1)

format_flow <- function(med, low, high) {
  str_c(f(med), " (", f(low), ", ", f(high), ")")
}

job_names <- c(
  "sl" = "0sl",
  "ss" = "1ss",
  "sh" = "2sh"
)

flow_dat <- df_flow %>%
  mutate(
    time = (time - min(time)) / 52,
    job = job_names[job]
  ) %>%
  pivot_longer(cols = -c(job, scenario, time)) %>%
  separate(name, into = c("name", "quantile"), sep = "___") %>%
  filter(name %in% c("flow_si", "flow_txi", "flow_itx")) %>%
  arrange(job, scenario, time) %>%
  pivot_wider(names_from = quantile, values_from = value) %>%
  mutate(cleaned = pmap_chr(list(p500, p025, p975), format_flow)) %>%
  select(scenario, time, job, name, cleaned) %>%
  pivot_wider(names_from = job, values_from = cleaned) %>%
  select(name, scenario, time, everything()) %>%
  arrange(name, scenario, time)

if (!fs::dir_exists("out/table_dats")) fs::dir_create("out/table_dats")
write_csv(flow_dat, "out/table_dats/flow_dat.csv")

hmsg <- function(files, header, title, delete.file) {
	cat(files)
}

options(pager = hmsg)
?print
