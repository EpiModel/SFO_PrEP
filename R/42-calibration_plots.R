get_targets <- function(fle, tgts) {
  df_tmp <- as_tibble(readRDS(fle)) %>%
    filter(time %% 4 == T) %>%
    mutate(
      prev.B = i.B / (s.B + i.B),
      dx.B = i_dx.B / i.B,
      tx.B = i_tx.B / i_dx.B,
      sup_dx.B = i_sup.B / i_dx.B,
      sup_tx.B = i_sup.B / i_tx.B,
      sup_dur.B = i_sup_dur.B / i_sup.B,
      prev.H = i.H / (s.H + i.H),
      dx.H = i_dx.H / i.H,
      tx.H = i_tx.H / i_dx.H,
      sup_dx.H = i_sup.H / i_dx.H,
      sup_tx.H = i_sup.H / i_tx.H,
      sup_dur.H = i_sup_dur.H / i_sup.H,
      prev.W = i.W / (s.W + i.W),
      dx.W = i_dx.W / i.W,
      tx.W = i_tx.W / i_dx.W,
      sup_dx.W = i_sup.W / i_dx.W,
      sup_tx.W = i_sup.W / i_tx.W,
      sup_dur.W = i_sup_dur.W / i_sup.W,
      prep_retained6m = s_prep_6m / s_prep
    ) %>%
    select(c(time, all_of(names(tgts)))) %>%
    pivot_longer(-time) %>%
    separate(name, into = c("name", "pop"), sep = "\\.")
  df_tmp
}

library(tidyverse)
library(future.apply)
plan(multiprocess, workers = 4)

source("R/utils-targets.R")
races <- c(
  "B" = "Black",
  "H" = "Hispanic",
  "W" = "White/Other"
)
filenames <- fs::dir_ls("/media/BACKUP/home_backup/SFO_choose_restart_SFO/out")

# Prevalence ---
tgts <- targets[str_starts(names(targets), "prev")]
df_tgts <- tibble(name = names(tgts), value = tgts) %>%
  mutate(pop = races[str_sub(name, -1, -1)])

if (!file.exists("out/df_calplot_prev.rds")) {
  df_targets <- bind_rows(future_lapply(filenames, get_targets, tgts = tgts))
  df_targets <- df_targets %>%
    group_by(time, pop) %>%
    summarise(
      q1 = quantile(value, prob = 0.25, na.rm = TRUE),
      q2 = quantile(value, prob = 0.5, na.rm = TRUE),
      q3 = quantile(value, prob = 0.75, na.rm = TRUE)
    )
  gc()
  df_targets$pop <- races[df_targets$pop]
  saveRDS(df_targets, "out/df_calplot_prev.rds")
} else {
  df_targets <- readRDS("out/df_calplot_prev.rds")
}

ggplot(df_targets, aes(x = time / 52, y = q2, ymin = q1, ymax = q3, color = pop,
         fill = pop)) +
  geom_hline(
    data = df_tgts, aes(yintercept = value, color = pop),
    linetype = 2, alpha = .8
  ) +
  geom_ribbon(alpha = 0.5, size = 0) +
  geom_line(size = 0.5) +
  geom_text(
    data = df_tgts,
    aes(y = value + 0.015, color = pop, label = value, x = 67),
    inherit.aes = FALSE, size = 3
  ) +
  theme_classic() +
  scale_y_continuous(lim = c(0.15, 0.55)) +
  theme(
    legend.position = "right",
    axis.text.x = element_text(margin = margin(5, 0, 10, 0, "pt")),
    axis.text.y = element_text(margin = margin(0, 5, 0, 10, "pt")),
    text = element_text(size = 10),
    aspect.ratio = 1/2
  ) +
  guides(
    col = guide_legend(title = "Race"),
    fill = guide_legend(title = "Race")
  ) +
  ylab("HIV prevalence") +
  xlab("Time (years)") +
  ggtitle(" ")

ggsave(
  "out/plots/additional_calib_prev.jpeg",
  device = "jpeg", dpi = 600,
  height = 5, width = 7,
  units = "in"
)
rm(df_targets)

# tx
tgts <- targets[str_starts(names(targets), "tx")]
df_tgts <- tibble(name = names(tgts), value = tgts) %>%
  mutate(pop = races[str_sub(name, -1, -1)])

if (!file.exists("out/df_calplot_tx.rds")) {
  df_targets <- bind_rows(future_lapply(filenames, get_targets, tgts = tgts))
  df_targets <- df_targets %>%
    group_by(time, pop) %>%
    summarise(
      q1 = quantile(value, prob = 0.25, na.rm = TRUE),
      q2 = quantile(value, prob = 0.5, na.rm = TRUE),
      q3 = quantile(value, prob = 0.75, na.rm = TRUE)
    )
  gc()
  df_targets$pop <- races[df_targets$pop]
  saveRDS(df_targets, "out/df_calplot_tx.rds")
} else {
  df_targets <- readRDS("out/df_calplot_tx.rds")
}

ggplot(df_targets, aes(x = time / 52, y = q2, ymin = q1, ymax = q3, color = pop,
         fill = pop)) +
  geom_hline(
    data = df_tgts, aes(yintercept = value, color = pop),
    linetype = 2, alpha = .8
  ) +
  geom_ribbon(alpha = 0.5, size = 0) +
  geom_line(size = 0.5) +
  geom_text(
    data = df_tgts,
    aes(y = value + 0.01, color = pop, label = value, x = 67),
    inherit.aes = FALSE, size = 3
  ) +
  theme_classic() +
  scale_y_continuous(limits = c(0.65, 1)) +
  theme(
    legend.position = "right",
    legend.key.size = unit(0.5, "cm"),
    axis.text.x = element_text(margin = margin(5, 0, 10, 0, "pt")),
    axis.text.y = element_text(margin = margin(0, 5, 0, 10, "pt")),
    text = element_text(size = 10),
    aspect.ratio = 1/2
  ) +
  guides(
    col = guide_legend(title = "Race"),
    fill = guide_legend(title = "Race")
  ) +
  ylab("Diagnosed Treated") +
  xlab("Time (years)") +
  ggtitle(" ")

ggsave(
  "out/plots/additional_calib_tx.jpeg",
  device = "jpeg", dpi = 600,
  height = 5, width = 7,
  units = "in"
)

rm(df_targets)
# sup_tx
tgts <- targets[str_starts(names(targets), "sup_tx")]
df_tgts <- tibble(name = names(tgts), value = tgts) %>%
  mutate(pop = races[str_sub(name, -1, -1)])

if (!file.exists("out/df_calplot_sup_tx.rds")) {
  df_targets <- bind_rows(future_lapply(filenames, get_targets, tgts = tgts))
  df_targets <- df_targets %>%
    group_by(time, pop) %>%
    summarise(
      q1 = quantile(value, prob = 0.25, na.rm = TRUE),
      q2 = quantile(value, prob = 0.5, na.rm = TRUE),
      q3 = quantile(value, prob = 0.75, na.rm = TRUE)
    )
  gc()
  df_targets$pop <- races[df_targets$pop]
  saveRDS(df_targets, "out/df_calplot_sup_tx.rds")
} else {
  df_targets <- readRDS("out/df_calplot_sup_tx.rds")
}

ggplot(df_targets, aes(x = time / 52, y = q2, ymin = q1, ymax = q3, color = pop,
         fill = pop)) +
  geom_hline(
    data = df_tgts, aes(yintercept = value, color = pop),
    linetype = 2, alpha = .8
  ) +
  geom_ribbon(alpha = 0.5, size = 0) +
  geom_line(size = 0.5) +
  theme_classic() +
  scale_y_continuous(limits = c(0.55, 0.85)) +
  geom_text(
    data = df_tgts,
    aes(y = value + 0.015, color = pop, label = value, x = 67),
    inherit.aes = FALSE, size = 3
  ) +
  theme(
    legend.position = "right",
    legend.key.size = unit(0.5, "cm"),
    axis.text.x = element_text(margin = margin(5, 0, 10, 0, "pt")),
    axis.text.y = element_text(margin = margin(0, 5, 0, 10, "pt")),
    text = element_text(size = 10),
    aspect.ratio = 1/2
  ) +
  guides(
    col = guide_legend(title = "Race"),
    fill = guide_legend(title = "Race")
  ) +
  ylab("Treated Virally Suppressed") +
  xlab("Time (years)") +
  ggtitle(" ")

ggsave(
  "out/plots/additional_calib_sup_tx.jpeg",
  device = "jpeg", dpi = 600,
  height = 5, width = 7,
  units = "in"
)

rm(df_targets)
gc()
# linked3m
tgts <- targets[str_starts(names(targets), "linked3m")]
df_tgts <- tibble(name = names(tgts), value = tgts) %>%
  mutate(pop = races[str_sub(name, -1, -1)])

if (!file.exists("out/df_calplot_linked3m.rds")) {
  df_targets <- bind_rows(future_lapply(filenames, get_targets, tgts = tgts))
  df_targets <- df_targets %>%
    group_by(time, pop) %>%
    summarise(
      q1 = quantile(value, prob = 0.25, na.rm = TRUE),
      q2 = quantile(value, prob = 0.5, na.rm = TRUE),
      q3 = quantile(value, prob = 0.75, na.rm = TRUE)
    )
  gc()
  df_targets$pop <- races[df_targets$pop]
  saveRDS(df_targets, "out/df_calplot_linked3m.rds")
} else {
  df_targets <- readRDS("out/df_calplot_linked3m.rds")
}

ggplot(df_targets, aes(x = time / 52, y = q2, ymin = q1, ymax = q3, color = pop,
         fill = pop)) +
  geom_hline(
    data = df_tgts, aes(yintercept = value, color = pop),
    linetype = 2, alpha = .8
  ) +
  geom_ribbon(alpha = 0.5, size = 0) +
  geom_line(size = 0.5) +
  geom_text(
    data = df_tgts,
    aes(y = value - 0.01, color = pop, label = value, x = 67),
    inherit.aes = FALSE, size = 3
  ) +
  theme_classic() +
  scale_y_continuous(limits = c(0.65, 1)) +
  theme(
    legend.position = "right",
    legend.key.size = unit(0.5, "cm"),
    axis.text.x = element_text(margin = margin(5, 0, 10, 0, "pt")),
    axis.text.y = element_text(margin = margin(0, 5, 0, 10, "pt")),
    text = element_text(size = 10),
    aspect.ratio = 1/2
  ) +
  guides(
    col = guide_legend(title = "Race"),
    fill = guide_legend(title = "Race")
  ) +
  ylab("Diagnosed Linked to Care Within 3 Months") +
  xlab("Time (years)") +
  ggtitle(" ")

ggsave(
  "out/plots/additional_linked3m.jpeg",
  device = "jpeg", dpi = 600,
  height = 5, width = 7,
  units = "in"
)

rm(df_targets)
gc()
