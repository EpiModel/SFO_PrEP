library(tidyverse)
library(metR)
theme_set(theme_classic())

prep_labels <- c(
  "p_025_sfo" = "25.0% PrEP coverage",
  "p_0375"    = "37.5% PrEP coverage",
  "p_05"      = "50.0% PrEP coverage",
  "p_0625"    = "62.5% PrEP coverage",
  "p_075"     = "75.0% PrEP coverage",
  "s_0"       = "No PrEP",
  "s_0125"    = "12.5% PrEP coverage",
  "s_025_sfo" = "25.0% PrEP coverage",
  "s_0375"    = "37.5% PrEP coverage",
  "s_05"      = "50.0% PrEP coverage",
  "s_0625"    = "62.5% PrEP coverage",
  "s_075"     = "75.0% PrEP coverage"
)

art_labels <- c(
  "sl" = "Low ART",
  "ss" = "Empirical ART",
  "sh" = "High ART"
)

# PrEP cov - primary -----------------------------------------------------------
df <- readRDS("out/scenario_processed/SFO_scenarios_p.rds")$data

df <- df %>%
  mutate(
    year = (time - min(time)) / 52,
    prep_lvl = prep_labels[scenario]
  ) %>%
  group_by(prep_lvl, year) %>%
  summarise(prep_cov = median(s_prep / s_prep_elig, na.rm = TRUE))

ggplot(df, aes(x = year, y = prep_cov, col = prep_lvl, label = prep_lvl)) +
  geom_line() +
  geom_vline(xintercept = 4, col = "grey") +
  geom_text(
    data = filter(df, year == max(year)),
    vjust = -1, hjust = 1, size = 3,
  ) +
  annotate(
    "label", label = "Initial PrEP ramp up",
    col = "black", x = 2, y = 0.95, size = 2.5
  ) +
  annotate(
    "label", label = "Intervention period",
    col = "black", x = 9, y = 0.95, size = 2.5
  ) +
  scale_color_brewer(palette = "Dark2") +
  scale_y_continuous(breaks = seq(0, 1, 0.125), limits = c(0, 1),
                     labels = scales::label_percent(accuracy = 0.1),
                     expand = c(0, 0)) +
  scale_x_continuous(breaks = seq(0, 15, 2), expand = c(0, 0)) +
  theme(
    legend.position = "none",
    axis.text.x = element_text(margin = margin(5, 0, 10, 0, "pt")),
    axis.text.y = element_text(margin = margin(0, 5, 0, 10, "pt")),
    text = element_text(size = 10),
    aspect.ratio = 1
  ) +
  ylab("PrEP Coverage") +
  xlab("Time (years)") +
  ggtitle(" ")

ggsave(
  "out/plots/prep_cov_1.jpeg",
  device = "jpeg", dpi = 600,
  height = 5, width = 6, units = "in"
)

# PrEP cov - secondary ---------------------------------------------------------

jobs <- c("sl", "ss", "sh")
df_l <- vector("list", 3)
names(df_l) <- jobs
for (j in jobs) {
  fle <- paste0("out/scenario_processed/SFO_scenarios_", j, ".rds")
  df_l[[j]] <- readRDS(fle)$data %>%
    select(scenario, time, s_prep, s_prep_elig) %>%
    mutate(
        year = (time - min(time)) / 52,
        prep_lvl = prep_labels[scenario]
    ) %>%
    filter(year <= 10)
}

df <- bind_rows(df_l) %>%
  group_by(prep_lvl, year) %>%
  summarise(prep_cov = median(s_prep / s_prep_elig, na.rm = TRUE))

ggplot(df, aes(x = year, y = prep_cov, col = prep_lvl, label = prep_lvl)) +
  geom_line() +
  geom_vline(xintercept = 4, col = "grey") +
  geom_text(
    data = filter(df, year == max(year)),
    vjust = -1, hjust = 1, size = 3
  ) +
  annotate(
    "label", label = "PrEP Coverage Calibration Point",
    col = "black", x = 6, y = 0.95, size = 2.5
  ) +
  scale_color_brewer(palette = "Dark2") +
  scale_y_continuous(breaks = seq(0, 1, 0.125), limits = c(0, 1),
                     labels = scales::label_percent(accuracy = 0.1),
                     expand = c(0, 0)) +
  scale_x_continuous(breaks = seq(0, 15, 2), expand = c(0, 0)) +
  theme(
    legend.position = "none",
    axis.text.x = element_text(margin = margin(5, 0, 10, 0, "pt")),
    axis.text.y = element_text(margin = margin(0, 5, 0, 10, "pt")),
    text = element_text(size = 10),
    aspect.ratio = 1
  ) +
  ylab("PrEP Coverage") +
  xlab("Time (years)") +
  ggtitle(" ")

ggsave(
  "out/plots/prep_cov_2.jpeg",
  device = "jpeg", dpi = 600,
  height = 5, width = 6,
  units = "in"
)

# Incid | prep - ART: box ------------------------------------------------------

prep_labels <- c(
  "s_0"       = "PrEP 0% ",
  "s_0125"    = "PrEP 12.5% ",
  "s_025_sfo" = "PrEP 25.0% ",
  "s_0375"    = "PrEP 37.5% ",
  "s_05"      = "PrEP 50.0% ",
  "s_0625"    = "PrEP 62.5% ",
  "s_075"     = "PrEP 75.0% "
)

jobs <- c("sl", "ss", "sh")
df_l <- vector("list", 3)
names(df_l) <- jobs
for (j in jobs) {
  fle <- paste0("out/scenario_processed/SFO_scenarios_", j, ".rds")
  df_l[[j]] <- readRDS(fle)$data %>%
    filter(between(time, min(time) + 10 - 6, min(time) + 52 * 10 + 7)) %>%
    group_by(batch, scenario, sim) %>%
    summarise(
      ## prep_cov = mean(s_prep / s_prep_elig),
      ## art_cov = mean(i_tx / i),
      incid_pyar = mean(incid) / mean(s) * 5200
    ) %>%
    mutate(
      ## prep_cov = if_else(is.na(prep_cov), 0, prep_cov),
      ## art_cov = if_else(is.na(art_cov), 0, art_cov),
      art_lvl = art_labels[j],
      prep_lvl = prep_labels[scenario]
    )
}

df <- bind_rows(df_l) %>%
  mutate(
    prep_lvl = factor(prep_lvl, levels = prep_labels),
    art_lvl = factor(art_lvl, levels = art_labels)
  )

ggplot(df, aes(x = prep_lvl, y = incid_pyar, fill = art_lvl)) +
  geom_boxplot(outlier.shape = NA, size = 0.4) +
  scale_fill_discrete("") +
  xlab("PrEP coverage scenario") +
  ylab("Incidence per 100 PYAR") +
  theme(
    legend.position = c(0.9, 0.75),
    axis.text.x = element_text(margin = margin(5, 0, 10, 0, "pt")),
    axis.text.y = element_text(margin = margin(0, 5, 0, 10, "pt")),
    text = element_text(size = 10) # increase font size in all elts
  ) +
  ggtitle(" ")

ggsave(
  "out/plots/art_prep_inc.jpeg",
  device = "jpeg", dpi = 600,
  height = 4.5, width = 8,
  units = "in"
)

# PIA | prep - ART: box ------------------------------------------------------

prep_labels <- c(
  "s_0"       = "0% ",
  "s_0125"    = "12.5% ",
  "s_025_sfo" = "25.0% ",
  "s_0375"    = "37.5% ",
  "s_05"      = "50.0% ",
  "s_0625"    = "62.5% ",
  "s_075"     = "75.0% "
)

df_raw <- readRDS("out/df_raw_nia_pia.rds")

df <- df_raw %>%
  filter(
    job %in% c("sl", "ss", "sh"),
    scenario != "s_0"
  ) %>%
  mutate(
    art_lvl = art_labels[job],
    art_lvl = factor(art_lvl, levels = art_labels),
    prep_lvl = prep_labels[scenario],
    prep_lvl = factor(prep_lvl, levels = prep_labels)
  )

p_nia <- ggplot(df, aes(x = prep_lvl, y = nia, fill = art_lvl)) +
  geom_boxplot(outlier.shape = NA, size = 0.4) +
  scale_fill_discrete("") +
  xlab("PrEP coverage scenario") +
  ylab("Number of Infections Averted Over 10 Years") +
  theme(
    #legend.position = c(0.9, 0.2),
    legend.position = "top",
    axis.text.x = element_text(margin = margin(5, 0, 10, 0, "pt")),
    axis.text.y = element_text(margin = margin(0, 5, 0, 10, "pt")),
    text = element_text(size = 10) # increase font size in all elts
  ) +
  ggtitle(" ")

ggsave(
  "out/plots/art_prep_nia.jpeg",
  plot = p_nia,
  device = "jpeg", dpi = 600,
  height = 4.5, width = 8,
  units = "in"
)

p_pia <- ggplot(df, aes(x = prep_lvl, y = pia, fill = art_lvl)) +
  geom_boxplot(outlier.shape = NA, size = 0.4) +
  scale_fill_discrete("") +
  xlab("PrEP coverage scenario") +
  ylab("Percent of Infections Averted Over 10 Years") +
  theme(
    legend.position = "top",
    axis.text.x = element_text(margin = margin(5, 0, 10, 0, "pt")),
    axis.text.y = element_text(margin = margin(0, 5, 0, 10, "pt")),
    text = element_text(size = 10) # increase font size in all elts
  ) +
  ggtitle(" ")

ggsave(
  "out/plots/art_prep_pia.jpeg",
  plot = p_pia,
  device = "jpeg", dpi = 600,
  height = 4.5, width = 8,
  units = "in"
)

ggplot(df, aes(x = prep_lvl, y = nnt, fill = art_lvl)) +
  geom_boxplot(outlier.shape = NA, size = 0.4) +
  scale_fill_discrete("") +
  xlab("PrEP coverage scenario") +
  ylab("Number Needed to Treat") +
  ylim(0, 150) +
  theme(
    legend.position = "top",
    axis.text.x = element_text(margin = margin(5, 0, 10, 0, "pt")),
    axis.text.y = element_text(margin = margin(0, 5, 0, 10, "pt")),
    text = element_text(size = 10) # increase font size in all elts
  ) +
  ggtitle(" ")

ggsave(
  "out/plots/art_prep_nnt.jpeg",
  device = "jpeg", dpi = 600,
  height = 4.5, width = 8,
  units = "in"
)

p_nia_pia <- gridExtra::grid.arrange(p_pia, p_nia, ncol = 2, nrow = 1)

ggsave(
  "out/plots/art_prep_nia_pia.jpeg",
  plot = p_nia_pia,
  device = "jpeg", dpi = 600,
  height = 4.5, width = 8,
  units = "in"
)

# HIV prev - secondary ---------------------------------------------------------

jobs <- c("sl", "ss", "sh")
df_l <- vector("list", 3)
names(df_l) <- jobs
for (j in jobs) {
  fle <- paste0("out/scenario_processed/SFO_scenarios_", j, ".rds")
  df_l[[j]] <- readRDS(fle)$data %>%
    select(scenario, time, s, i, i_tx, incid) %>%
    mutate(
      year = (time - min(time)) / 52,
      prep_lvl = prep_labels[scenario],
      art_lvl = art_labels[j],
    ) %>%
    filter(year <= 10)
}

df <- bind_rows(df_l) %>%
  mutate(art_lvl = factor(art_lvl, levels = art_labels)) %>%
  group_by(art_lvl, prep_lvl, year) %>%
  summarise(
    prev = median(i / (i + s) , na.rm = TRUE),
    prev_infectious = median((i - i_tx) / (i + s), na.rm = TRUE),
    incid_pyar = mean(incid) / mean(s) * 5200
  )

ggplot(df, aes(x = year, y = prev, col = prep_lvl, label = prep_lvl)) +
  geom_line() +
  geom_vline(xintercept = 4, col = "grey") +
  scale_color_brewer(palette = "Dark2") +
  scale_y_continuous(limits = c(0.07, 0.35),
                     labels = scales::label_percent(accuracy = 1),
                     expand = c(0, 0)) +
  scale_x_continuous(breaks = seq(0, 15, 2), expand = c(0, 0)) +
  facet_grid(rows = vars(art_lvl)) +
  theme_bw() +
  theme(
    legend.position = "right",
    axis.text.x = element_text(margin = margin(5, 0, 10, 0, "pt")),
    axis.text.y = element_text(margin = margin(0, 5, 0, 10, "pt")),
    text = element_text(size = 10),
    aspect.ratio = 1/3
  ) +
  guides(col=guide_legend(title="PrEP Coverage")) +
  ylab("HIV prevalence") +
  xlab("Time (years)") +
  ggtitle(" ")

ggsave(
  "out/plots/hiv_prev_2.jpeg",
  device = "jpeg", dpi = 600,
  height = 5, width = 6.5,
  units = "in"
)

ggplot(df, aes(x = year, y = incid_pyar, col = prep_lvl, label = prep_lvl)) +
  geom_smooth(size = 0.5, se = F) +
  geom_vline(xintercept = 4, col = "grey") +
  scale_color_brewer(palette = "Dark2") +
  scale_y_continuous(limits = c(0, 2.25), expand = c(0, 0)) +
  scale_x_continuous(breaks = seq(0, 15, 2), expand = c(0, 0)) +
  facet_grid(rows = vars(art_lvl)) +
  theme_bw() +
  theme(
    legend.position = "right",
    axis.text.x = element_text(margin = margin(5, 0, 10, 0, "pt")),
    axis.text.y = element_text(margin = margin(0, 5, 0, 10, "pt")),
    text = element_text(size = 10),
    aspect.ratio = 1/3
  ) +
  guides(col=guide_legend(title="PrEP Coverage")) +
  ylab("HIV incidence per 100 PYAR") +
  xlab("Time (years)") +
  ggtitle(" ")

ggsave(
  "out/plots/hiv_incid_2.jpeg",
  device = "jpeg", dpi = 600,
  height = 5, width = 7,
  units = "in"
)
