# =============================================================================
# OCEANS Study: Baseline Cognitive and Functional Profile Analysis
# =============================================================================

# -----------------------------------------------------------------------------
# 0. Setup: Load packages
# -----------------------------------------------------------------------------

hist(df$moca)
hist(df$log_rt_cond1)


## packages

if (!require("pacman")) install.packages("pacman")

pacman::p_load(
  readxl,
  tidyverse,
  ggpubr,
  rstatix,
  broom,
  tableone,
  knitr
)


## data

df_raw <- read_excel("OCEANS_functional_data_baseline_04_08_26__1_.xlsx")

glimpse(df_raw)
summary(df_raw)

df_raw %>% count(intgrp)


## clean

df <- df_raw %>%
  rename(
    id              = idno,
    group           = intgrp,
    moca            = moca_score,
    sppb            = sppb_total,
    gait_speed      = sppb_gait_speed,
    acc_cond1       = cacond1,       # Accuracy: condition 1
    rt_cond1        = crtcond1,      # Reaction time (ms): condition 1
    acc_cond2       = cacond2,       # Accuracy: condition 2
    rt_cond2        = crtcond2,      # Reaction time (ms): condition 2
    trial_time1     = cttime1,       # Trial time: condition 1
    trial_time2     = cttime2,       # Trial time: condition 2
    pct_correct1    = cpct1,         # % correct: condition 1
    pct_correct2    = cpct2,         # % correct: condition 2
    pct_correct_tot = cpct_total,    # % correct: total
    avl_trial0      = cavl_0,        # Verbal learning: trial 0
    avl_trial1      = cavl_1,        # Verbal learning: trial 1
    avl_trial2      = cavl_2,        # Verbal learning: trial 2
    avl_delayed     = cavl_ld        # Verbal learning: delayed recall
  ) %>%
  mutate(
    group = factor(group, levels = c("Control", "Intervention")),
    avl_slope = avl_trial2 - avl_trial0,
    log_rt_cond1 = log(rt_cond1),
    log_rt_cond2 = log(rt_cond2)
  )

df %>%
  summarise(across(everything(), ~sum(is.na(.)))) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "n_missing") %>%
  filter(n_missing > 0)


## baseline table

vars <- c("moca", "sppb", "gait_speed", "rt_cond1", "rt_cond2",
          "pct_correct_tot", "avl_trial0", "avl_trial2", "avl_delayed")

tab1 <- CreateTableOne(
  vars     = vars,
  strata   = "group",
  data     = df,
  addOverall = TRUE
)

print(tab1, showAllLevels = FALSE, formatOptions = list(big.mark = ","))


## group comparisons - non-parametric given small N

outcomes <- c("moca", "sppb", "gait_speed",
              "log_rt_cond1", "log_rt_cond2",
              "pct_correct_tot", "avl_trial0", "avl_trial2",
              "avl_delayed", "avl_slope")

group_tests <- map_dfr(outcomes, function(var) {
  formula <- as.formula(paste(var, "~ group"))
  test    <- wilcox_test(df, formula, exact = FALSE)
  effect  <- wilcox_effsize(df, formula)
  tibble(
    outcome    = var,
    W          = test$statistic,
    p_value    = round(test$p, 3),
    effect_r   = round(effect$effsize, 3),
    effect_mag = effect$magnitude
  )
})

print(group_tests)


## does MoCA predict verbal learning at baseline?

m1 <- lm(avl_delayed ~ moca, data = df)
m2 <- lm(avl_delayed ~ moca + group, data = df)
m3 <- lm(avl_delayed ~ moca * group, data = df)

anova(m1, m2, m3)

tidy(m2, conf.int = TRUE) %>%
  mutate(across(where(is.numeric), ~round(., 3)))

glance(m2)


## does gait speed predict reaction time?

m4 <- lm(log_rt_cond1 ~ gait_speed + group, data = df)

tidy(m4, conf.int = TRUE) %>%
  mutate(across(where(is.numeric), ~round(., 3)))

glance(m4)


## plots

# moca by group
p_moca <- ggplot(df, aes(x = group, y = moca, fill = group)) +
  geom_boxplot(alpha = 0.6, outlier.shape = NA) +
  geom_jitter(width = 0.1, size = 2, alpha = 0.7, aes(color = group)) +
  stat_compare_means(method = "wilcox.test", label = "p.format",
                     label.x = 1.4, label.y = max(df$moca, na.rm = TRUE) + 0.5) +
  scale_fill_manual(values  = c("Control" = "#4E9AF1", "Intervention" = "#F18A4E")) +
  scale_color_manual(values = c("Control" = "#4E9AF1", "Intervention" = "#F18A4E")) +
  labs(
    title    = "MoCA Scores at Baseline by Group",
    subtitle = "OCEANS Study (N = 23)",
    x        = NULL,
    y        = "MoCA Score (0–30)",
    caption  = "Wilcoxon rank-sum test; points are individual participants"
  ) +
  theme_pubr() +
  theme(legend.position = "none")

print(p_moca)
ggsave("fig1_moca_by_group.png", p_moca, width = 6, height = 5, dpi = 300)


# learning curves
avl_long <- df %>%
  select(id, group, avl_trial0, avl_trial1, avl_trial2, avl_delayed) %>%
  pivot_longer(
    cols      = c(avl_trial0, avl_trial1, avl_trial2, avl_delayed),
    names_to  = "trial",
    values_to = "words_recalled"
  ) %>%
  mutate(
    trial = recode(trial,
      avl_trial0   = "Trial 0",
      avl_trial1   = "Trial 1",
      avl_trial2   = "Trial 2",
      avl_delayed  = "Delayed"
    ),
    trial = factor(trial, levels = c("Trial 0", "Trial 1", "Trial 2", "Delayed"))
  )

p_avl <- avl_long %>%
  group_by(group, trial) %>%
  summarise(
    mean_words = mean(words_recalled, na.rm = TRUE),
    se         = sd(words_recalled, na.rm = TRUE) / sqrt(n()),
    .groups    = "drop"
  ) %>%
  ggplot(aes(x = trial, y = mean_words, group = group, color = group)) +
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = mean_words - se, ymax = mean_words + se), width = 0.15) +
  scale_color_manual(values = c("Control" = "#4E9AF1", "Intervention" = "#F18A4E")) +
  labs(
    title    = "Verbal Learning Curves at Baseline by Group",
    subtitle = "Mean words recalled (± SE) across trials",
    x        = "Trial",
    y        = "Words Recalled",
    color    = "Group",
    caption  = "OCEANS Study baseline assessment"
  ) +
  theme_pubr()

print(p_avl)
ggsave("fig2_verbal_learning_curves.png", p_avl, width = 7, height = 5, dpi = 300)


# moca vs delayed recall
p_scatter <- ggplot(df, aes(x = moca, y = avl_delayed, color = group)) +
  geom_point(size = 3, alpha = 0.8) +
  geom_smooth(method = "lm", se = TRUE, alpha = 0.15) +
  scale_color_manual(values = c("Control" = "#4E9AF1", "Intervention" = "#F18A4E")) +
  labs(
    title    = "Global Cognition and Delayed Verbal Recall at Baseline",
    subtitle = "Linear fit by group; shading = 95% CI",
    x        = "MoCA Score",
    y        = "Words Recalled (Delayed Trial)",
    color    = "Group",
    caption  = "OCEANS Study baseline assessment"
  ) +
  theme_pubr()

print(p_scatter)
ggsave("fig3_moca_vs_delayed_recall.png", p_scatter, width = 7, height = 5, dpi = 300)


# reaction time by condition
rt_long <- df %>%
  select(id, group, log_rt_cond1, log_rt_cond2) %>%
  pivot_longer(
    cols      = c(log_rt_cond1, log_rt_cond2),
    names_to  = "condition",
    values_to = "log_rt"
  ) %>%
  mutate(condition = recode(condition,
    log_rt_cond1 = "Condition 1",
    log_rt_cond2 = "Condition 2"
  ))

p_rt <- ggplot(rt_long, aes(x = condition, y = log_rt, fill = group)) +
  geom_boxplot(alpha = 0.6, position = position_dodge(0.8)) +
  geom_point(aes(color = group), position = position_dodge(0.8), size = 2, alpha = 0.7) +
  scale_fill_manual(values  = c("Control" = "#4E9AF1", "Intervention" = "#F18A4E")) +
  scale_color_manual(values = c("Control" = "#4E9AF1", "Intervention" = "#F18A4E")) +
  labs(
    title    = "Processing Speed (Reaction Time) by Condition and Group",
    subtitle = "Log-transformed reaction time; lower = faster",
    x        = "Task Condition",
    y        = "Log Reaction Time (ms)",
    fill     = "Group",
    color    = "Group",
    caption  = "OCEANS Study baseline assessment"
  ) +
  theme_pubr()

print(p_rt)
ggsave("fig4_reaction_time_by_group.png", p_rt, width = 7, height = 5, dpi = 300)


sessionInfo() # for reproducibility
