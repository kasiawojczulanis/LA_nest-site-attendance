###############################################################################
# This file is intended as a reproducible supplement accompanying the manuscript.
# 
# The eye that guards the rock: does it shape the world?
# Nest site attendance patterns in a colonially breeding pelagic seabird
#
# Authors:
#   Katarzyna Wojczulanis‑Jakubas, Dorian Cordier, Marion Devogel,
#   Antoine Grissot, Dariusz Jakubas, Dorota Kidawa, Martyna Syposz
#
# Description:
#   This script contains all analyses reported in the manuscript, including:
#     (1) Bayesian GAMM of sex‑specific nest site attendance across the 
#         breeding season
#     (2) Posterior predictions and sex‑difference curves across phenological time,
#     (3) Bayesian linear mixed‑effects model comparing breeders vs failed breeders,
#     (4) Posterior marginal means, pairwise contrasts, and distribution‑based
#         visualisations of the sex differences,
#     (5) Descriptive table of failed‑breeder occurrence by sex and stage,   
#     (5) Figures for publication and supplementary tables.
#
#   All analyses were conducted in R (version 4.5.2)      
###############################################################################

# Libraries -------------------------------------------------------------------
library(tidyverse)
library(readr)
library(readxl)

library(brms)        
library(mgcv)        
library(ggnewscale)
library(patchwork)
library(cowplot)
library(officer)
library(flextable)
library(emmeans)
library(tidybayes)
library(ggdist)


# Nest-site attendance patterns ------------------------------------------------

# Load data 

DA_breeders <- readRDS("./DA_patterns of breeders.rds")

# AR1 time index
DA_breeders <- DA_breeders %>%
  arrange(ringno, pheno_day) %>%
  group_by(ringno) %>%
  mutate(time_index = row_number()) %>%
  ungroup()

# bayesian GAMM formula, with AR1
bf_bayes <- bf(
  col_att_min_per24 ~ sx + s(pheno_day, by = sx) +
    (1 | ringno) + (1 | nest) + (1 | season),
  autocor = cor_ar(~ time_index | ringno, p = 1)
)

# the model
set.seed(123)
brm_model <- brm(
  bf_bayes,
  data = DA_breeders,
  family = gaussian(),
  chains = 4, cores = 4,
  iter = 4000, warmup = 1000,
  control = list(adapt_delta = 0.95)
)

summary(brm_model)

# plotting - patterns

# predictions 
new_data <- DA_breeders

pred_bayes <- fitted(
  brm_model,
  newdata = new_data,
  re_formula = NA,
  summary = TRUE
)

new_data$fit   <- pred_bayes[,"Estimate"]
new_data$lower <- pred_bayes[,"Q2.5"]
new_data$upper <- pred_bayes[,"Q97.5"]


# phase rectangles 
phase_df <- data.frame(
  xmin = c(min(DA_breeders$pheno_day), -28, 0),
  xmax = c(-28, 0, max(DA_breeders$pheno_day)),
  phase = factor(c("Mating", "Incubation", "Chick Rearing"),
                 levels = c("Mating", "Incubation", "Chick Rearing"))
)


# plot GAM - pattern
plot_gam <- ggplot(new_data, aes(x = pheno_day, y = fit, color = sx)) +
  geom_rect(
    data = phase_df,
    aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf, fill = phase),
    alpha = 0.3, inherit.aes = FALSE
  ) +
  scale_fill_manual(
    values = c("Mating" = "salmon", "Incubation" = "steelblue", "Chick Rearing" = "seashell4"),
    name = "Breeding Phase"
  ) +
  new_scale_fill() +
  geom_line(size = 1) +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = sx),
              alpha = 0.2, color = NA, show.legend = FALSE) +
  labs(
    x = "Phenological day",
    y = "Predicted nest site attendance (minutes per 24h)",
    color = "Sex"
  ) +
  theme_minimal()


# plotting - sex differences 
pheno_seq <- seq(min(DA_breeders$pheno_day),
                 max(DA_breeders$pheno_day),
                 length.out = 200)

sx_levels <- levels(DA_breeders$sx)

newdata_m <- data.frame(
  pheno_day = pheno_seq,
  sx        = factor("male", levels = sx_levels),
  nest = NA, season = NA, time_index = NA, ringno = NA
)
newdata_m$ringno <- "dummy"
newdata_m$time_index <- seq_len(nrow(newdata_m))


newdata_f <- data.frame(
  pheno_day = pheno_seq,
  sx        = factor("female", levels = sx_levels),
  nest = NA, season = NA, time_index = NA, ringno = NA
)
newdata_f$ringno <- "dummy"
newdata_f$time_index <- seq_len(nrow(newdata_f))


draws_m <- posterior_epred(brm_model, newdata = newdata_m, re_formula = NA)
draws_f <- posterior_epred(brm_model, newdata = newdata_f, re_formula = NA)

draws_diff <- draws_m - draws_f

diff_mean  <- apply(draws_diff, 2, mean)
diff_lower <- apply(draws_diff, 2, quantile, probs = 0.025)
diff_upper <- apply(draws_diff, 2, quantile, probs = 0.975)

signif <- diff_lower > 0 | diff_upper < 0

diff_df <- data.frame(
  pheno_day = pheno_seq,
  diff      = diff_mean,
  lower     = diff_lower,
  upper     = diff_upper,
  signif    = signif
)


# phase rectangles (sx)
phase_df <- data.frame(
  xmin = c(min(diff_df$pheno_day), -28, 0),
  xmax = c(-28, 0, max(diff_df$pheno_day)),
  phase = factor(c("Mating", "Incubation", "Chick Rearing"),
                 levels = c("Mating", "Incubation", "Chick Rearing"))
)


# plot -  sex difference 
plot_diff <- ggplot(diff_df, aes(x = pheno_day, y = diff)) +
  geom_rect(
    data = phase_df,
    aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf, fill = phase),
    alpha = 0.3, inherit.aes = FALSE
  ) +
  geom_line(color = "black", size = 1.2) +
  geom_ribbon(aes(ymin = lower, ymax = upper),
              fill = "grey", alpha = 0.4) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_point(
    data = subset(diff_df, signif),
    aes(x = pheno_day, y = diff),
    color = "red", size = 1.5
  ) +
  scale_fill_manual(
    values = c("Mating" = "salmon", "Incubation" = "steelblue", "Chick Rearing" = "seashell4"),
    breaks = c("Mating", "Incubation", "Chick Rearing"),
    name = "Breeding Phase"
  ) +
  labs(
    x = "Phenological day",
    y = "Male – Female (Predicted nest site attendance - minutes per 24 h)"
  ) +
  theme_minimal() +
  theme(legend.position = "none")


# combine plots 
legend <- get_legend(plot_gam)
plot_gam_nolegend <- plot_gam + theme(legend.position = "none")

combined_plot <- plot_grid(
  plot_gam_nolegend,
  plot_diff,
  legend,
  nrow = 1,
  rel_widths = c(1, 1, 0.3)
)

# ggsave("Fig_Q1_bayesian.jpg", combined_plot, width = 12, height = 5, dpi = 300)



# Sample size for breeding stages (suppl table) -----------------------------
table_samplesize <- DA_breeders %>% mutate(session = if_else(pheno_day < -28, "mating",
                                                             if_else(pheno_day > 0, "chick rearing", "incubation"))) %>% 
  group_by(season, session, nest) %>% 
  summarise(n = n()) %>% 
  group_by(season, session) %>% 
  summarise(n = n()) 


# sort content
table_samplesize$session <- factor(table_samplesize$session, levels = c("mating", "incubation", "chick rearing"))
table_samplesize <- table_samplesize %>% 
  arrange(session, season)

# save the table 
doc <- read_docx()
ft <- flextable(table_samplesize)
ft <- autofit(ft)      # Adjust column widths
doc <- body_add_flextable(doc, value = ft)
print(doc, target = "Table_S1.docx")


# Breeders vs failed ------------------------------------------------------------

# load data
df <- readRDS("./DA_breeders_non_breeders_24.rds")

# ensure factors
df <- df %>%
  mutate(
    status  = factor(status),
    sx      = factor(sx),
    session = factor(session),
    nest    = factor(nest)
  )

# model formula
bf_lmm <- bf(
  on_screen_min24 ~ status + sx + session + (1 | nest)
)

# model
set.seed(123)
brm_lmm <- brm(
  bf_lmm,
  data = df,
  family = gaussian(),
  chains = 4, cores = 4,
  iter = 4000, warmup = 1000,
  control = list(adapt_delta = 0.95)
)

summary(brm_lmm)
bayes_R2(brm_lmm)

# posterior marginal means
emm <- emmeans(brm_lmm, ~ status + sx + session)

# combination matrix (status × sex × session)
emm_grid <- as.data.frame(emm) %>%
  select(status, sx, session)

# posterior draws 
draws_mat <- posterior_epred(brm_lmm, newdata = emm_grid, re_formula = NA)

draws_df <- as.data.frame(draws_mat)
draws_df$.draw <- seq_len(nrow(draws_df))

draws_long <- draws_df %>%
  pivot_longer(
    cols = - .draw,
    names_to = "row",
    values_to = ".epred"
  )

# add row index to the emmeans matrix
emm_grid <- emm_grid %>%
  mutate(row = paste0("V", row_number()))

# merge: posterior draws + groups info
emm_draws <- draws_long %>%
  left_join(emm_grid, by = "row")

# sample sizes (for plot)
sample_size_emm_df <- df %>%
  group_by(status, sx, ringno, session) %>%
  summarise(n = n()) %>%
  group_by(status, sx, session) %>%
  summarise(n = n())

emm_draws <- emm_draws %>%
  left_join(sample_size_emm_df,
            by = c("status", "sx", "session"))

# # Pairwise contrasts
# pairwise_results <- as.data.frame(pairs(emm, adjust = "tukey"))
# 
# # export table
# ft <- flextable(pairwise_results)
# ft <- font(ft, fontname = "Times New Roman", part = "all")
# ft <- fontsize(ft, size = 12, part = "all")
# ft <- autofit(ft)
# 
# doc <- read_docx()
# doc <- body_add_flextable(doc, value = ft)
# doc <- body_end_section_landscape(doc)
# print(doc, target = "Table_S2.docx")
# 
# 
# Plotting - posterior means

label_df <- sample_size_emm_df %>%
  mutate(
    x_pos = max(emm_draws$.epred) + 5
  ) %>% 
  filter(!is.na(sx))

comp_plot <- emm_draws %>%
  ggplot(aes(x = .epred, y = session, fill = sx)) +
  
  stat_halfeye(
    alpha = 0.6,
    adjust = 0.7,
    width = 0.6,
    .width = 0.95,
    justification = -0.2,
    point_interval = median_qi
  ) +
  
  geom_text(
    data = label_df,
    aes(
      x = x_pos,
      y = session,
      label = paste0("n=", n),
      color = sx
    ),
    inherit.aes = FALSE,
    hjust = 0,
    size = 3,
    position = position_dodge(width = 0.6)
  ) +

  facet_grid(status ~ ., labeller = label_both) +
  
  labs(
    x = "Posterior distribution of nest site attendance (min / 24h)",
    y = "Breeding stage",
    fill = "Sex"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    strip.text = element_text(face = "bold"),
    legend.position = "right",
    plot.margin = margin(5.5, 40, 5.5, 5.5)
  ) +
  
  expand_limits(x = max(label_df$x_pos) + 5) +
  guides(color = "none")

# comp_plot

# plotting - posterior sex differences

# compute differences (male – female)
diff_draws <- emm_draws %>%
  select(.draw, .epred, status, sx, session) %>%
  pivot_wider(
    names_from = sx,
    values_from = .epred
  ) %>%
  mutate(
    diff = male - female
  )

# plot - sex diff
diff_plot <- diff_draws %>%
  ggplot(aes(x = diff, y = session)) +
  
  stat_halfeye(
    adjust = 0.7,
    width = 0.6,
    .width = c(0.5, 0.8, 0.95),
    justification = -0.2,
    point_interval = median_qi
  ) +
  
  geom_vline(xintercept = 0, linetype = "dashed") +
  
  facet_grid(status ~ ., labeller = label_both) +
  
  labs(
    x = "Posterior difference (male – female)\nNest site attendance (min / 24h)",
    y = "Breeding stage"
  ) +
  
  theme_minimal(base_size = 13) +
  theme(
    strip.text = element_text(face = "bold"),
    legend.position = "none"
  )

# diff_plot

# Combine side-by-side
combined_plot <- comp_plot + diff_plot +
  plot_layout(ncol = 2, widths = c(1, 1))

combined_plot

ggsave("Fig_Q2_24_bayesian.jpg", combined_plot, dpi = 300)


# Failed breeders presence --------------------------------------------------------

df <- readRDS("./DA_breeders_non_breeders_46.rds") # 46 hours (to check presence)
# df <- read_excel("./DA_breeders_non_breeders_46.xlsx") # 46 hours (to check presence)

df %>% 
  group_by(session, status, sx, ringno) %>% summarise(n = n()) %>% 
  group_by(session, status, sx) %>% summarise(n = n()) %>% 
  filter(status == "failed breeder")
