rm(list = ls())

# Libs
library(tidyverse)
library(readr)

library(ggnewscale)
library(patchwork)
library(cowplot)

library(mgcv)
library(lmerTest)
library(performance)
library(emmeans)



# Question 1 -----

# Male and female breeders - nest site attendance during the entire breeding season (GAMM)

# Load data
DA_Q1_breeders <- readRDS("./DA_Q1_breeders.rds")

# GAMM fit ----
# (with cor matrix - corAR1) 
gam_model_a1 <- gamm(
  col_att_min_per24 ~ sx + s(pheno_day, by = sx),
  random = list(ringno = ~1, nest = ~1, season = ~1),
  correlation = corAR1(),
  data = DA_Q1_breeders,
  method = "REML",
  select = TRUE
)

# Model ouput
summary(gam_model_a1$gam)
gam.check(gam_model_a1$gam)


# Plotting ----

# prediction data
new_data <- DA_Q1_breeders
new_data$fit <- predict(gam_model_a1$gam, newdata = new_data, se.fit = TRUE)$fit
new_data$se <- predict(gam_model_a1$gam, newdata = new_data, se.fit = TRUE)$se.fit

# confidence intervals
new_data$upper <- new_data$fit + 1.96 * new_data$se
new_data$lower <- new_data$fit - 1.96 * new_data$se

# phases
phase_df <- data.frame(
  xmin = c(min(DA_Q1_breeders$pheno_day), -28, 0),
  xmax = c(-28, 0, max(DA_Q1_breeders$pheno_day)),
  phase = factor(c("Mating", "Incubation", "Chick Rearing"),
                 levels = c("Mating", "Incubation", "Chick Rearing"))
)

# plot
plot_gam <- ggplot(new_data, aes(x = pheno_day, y = fit, color = sx)) +
  
  # Phase rectangles
  geom_rect(data = phase_df, aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf, fill = phase),
            alpha = 0.3, inherit.aes = FALSE) +
  scale_fill_manual(
    values = c("Mating" = "salmon", "Incubation" = "steelblue", "Chick Rearing" = "seashell4"),
    name = "Breeding Phase"
  ) +
  
  # Reset fill scale for ribbon
  new_scale_fill() +
  
  # Main plot layers
  geom_line(size = 1) +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = sx), alpha = 0.2, color = NA, show.legend = FALSE) +
  
  labs(x = "Phenological Day", y = "Predicted nest site attendance (minutes per 24h)", color = "Sex") +
  theme_minimal()



# Sex differences ----

pheno_seq <- seq(min(DA_Q1_breeders$pheno_day), max(DA_Q1_breeders$pheno_day), length.out = 200)


# Match factor levels
sx_levels <- levels(DA_Q1_breeders$sx)

newdata_m <- data.frame(
  pheno_day = pheno_seq,
  sx = factor(rep("male", length(pheno_seq)), levels = sx_levels),
  nest = NA, season = NA, time_index = NA
)

newdata_f <- data.frame(
  pheno_day = pheno_seq,
  sx = factor(rep("female", length(pheno_seq)), levels = sx_levels),
  nest = NA, season = NA, time_index = NA
)



# Predict with Standard Errors
gam_part <- gam_model_a1$gam

pred_m <- predict(gam_part, newdata_m, type = "response", se.fit = TRUE)
pred_f <- predict(gam_part, newdata_f, type = "response", se.fit = TRUE)



# Calculate differnce and conf intervals
diff <- pred_m$fit - pred_f$fit
se_diff <- sqrt(pred_m$se.fit^2 + pred_f$se.fit^2)

upper <- diff + 1.96 * se_diff
lower <- diff - 1.96 * se_diff

signif <- lower > 0 | upper < 0  # TRUE where CI excludes zero

diff_df <- data.frame(
  pheno_day = pheno_seq,
  diff = diff,
  upper = upper,
  lower = lower,
  signif = signif
)


# Plot

phase_df <- data.frame(
  xmin = c(min(diff_df$pheno_day), -28, 0),
  xmax = c(-28, 0, max(diff_df$pheno_day)),
  phase = factor(c("Mating", "Incubation", "Chick Rearing"),
                 levels = c("Mating", "Incubation", "Chick Rearing"))
)

plot_diff <- ggplot(diff_df, aes(x = pheno_day, y = diff)) +
  # Phase rectangles
  geom_rect(data = phase_df, aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf, fill = phase),
            alpha = 0.3, inherit.aes = FALSE) +
  
  # Main plot layers
  geom_line(color = "black", size = 1.2) +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "grey", alpha = 0.4) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_point(data = subset(diff_df, signif), aes(x = pheno_day, y = diff), color = "red", size = 1.5) +
  
  # Custom legend and labels
  scale_fill_manual(
    values = c("Mating" = "salmon", "Incubation" = "steelblue", "Chick Rearing" = "seashell4"),
    breaks = c("Mating", "Incubation", "Chick Rearing"),
    name = "Breeding Phase"
  ) +
  labs(
    x = "Phenological day",
    y = "Male – Female (Predicted nest site attendance - minutes per 24 h)"
  ) +
  theme_minimal()  +
  theme(legend.position = "none")


# Combine plots side by side

# extract the legend from plot_gam
legend <- get_legend(plot_gam)

# remove legend from plot_gam
plot_gam_nolegend <- plot_gam + theme(legend.position = "none")

# arrange plots: plot_gam (left), plot_diff (middle), legend (right)
combined_plot <- plot_grid(
  plot_gam_nolegend, 
  plot_diff, 
  legend,
  nrow = 1,
  rel_widths = c(1, 1, 0.3)  # adjust widths as needed
)

# ggsave("Fig_Q1.jpg", combined_plot, width = 12, height = 5, dpi = 300)


# Question 2 -----

# Comparison of breeders and failed breeders (regarding sex and chick rearing stage)
df <- readRDS("./DA_Q2_breeders_non_breeders_24.rds") # 24 hours (main text)
# df <- readRDS("./DA_Q2_breeders_non_breeders_46.rds") # 46 hours (suppl)

# REML fit ----
# model <- lmer(on_screen_min24 ~ status + sx + session + (1 | nest/ringNo), data = df)
# model_simple <- lmer(on_screen_min24 ~ status + sx + session + (1 | nest), data = df)

# model <- lmer(on_screen_min46 ~ status + sx + session + (1 | nest/ringNo), data = df)
model_simple <- lmer(on_screen_min46 ~ status + sx + session + (1 | nest), data = df)

summary(model_simple)
r2_nakagawa(model_simple)

# post-hocs and plotting


#  emmeans for all combinations
emm <- emmeans(model_simple, ~ status + sx + session)
pairwise_results <-as.data.frame(pairs(emm, adjust = "tukey"))  # Tukey's HSD correction


# supplementary table 
# write_csv(pairwise_results, "Table_SQ2_pairwise_comparisons_24.csv")
# write_csv(pairwise_results, "Table_SQ2_pairwise_comparisons_46.csv")


# plotting
emm_df <- as.data.frame(emm)

comp_plot <- ggplot(emm_df, aes(x = session, y = emmean, color = sx, shape = status)) +
  geom_point(position = position_dodge(width = 0.5), size = 3) +
  geom_errorbar(aes(ymin = emmean - SE, ymax = emmean + SE),
                position = position_dodge(width = 0.5), width = 0.2) +
  labs(
    x = "Breeding stage",
    # y = "Estimated mean of nest site attendance (minutes per 24h)",
    y = "Estimated mean of nest site attendance (minutes per 46h)",
    color = "Sex",
    shape = "Breeding status"
  ) +
  theme_minimal()


# ggsave("Fig_Q2_24.jpg", comp_plot, dpi = 300)
# ggsave("Fig_Q2_46.jpg", comp_plot, dpi = 300)


# Question 3 -----

non_breeders_presence <- readRDS("C:/Users/DELL/Dropbox/Working files/Projects/William/non_breeders_presence.rds")

stats::fisher.test(table(non_breeders_presence$presence, non_breeders_presence$sx))

# proportions
props <- failed46_presence %>%
  group_by(session, sx) %>%
  summarise(sum = sum(presence), .groups = "drop")


summary_data <- non_breeders_presence %>%
  group_by(nest, session, sx) %>%
  summarise(presence_rate = mean(presence), .groups = "drop")

pres_plot <- ggplot(summary_data, aes(x = session, y = presence_rate, fill = sx)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~nest) +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(
    title = "Presence Rate by Session, Sex, and Nest",
    x = "Session",
    y = "Presence Rate",
    fill = "Sex"
  ) +
  theme_classic() +
  theme(
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
  )

ggsave("Fig_Q3.jpg", pres_plot, dpi = 300)
