# Forest Plots ------------------------------------------------------------
library(tidyverse)
library(gt)
library(patchwork)

## Unadjusted Results ---------------------------
# load data
res_unadj <- data.frame(
  outcome = c("Total Length of Hospital Stay (>7 days)", "ICU Stay (>4 days)", "GFR Reduction (>=50%)", 
              "Post-op Instestinal Ischemia (yes)", "Post-op Cerebrovascular Stroke (yes)",
              "Post-op Spinal Ischemia (yes)", "Post-op Dialysis (yes)", "Post-op Length of Hospital Stay (>7 days)",
              "Post-op Complications (yes)", "Post-op Leg Ischemia/Emboli (yes)",
              "Post-op Respiratory (yes)", "Re-intervention (yes)", "Post Treatment of All Branches (yes)"),
  estimate = c(5.14, 3.11, 1.57, 1.13, 3.42, 2.56, 2.42, 3.39, 
               1.74, 1.97, 2.65, 1.86, 2.28),
  conf.low = c(4.11, 2.43, 1.08, 0.56, 1.83, 1.70, 1.44, 2.69,
               1.37, 1.13, 1.87, 1.33, 1.78),
  conf.high = c(6.43, 3.97, 2.29, 2.26, 6.38, 3.86, 4.07, 4.29,
                2.21, 3.46, 3.75, 2.62, 2.91),
  p.value = c("<0.001", "<0.001", 0.02, 0.74, "<0.001", "<0.001", "<0.001", "<0.001",
              "<0.001", 0.02, "<0.001", "<0.001", "<0.001")
)

# create forest plot on log scale (middle section of figure)
p_mid <- 
  res_unadj %>%
  ggplot(aes(y = fct_rev(outcome))) +
  theme_classic() +
  geom_point(aes(x=estimate), shape=15, size=3) +
  geom_linerange(aes(xmin=conf.low, xmax=conf.high)) +
  labs(x="Unadjusted OR", y="") +
  geom_vline(xintercept = 1, linetype="dashed") +
  coord_cartesian(ylim=c(1,14), xlim=c(0, 7)) +
  theme(axis.line.y = element_blank(),
        axis.ticks.y= element_blank(),
        axis.text.y= element_blank(),
        axis.title.y= element_blank())
# wrangle results into pre-plotting table form
res_plot <- res_unadj %>%
  mutate(across(c(estimate, conf.low, conf.high), ~str_pad(round(.x, 2), width=4, pad="0", side="right")),
         estimate_lab = paste0(estimate, " (", conf.low, "-", conf.high,")")) %>%
  bind_rows(data.frame(outcome = "Outcome", estimate_lab = "Unadjusted OR \n (95% CI)", 
                       conf.low = "", conf.high="",p.value="p-value")) %>%
  mutate(outcome = fct_rev(fct_relevel(outcome, "Outcome")))
# left side of plot - hazard ratios
p_left <-
  res_plot  %>%
  ggplot(aes(y = outcome)) + 
  geom_text(aes(x=0, label=outcome), hjust=0, fontface = "bold") +
  geom_text(aes(x=1, label=estimate_lab), hjust=0, 
            fontface = ifelse(res_plot$estimate_lab == 
                                "Unadjusted OR \n (95% CI)", "bold", "plain")) +
  theme_void() +
  coord_cartesian(xlim=c(0,2))
# right side of plot - pvalues
p_right <-
  res_plot  %>%
  ggplot() +
  geom_text(aes(x=0, y=outcome, label=p.value), hjust=0, 
            fontface = ifelse(res_plot$p.value == "p-value", "bold", "plain")) +
  theme_void() 
# layout design (top, left, bottom, right)
layout <- c(
  area(t = 0, l = 0, b = 30, r = 11),
  area(t = 1, l = 9, b = 30, r = 16),
  area(t = 0, l = 16, b = 30, r = 17)
)
# final plot arrangement
p_left + p_mid + p_right + plot_layout(design = layout) +
  plot_annotation(
    caption = "Exposure = Symptomatic (Ref: Asymptomatic), OR = Odds Ratio, CI = Confidence Interval"
  )
## save final figure
ggsave("forest-plot-unadj.png", width=11, height=5)

## Adjusted Results ---------------------------
# load data
res_adj <- data.frame(
  outcome = c("Total Length of Hospital Stay (>7 days)", "ICU Stay (>4 days)", "GFR Reduction (>=50%)", 
              "Post-op Instestinal Ischemia (yes)", "Post-op Cerebrovascular Stroke (yes)",
              "Post-op Spinal Ischemia (yes)", "Post-op Dialysis (yes)", "Post-op Length of Hospital Stay (>7 days)",
              "Post-op Complications (yes)", "Post-op Leg Ischemia/Emboli (yes)",
              "Post-op Respiratory (yes)", "Re-intervention (yes)", "Post Treatment of All Branches (yes)"),
  estimate = c(3.79, 2.08, 1.35, 1.07, 1.81, 1.61, 1.78, 2.46, 1.44, 1.74,
               2.17, 1.48, 1.64),
  conf.low = c(2.99, 1.59, 0.90, 0.5, 0.91, 1.00, 1.01, 1.91, 1.12, 0.94,
               1.47, 1.04, 1.24),
  conf.high = c(4.81, 2.71, 2.02, 2.3, 3.6, 2.6, 3.13, 3.17, 1.86, 3.22,
                3.21, 2.09, 2.16),
  p.value = c("<0.001", "<0.001", 0.15, 0.87, 0.09, 0.05, 0.047, "<0.001", 0.004, 0.08,
              "<0.001", 0.03, "<0.001")
)

# create forest plot on log scale (middle section of figure),
p_mid <- 
  res_adj %>%
  ggplot(aes(y = fct_rev(outcome))) +
  theme_classic() +
  geom_point(aes(x=estimate), shape=15, size=3) +
  geom_linerange(aes(xmin=conf.low, xmax=conf.high)) +
  labs(x="Adjusted OR", y="") +
  geom_vline(xintercept = 1, linetype="dashed") +
  coord_cartesian(ylim=c(1,14), xlim=c(0, 7)) +
  theme(axis.line.y = element_blank(),
        axis.ticks.y= element_blank(),
        axis.text.y= element_blank(),
        axis.title.y= element_blank())
# wrangle results into pre-plotting table form
res_plot <- res_adj %>%
  mutate(estimate_lab = 
           paste0(estimate, " (", sprintf("%.2f", conf.low), "-", sprintf("%.2f", conf.high),")")) %>%
  bind_rows(data.frame(outcome = "Outcome", estimate_lab = "Adjusted OR \n (95% CI)", 
                       p.value="p-value")) %>%
  mutate(outcome = fct_rev(fct_relevel(outcome, "Outcome")))
# left side of plot - hazard ratios
p_left <-
  res_plot  %>%
  ggplot(aes(y = outcome)) + 
  geom_text(aes(x=0, label=outcome), hjust=0, fontface = "bold") +
  geom_text(aes(x=1, label=estimate_lab), hjust=0, 
            fontface = ifelse(res_plot$estimate_lab == 
                                "Adjusted OR \n (95% CI)", "bold", "plain")) +
  theme_void() +
  coord_cartesian(xlim=c(0,2))
# right side of plot - pvalues
p_right <-
  res_plot  %>%
  ggplot() +
  geom_text(aes(x=0, y=outcome, label=p.value), hjust=0, 
            fontface = ifelse(res_plot$p.value == "p-value", "bold", "plain")) +
  theme_void() 
# layout design (top, left, bottom, right)
layout <- c(
  area(t = 0, l = 0, b = 30, r = 11),
  area(t = 1, l = 9, b = 30, r = 16),
  area(t = 0, l = 16, b = 30, r = 17)
)
# final plot arrangement
p_left + p_mid + p_right + plot_layout(design = layout) +
  plot_annotation(
    caption = "Exposure = Symptomatic (Ref: Asymptomatic), OR = Odds Ratio, CI = Confidence Interval"
  )
## save final figure
ggsave("forest-plot-adj.png", width=11, height=5)