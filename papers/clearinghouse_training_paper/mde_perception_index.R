# ============================================================
# MDE Analysis for Perception Index
# Clearinghouse Seed Quality Intervention - Uganda
# Using DeclareDesign framework
# ============================================================

library(DeclareDesign)
library(ggplot2)
library(patchwork)

set.seed(20260327)

# ============================================================
# Design parameters extracted from actual data
# ============================================================

params <- readRDS("mde_params.rds")

J         <- 123       # Number of catchment areas (clusters)
n_per_j   <- 14        # Mean farmers per cluster (round from 13.5)
icc       <- 0.165     # ICC at catchment-area level
prob_treat <- 0.50     # Balanced 50/50 (paper: 65/65 CAs)
# Note: in the perception sample it's 57% treated due to differential 
# non-response, but we use 50/50 for the MDE since that's the design

obs_effect_std <- 0.156  # Observed effect in SD units
obs_effect_icw <- 0.09   # Observed effect as reported in paper

cat("===== DESIGN PARAMETERS =====\n")
cat("Clusters (J):", J, "\n")
cat("Farmers per cluster:", n_per_j, "\n")
cat("Total N:", J * n_per_j, "\n")
cat("ICC:", icc, "\n")
cat("Treatment probability:", prob_treat, "\n")
cat("Observed effect (SD units):", obs_effect_std, "\n")
cat("Observed effect (paper units):", obs_effect_icw, "\n\n")

# ============================================================
# Analytical MDE computation (Bloom 2005 / Duflo et al 2007)
# ============================================================

# Design effect for cluster randomization
# DE = 1 + (n_bar - 1) * ICC
DE <- 1 + (n_per_j - 1) * icc
cat("Design effect:", round(DE, 3), "\n")

# Effective sample size
N_total <- J * n_per_j
N_eff <- N_total / DE

cat("Effective sample size:", round(N_eff, 1), "\n")

# MDE = M_J * sqrt(1/P(1-P)) * sqrt(DE/N) * sigma_y
# where M_J is the multiplier for J clusters
# For alpha=0.05: M_J = t_{J-2, 0.975} + t_{J-2, 0.20} (for 80% power)
# For alpha=0.10: M_J = t_{J-2, 0.95} + t_{J-2, 0.20}

# Using t-distribution with J-2 degrees of freedom
df_satt <- J - 2  # Conservative; actual Satterthwaite df may be lower

# Alpha = 0.05, power = 0.80
t_alpha05 <- qt(0.975, df_satt)
t_power80 <- qt(0.80, df_satt)
MDE_05 <- (t_alpha05 + t_power80) * sqrt(1/(prob_treat*(1-prob_treat))) * sqrt(DE / N_total) * 1

# Alpha = 0.10, power = 0.80
t_alpha10 <- qt(0.95, df_satt)
MDE_10 <- (t_alpha10 + t_power80) * sqrt(1/(prob_treat*(1-prob_treat))) * sqrt(DE / N_total) * 1

cat("\n===== ANALYTICAL MDE (standardized units, sigma_y = 1) =====\n")
cat("MDE at alpha=0.05, power=0.80:", round(MDE_05, 3), "SD\n")
cat("MDE at alpha=0.10, power=0.80:", round(MDE_10, 3), "SD\n")

# Convert to paper (ICW) units  
scaling <- obs_effect_icw / obs_effect_std
cat("\nMDE at alpha=0.05, power=0.80 (paper units):", round(MDE_05 * scaling, 3), "\n")
cat("MDE at alpha=0.10, power=0.80 (paper units):", round(MDE_10 * scaling, 3), "\n")

cat("\nObserved effect:", round(obs_effect_std, 3), "SD =", round(obs_effect_icw,3), "(paper)\n")
cat("Ratio observed/MDE_05:", round(obs_effect_std / MDE_05, 2), "\n")
cat("Ratio observed/MDE_10:", round(obs_effect_std / MDE_10, 2), "\n")

# ============================================================
# DeclareDesign simulation-based MDE
# ============================================================

# Function to declare design for a given effect size
make_design <- function(tau) {
  
  M <- declare_model(
    clusters = add_level(N = J, 
                         cluster_shock = rnorm(N, 0, sqrt(icc))),
    individuals = add_level(N = n_per_j,
                            U = rnorm(N, 0, sqrt(1 - icc)),
                            potential_outcomes(Y ~ tau * Z + cluster_shock + U))
  )
  
  I <- declare_inquiry(ATE = mean(Y_Z_1 - Y_Z_0))
  
  D_assign <- declare_assignment(Z = cluster_ra(clusters = clusters, prob = prob_treat))
  
  D_measure <- declare_measurement(Y = reveal_outcomes(Y ~ Z))
  
  A <- declare_estimator(Y ~ Z, clusters = clusters, 
                         se_type = "CR0",
                         .method = estimatr::lm_robust,
                         inquiry = "ATE", 
                         label = "DIM_CR0")
  
  design <- M + I + D_assign + D_measure + A
  return(design)
}

# Search over effect sizes to find MDE
# MDE = smallest tau where power >= 0.80

tau_grid <- seq(0.05, 0.45, by = 0.025)

cat("\n===== SIMULATION-BASED MDE SEARCH =====\n")
cat("Testing effect sizes:", paste(tau_grid, collapse=", "), "\n")

designs <- lapply(tau_grid, make_design)

# Diagnose all designs
cat("Running simulations (500 sims each)...\n")

power_results <- data.frame(tau = tau_grid, power_05 = NA, power_10 = NA)

for(i in seq_along(tau_grid)) {
  d <- designs[[i]]
  sims <- simulate_design(d, sims = 500)
  power_results$power_05[i] <- mean(sims$p.value <= 0.05)
  power_results$power_10[i] <- mean(sims$p.value <= 0.10)
  cat(sprintf("  tau = %.3f: power(0.05) = %.3f, power(0.10) = %.3f\n", 
              tau_grid[i], power_results$power_05[i], power_results$power_10[i]))
}

# Interpolate MDE
# Find where power crosses 0.80

interpolate_mde <- function(tau_vec, power_vec, target = 0.80) {
  if(all(power_vec < target)) return(NA)
  if(all(power_vec >= target)) return(min(tau_vec))
  idx <- max(which(power_vec < target))
  # Linear interpolation
  mde <- tau_vec[idx] + (target - power_vec[idx]) / (power_vec[idx+1] - power_vec[idx]) * (tau_vec[idx+1] - tau_vec[idx])
  return(mde)
}

mde_sim_05 <- interpolate_mde(power_results$tau, power_results$power_05, 0.80)
mde_sim_10 <- interpolate_mde(power_results$tau, power_results$power_10, 0.80)

cat("\n===== SIMULATION-BASED MDE =====\n")
cat("MDE at alpha=0.05, power=0.80:", round(mde_sim_05, 3), "SD\n")
cat("MDE at alpha=0.10, power=0.80:", round(mde_sim_10, 3), "SD\n")
cat("MDE at alpha=0.05, power=0.80 (paper units):", round(mde_sim_05 * scaling, 3), "\n")
cat("MDE at alpha=0.10, power=0.80 (paper units):", round(mde_sim_10 * scaling, 3), "\n")

# ============================================================
# Power at the observed effect size
# ============================================================

cat("\n===== POWER AT OBSERVED EFFECT =====\n")

design_obs <- make_design(tau = obs_effect_std)
sims_obs <- simulate_design(design_obs, sims = 1000)
power_at_obs_05 <- mean(sims_obs$p.value <= 0.05)
power_at_obs_10 <- mean(sims_obs$p.value <= 0.10)

cat("Power at observed effect (0.156 SD) with alpha=0.05:", round(power_at_obs_05, 3), "\n")
cat("Power at observed effect (0.156 SD) with alpha=0.10:", round(power_at_obs_10, 3), "\n")

# ============================================================
# VISUALIZATIONS
# ============================================================

dd_palette <- c(
  blue     = "#3564ED",
  pink     = "#C6227F",
  sky      = "#72B4F3",
  gray     = "#B8B8B8",
  dark     = "#2C2C2C",
  orange   = "#E67E22",
  green    = "#27AE60",
  purple   = "#8E44AD"
)

theme_dd <- function(base_size = 12) {
  theme_minimal(base_size = base_size) +
    theme(
      text = element_text(color = "#2C2C2C"),
      plot.title = element_text(face = "bold", size = base_size + 2, hjust = 0),
      plot.subtitle = element_text(color = "#666666", size = base_size, hjust = 0,
                                   margin = margin(b = 10)),
      plot.caption = element_text(color = "#999999", size = base_size - 2, hjust = 1),
      panel.grid.major = element_line(color = "#ECECEC", linewidth = 0.3),
      panel.grid.minor = element_blank(),
      axis.title = element_text(size = base_size, face = "plain"),
      axis.text = element_text(size = base_size - 1, color = "#555555"),
      strip.text = element_text(face = "bold", size = base_size),
      legend.position = "bottom",
      legend.title = element_text(face = "bold", size = base_size - 1),
      legend.text = element_text(size = base_size - 1),
      plot.margin = margin(15, 15, 15, 15)
    )
}

# Plot 1: Power curve by effect size
p1 <- ggplot(power_results, aes(x = tau)) +
  geom_line(aes(y = power_05, color = "alpha = 0.05"), linewidth = 1) +
  geom_line(aes(y = power_10, color = "alpha = 0.10"), linewidth = 1) +
  geom_hline(yintercept = 0.80, linetype = "dashed", color = dd_palette["gray"], linewidth = 0.5) +
  geom_vline(xintercept = obs_effect_std, linetype = "dotted", color = dd_palette["orange"], linewidth = 0.8) +
  annotate("text", x = obs_effect_std + 0.01, y = 0.15, 
           label = paste0("Observed effect\n(", round(obs_effect_std,3), " SD)"),
           hjust = 0, color = dd_palette["orange"], size = 3.2, fontface = "italic") +
  annotate("text", x = max(tau_grid), y = 0.82, label = "80% power",
           hjust = 1, color = dd_palette["dark"], size = 3.2) +
  # Mark MDE points
  geom_point(data = data.frame(x = c(mde_sim_05, mde_sim_10), y = c(0.80, 0.80),
                                alpha_level = c("alpha = 0.05", "alpha = 0.10")),
             aes(x = x, y = y, color = alpha_level), size = 3) +
  scale_color_manual(values = c("alpha = 0.05" = dd_palette[["blue"]], 
                                 "alpha = 0.10" = dd_palette[["pink"]]),
                     name = "Significance level") +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2),
                     labels = scales::percent) +
  scale_x_continuous(breaks = seq(0.05, 0.45, 0.05)) +
  labs(title = "Power by True Effect Size",
       subtitle = paste0("Cluster-RCT: ", J, " catchment areas, ~", n_per_j, 
                        " farmers/CA, ICC = ", round(icc,3)),
       x = "True effect size (standardized, SD units)", 
       y = "Power",
       caption = "Based on 500 simulations per effect size") +
  theme_dd()

# Plot 2: Sampling distribution at observed effect
sims_obs_df <- sims_obs
p2 <- ggplot(sims_obs_df, aes(x = estimate)) +
  geom_histogram(bins = 40, fill = dd_palette["sky"], color = "white", linewidth = 0.2) +
  geom_vline(xintercept = obs_effect_std, linetype = "dashed", color = dd_palette["pink"], linewidth = 0.7) +
  geom_vline(xintercept = 0, linetype = "solid", color = dd_palette["dark"], linewidth = 0.3) +
  annotate("text", x = obs_effect_std, y = Inf, label = "True effect (0.156 SD)", 
           vjust = 2, hjust = -0.1, color = dd_palette["pink"], size = 3.2, fontface = "italic") +
  labs(title = "Sampling Distribution at Observed Effect Size",
       subtitle = paste0("1000 simulations | Power(0.05) = ", 
                        scales::percent(power_at_obs_05, 0.1),
                        " | Power(0.10) = ",
                        scales::percent(power_at_obs_10, 0.1)),
       x = "Estimated effect (SD units)", y = "Count") +
  theme_dd()

# Plot 3: MDE comparison bar chart
mde_comparison <- data.frame(
  quantity = c("MDE (alpha=0.05)", "MDE (alpha=0.10)", "Observed effect"),
  value_sd = c(mde_sim_05, mde_sim_10, obs_effect_std),
  value_paper = c(mde_sim_05 * scaling, mde_sim_10 * scaling, obs_effect_icw),
  type = c("MDE", "MDE", "Observed")
)

p3 <- ggplot(mde_comparison, aes(x = reorder(quantity, value_sd), y = value_sd, fill = type)) +
  geom_col(width = 0.6) +
  geom_text(aes(label = paste0(round(value_sd, 3), " SD\n(", round(value_paper, 3), " ICW)")),
            hjust = -0.1, size = 3.2, color = dd_palette["dark"]) +
  scale_fill_manual(values = c("MDE" = dd_palette[["sky"]], "Observed" = dd_palette[["orange"]]),
                    guide = "none") +
  coord_flip(ylim = c(0, max(mde_comparison$value_sd) * 1.4)) +
  labs(title = "MDE vs. Observed Effect",
       subtitle = "Perception index (seed quality ratings)",
       x = NULL, y = "Effect size (SD units)") +
  theme_dd()

# Combine
combined <- (p1) / (p2 | p3) +
  plot_annotation(
    title = "MDE Diagnosis: Perception Index in Clearinghouse RCT",
    subtitle = paste0("Design: ", J, " catchment areas, ~", J*n_per_j, " farmers, ICC = ", round(icc,3)),
    theme = theme_dd()
  )

ggsave("mde_perception_diagnosis.pdf", combined, width = 12, height = 10)
cat("\nPlot saved to: mde_perception_diagnosis.pdf\n")

# ============================================================
# SUMMARY TABLE
# ============================================================

cat("\n\n")
cat("================================================================\n")
cat("  MDE DIAGNOSIS SUMMARY: Perception Index\n")
cat("  Clearinghouse Seed Quality Intervention\n")
cat("================================================================\n\n")

cat(sprintf("%-40s %s\n", "Design parameter", "Value"))
cat(sprintf("%-40s %s\n", "----------------------------------------", "-------------------"))
cat(sprintf("%-40s %d\n", "Number of clusters (catchment areas)", J))
cat(sprintf("%-40s %d\n", "Mean farmers per cluster", n_per_j))
cat(sprintf("%-40s %d\n", "Total sample (perception analysis)", N_total))
cat(sprintf("%-40s %.3f\n", "ICC (catchment area level)", icc))
cat(sprintf("%-40s %.2f\n", "Design effect", DE))
cat(sprintf("%-40s %.0f\n", "Effective sample size", N_eff))
cat(sprintf("%-40s %.1f%%\n", "Treatment probability", prob_treat*100))

cat("\n")
cat(sprintf("%-40s %s\n", "MDE Results", ""))
cat(sprintf("%-40s %s\n", "----------------------------------------", "-------------------"))
cat(sprintf("%-40s %.3f SD (%.3f ICW)\n", "MDE (alpha=0.05, power=0.80)", mde_sim_05, mde_sim_05*scaling))
cat(sprintf("%-40s %.3f SD (%.3f ICW)\n", "MDE (alpha=0.10, power=0.80)", mde_sim_10, mde_sim_10*scaling))

cat("\n")
cat(sprintf("%-40s %s\n", "Observed vs. MDE", ""))
cat(sprintf("%-40s %s\n", "----------------------------------------", "-------------------"))
cat(sprintf("%-40s %.3f SD (%.3f ICW)\n", "Observed effect", obs_effect_std, obs_effect_icw))
cat(sprintf("%-40s %.3f\n", "Observed SE (cluster-robust)", params$obs_se))
cat(sprintf("%-40s %.3f\n", "Observed p-value (Satterthwaite)", params$obs_pval))
cat(sprintf("%-40s %.1f%%\n", "Power at observed effect (alpha=0.05)", power_at_obs_05*100))
cat(sprintf("%-40s %.1f%%\n", "Power at observed effect (alpha=0.10)", power_at_obs_10*100))

cat("\n")
cat(sprintf("%-40s %s\n", "CONCLUSION", ""))
cat(sprintf("%-40s %s\n", "----------------------------------------", "-------------------"))

if(obs_effect_std < mde_sim_10) {
  cat("The observed effect of 0.09 ICW units (0.156 SD) is BELOW\n")
  cat("the MDE even at alpha=0.10. The study is underpowered for\n")
  cat("detecting effects of this magnitude on the perception index.\n")
} else if(obs_effect_std < mde_sim_05) {
  cat("The observed effect of 0.09 ICW units (0.156 SD) is BELOW\n")
  cat("the MDE at alpha=0.05 but within range at alpha=0.10.\n")
  cat("The 10%-level significance is consistent with borderline power.\n")
} else {
  cat("The observed effect is above the MDE. The design has adequate\n")
  cat("power for this effect size.\n")
}

cat("\n================================================================\n")
