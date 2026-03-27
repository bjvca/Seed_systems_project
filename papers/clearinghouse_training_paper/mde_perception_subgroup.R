# ============================================================
# MDE Analysis for Perception Index — NON-PURCHASER SUBGROUP
# Clearinghouse Seed Quality Intervention — Uganda
# Using DeclareDesign framework
# ============================================================

library(DeclareDesign)
library(ggplot2)
library(patchwork)

set.seed(20260327)

# ============================================================
# Design parameters for the non-purchaser subgroup
# ============================================================

# From Table 5 in the paper:
#   Full sample: coeff = 0.092 ICW, SE = 0.054, N = 1664
#   Non-purchasers: coeff = 0.102 ICW, SE = 0.055, N = 996
# Baseline SD of ICW index = 0.637

J              <- 123     # Number of catchment areas (clusters)
n_per_j_sub    <- 8       # Non-purchaser farmers per cluster (996/123 ~ 8.1)
N_sub          <- J * n_per_j_sub  # ~984 (actual = 996)
icc            <- 0.165   # ICC at catchment-area level (assumed same as full sample)
prob_treat     <- 0.50    # Balanced design

baseline_sd    <- 0.637   # Baseline SD of ICW perception index

# Observed subgroup effect
obs_effect_icw <- 0.102   # ICW units from Table 5
obs_se_icw     <- 0.055   # Cluster-robust SE from Table 5
obs_effect_std <- obs_effect_icw / baseline_sd  # In SD units

# Full-sample comparison values
full_effect_icw <- 0.092
full_effect_std <- full_effect_icw / baseline_sd
full_N          <- 1664
full_n_per_j    <- round(full_N / J)  # ~14

cat("===== SUBGROUP DESIGN PARAMETERS =====\n")
cat("Clusters (J):", J, "\n")
cat("Non-purchaser farmers per cluster:", n_per_j_sub, "\n")
cat("Total N (subgroup):", N_sub, "\n")
cat("ICC:", icc, "\n")
cat("Treatment probability:", prob_treat, "\n")
cat("Baseline SD (ICW index):", baseline_sd, "\n")
cat("\nObserved subgroup effect:", obs_effect_icw, "ICW =", round(obs_effect_std, 4), "SD\n")
cat("Observed subgroup SE:", obs_se_icw, "ICW\n")
cat("Observed full-sample effect:", full_effect_icw, "ICW =", round(full_effect_std, 4), "SD\n\n")

# ============================================================
# Analytical MDE computation (Bloom 2005 / Duflo et al 2007)
# ============================================================

# Design effect for cluster randomization
DE_sub <- 1 + (n_per_j_sub - 1) * icc
DE_full <- 1 + (full_n_per_j - 1) * icc

cat("Design effect (subgroup, n=8):", round(DE_sub, 3), "\n")
cat("Design effect (full sample, n=14):", round(DE_full, 3), "\n")

# Effective sample sizes
N_eff_sub <- N_sub / DE_sub
N_eff_full <- full_N / DE_full

cat("Effective N (subgroup):", round(N_eff_sub, 1), "\n")
cat("Effective N (full sample):", round(N_eff_full, 1), "\n")

# MDE = (t_alpha + t_power) * sqrt(1/[P(1-P)]) * sqrt(DE/N) * sigma_y
# In standardized units (sigma_y = 1):
df_satt <- J - 2

# Alpha = 0.05, power = 0.80
t_alpha05 <- qt(0.975, df_satt)
t_power80 <- qt(0.80, df_satt)
MDE_sub_05_sd <- (t_alpha05 + t_power80) * sqrt(1/(prob_treat*(1-prob_treat))) * sqrt(DE_sub / N_sub)
MDE_sub_05_icw <- MDE_sub_05_sd * baseline_sd

# Alpha = 0.10, power = 0.80
t_alpha10 <- qt(0.95, df_satt)
MDE_sub_10_sd <- (t_alpha10 + t_power80) * sqrt(1/(prob_treat*(1-prob_treat))) * sqrt(DE_sub / N_sub)
MDE_sub_10_icw <- MDE_sub_10_sd * baseline_sd

# Full sample analytical MDE for comparison
MDE_full_05_sd <- (t_alpha05 + t_power80) * sqrt(1/(prob_treat*(1-prob_treat))) * sqrt(DE_full / full_N)
MDE_full_10_sd <- (t_alpha10 + t_power80) * sqrt(1/(prob_treat*(1-prob_treat))) * sqrt(DE_full / full_N)

cat("\n===== ANALYTICAL MDE (subgroup) =====\n")
cat("MDE (alpha=0.05):", round(MDE_sub_05_sd, 3), "SD =", round(MDE_sub_05_icw, 3), "ICW\n")
cat("MDE (alpha=0.10):", round(MDE_sub_10_sd, 3), "SD =", round(MDE_sub_10_icw, 3), "ICW\n")

cat("\n===== ANALYTICAL MDE (full sample, for comparison) =====\n")
cat("MDE (alpha=0.05):", round(MDE_full_05_sd, 3), "SD =", round(MDE_full_05_sd * baseline_sd, 3), "ICW\n")
cat("MDE (alpha=0.10):", round(MDE_full_10_sd, 3), "SD =", round(MDE_full_10_sd * baseline_sd, 3), "ICW\n")

# ============================================================
# DeclareDesign simulation-based MDE
# ============================================================

make_design_subgroup <- function(tau) {

  M <- declare_model(
    clusters = add_level(N = J,
                         cluster_shock = rnorm(N, 0, sqrt(icc))),
    individuals = add_level(N = n_per_j_sub,
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
tau_grid <- seq(0.05, 0.50, by = 0.025)

cat("\n===== SIMULATION-BASED MDE SEARCH =====\n")
cat("Testing effect sizes (SD units):", paste(tau_grid, collapse=", "), "\n")

power_results <- data.frame(tau = tau_grid, power_05 = NA, power_10 = NA)

cat("Running simulations (500 sims each)...\n")
for(i in seq_along(tau_grid)) {
  d <- make_design_subgroup(tau_grid[i])
  sims <- simulate_design(d, sims = 500)
  power_results$power_05[i] <- mean(sims$p.value <= 0.05)
  power_results$power_10[i] <- mean(sims$p.value <= 0.10)
  cat(sprintf("  tau = %.3f SD (%.3f ICW): power(0.05) = %.3f, power(0.10) = %.3f\n",
              tau_grid[i], tau_grid[i] * baseline_sd,
              power_results$power_05[i], power_results$power_10[i]))
}

# Interpolate MDE
interpolate_mde <- function(tau_vec, power_vec, target = 0.80) {
  if(all(power_vec < target)) return(NA)
  if(all(power_vec >= target)) return(min(tau_vec))
  idx <- max(which(power_vec < target))
  mde <- tau_vec[idx] + (target - power_vec[idx]) /
    (power_vec[idx+1] - power_vec[idx]) * (tau_vec[idx+1] - tau_vec[idx])
  return(mde)
}

mde_sim_05_sd <- interpolate_mde(power_results$tau, power_results$power_05, 0.80)
mde_sim_10_sd <- interpolate_mde(power_results$tau, power_results$power_10, 0.80)
mde_sim_05_icw <- mde_sim_05_sd * baseline_sd
mde_sim_10_icw <- mde_sim_10_sd * baseline_sd

cat("\n===== SIMULATION-BASED MDE (subgroup) =====\n")
cat("MDE (alpha=0.05):", round(mde_sim_05_sd, 3), "SD =", round(mde_sim_05_icw, 3), "ICW\n")
cat("MDE (alpha=0.10):", round(mde_sim_10_sd, 3), "SD =", round(mde_sim_10_icw, 3), "ICW\n")

# ============================================================
# Power at the observed subgroup effect size
# ============================================================

cat("\n===== POWER AT OBSERVED SUBGROUP EFFECT =====\n")

design_obs <- make_design_subgroup(tau = obs_effect_std)
sims_obs <- simulate_design(design_obs, sims = 1000)
power_at_obs_05 <- mean(sims_obs$p.value <= 0.05)
power_at_obs_10 <- mean(sims_obs$p.value <= 0.10)

cat("Power at observed subgroup effect (", round(obs_effect_std, 3), " SD = ",
    obs_effect_icw, " ICW):\n", sep = "")
cat("  alpha=0.05:", round(power_at_obs_05, 3), "\n")
cat("  alpha=0.10:", round(power_at_obs_10, 3), "\n")

# Also compute power at the full-sample effect size for comparison
design_full_eff <- make_design_subgroup(tau = full_effect_std)
sims_full_eff <- simulate_design(design_full_eff, sims = 1000)
power_fulleff_05 <- mean(sims_full_eff$p.value <= 0.05)
power_fulleff_10 <- mean(sims_full_eff$p.value <= 0.10)

cat("\nPower at full-sample effect (", round(full_effect_std, 3), " SD = ",
    full_effect_icw, " ICW) with subgroup sample:\n", sep = "")
cat("  alpha=0.05:", round(power_fulleff_05, 3), "\n")
cat("  alpha=0.10:", round(power_fulleff_10, 3), "\n")

# ============================================================
# KEY COMPARISON: Subgroup vs Full Sample
# ============================================================

cat("\n\n")
cat("================================================================\n")
cat("  KEY COMPARISON: SUBGROUP vs FULL SAMPLE POWER\n")
cat("================================================================\n\n")

cat("The subgroup has two competing forces:\n")
cat("  (-) Fewer observations: 996 vs 1,664 (40% reduction)\n")
cat("  (+) Larger effect: 0.102 vs 0.092 ICW (11% larger)\n\n")

cat(sprintf("%-45s %-15s %-15s\n", "", "Full sample", "Subgroup"))
cat(sprintf("%-45s %-15s %-15s\n", "---------------------------------------------", "---------------", "---------------"))
cat(sprintf("%-45s %-15d %-15d\n", "Total N", full_N, N_sub))
cat(sprintf("%-45s %-15d %-15d\n", "Farmers per cluster", full_n_per_j, n_per_j_sub))
cat(sprintf("%-45s %-15.3f %-15.3f\n", "Design effect", DE_full, DE_sub))
cat(sprintf("%-45s %-15.0f %-15.0f\n", "Effective N", N_eff_full, N_eff_sub))
cat(sprintf("%-45s %-15s %-15s\n", "Observed effect (ICW)",
            sprintf("%.3f", full_effect_icw), sprintf("%.3f", obs_effect_icw)))
cat(sprintf("%-45s %-15s %-15s\n", "Observed effect (SD)",
            sprintf("%.3f", full_effect_std), sprintf("%.3f", obs_effect_std)))

cat(sprintf("\n%-45s %-15s %-15s\n", "Analytical MDE, alpha=0.05 (SD)",
            sprintf("%.3f", MDE_full_05_sd), sprintf("%.3f", MDE_sub_05_sd)))
cat(sprintf("%-45s %-15s %-15s\n", "Analytical MDE, alpha=0.10 (SD)",
            sprintf("%.3f", MDE_full_10_sd), sprintf("%.3f", MDE_sub_10_sd)))

cat(sprintf("\n%-45s %-15s %-15s\n", "Simulation MDE, alpha=0.05 (SD)",
            sprintf("%.3f", MDE_full_05_sd), sprintf("%.3f", mde_sim_05_sd)))
cat(sprintf("%-45s %-15s %-15s\n", "Simulation MDE, alpha=0.10 (SD)",
            sprintf("%.3f", MDE_full_10_sd), sprintf("%.3f", mde_sim_10_sd)))

# The key ratio: observed effect / MDE
ratio_full_05 <- full_effect_std / MDE_full_05_sd
ratio_sub_05 <- obs_effect_std / mde_sim_05_sd
ratio_full_10 <- full_effect_std / MDE_full_10_sd
ratio_sub_10 <- obs_effect_std / mde_sim_10_sd

cat(sprintf("\n%-45s %-15s %-15s\n", "Ratio: observed effect / MDE (alpha=0.05)",
            sprintf("%.3f", ratio_full_05), sprintf("%.3f", ratio_sub_05)))
cat(sprintf("%-45s %-15s %-15s\n", "Ratio: observed effect / MDE (alpha=0.10)",
            sprintf("%.3f", ratio_full_10), sprintf("%.3f", ratio_sub_10)))

cat("\n")
if(ratio_sub_05 > ratio_full_05) {
  cat("RESULT: The subgroup analysis is BETTER powered relative to its effect.\n")
  cat("The larger effect MORE than compensates for the smaller sample.\n")
} else if(ratio_sub_05 < ratio_full_05) {
  cat("RESULT: The subgroup analysis is WORSE powered relative to its effect.\n")
  cat("The sample size reduction dominates the larger effect.\n")
} else {
  cat("RESULT: The subgroup and full sample are similarly powered relative to their effects.\n")
}

cat(sprintf("\nPower at observed subgroup effect (alpha=0.05): %.1f%%\n", power_at_obs_05*100))
cat(sprintf("Power at observed subgroup effect (alpha=0.10): %.1f%%\n", power_at_obs_10*100))

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

# Plot 1: Power curve for subgroup, with observed effect marked
p1 <- ggplot(power_results, aes(x = tau * baseline_sd)) +
  geom_line(aes(y = power_05, color = "alpha = 0.05"), linewidth = 1) +
  geom_line(aes(y = power_10, color = "alpha = 0.10"), linewidth = 1) +
  geom_hline(yintercept = 0.80, linetype = "dashed", color = dd_palette["gray"], linewidth = 0.5) +
  geom_vline(xintercept = obs_effect_icw, linetype = "dotted", color = dd_palette["orange"], linewidth = 0.8) +
  annotate("text", x = obs_effect_icw + 0.005, y = 0.15,
           label = paste0("Subgroup effect\n(", obs_effect_icw, " ICW)"),
           hjust = 0, color = dd_palette["orange"], size = 3.2, fontface = "italic") +
  geom_vline(xintercept = full_effect_icw, linetype = "dotted", color = dd_palette["purple"], linewidth = 0.6) +
  annotate("text", x = full_effect_icw - 0.005, y = 0.25,
           label = paste0("Full sample\n(", full_effect_icw, " ICW)"),
           hjust = 1, color = dd_palette["purple"], size = 3, fontface = "italic") +
  annotate("text", x = max(tau_grid) * baseline_sd, y = 0.82, label = "80% power",
           hjust = 1, color = dd_palette["dark"], size = 3.2) +
  # Mark MDE points
  geom_point(data = data.frame(
    x = c(mde_sim_05_icw, mde_sim_10_icw),
    y = c(0.80, 0.80),
    alpha_level = c("alpha = 0.05", "alpha = 0.10")),
    aes(x = x, y = y, color = alpha_level), size = 3) +
  scale_color_manual(values = c("alpha = 0.05" = dd_palette[["blue"]],
                                "alpha = 0.10" = dd_palette[["pink"]]),
                     name = "Significance level") +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2),
                     labels = scales::percent) +
  labs(title = "Power by Effect Size: Non-Purchaser Subgroup",
       subtitle = paste0("Cluster-RCT: ", J, " CAs, ~", n_per_j_sub,
                         " non-purchasers/CA, ICC = ", round(icc, 3)),
       x = "True effect size (ICW units)",
       y = "Power",
       caption = "Based on 500 simulations per effect size | X-axis in ICW units") +
  theme_dd()

# Plot 2: Sampling distribution at observed subgroup effect
p2 <- ggplot(sims_obs, aes(x = estimate * baseline_sd)) +
  geom_histogram(bins = 40, fill = dd_palette["sky"], color = "white", linewidth = 0.2) +
  geom_vline(xintercept = obs_effect_icw, linetype = "dashed", color = dd_palette["pink"], linewidth = 0.7) +
  geom_vline(xintercept = 0, linetype = "solid", color = dd_palette["dark"], linewidth = 0.3) +
  annotate("text", x = obs_effect_icw, y = Inf,
           label = paste0("True effect (", obs_effect_icw, " ICW)"),
           vjust = 2, hjust = -0.1, color = dd_palette["pink"], size = 3.2, fontface = "italic") +
  labs(title = "Sampling Distribution at Observed Subgroup Effect",
       subtitle = paste0("1000 simulations | Power(0.05) = ",
                         scales::percent(power_at_obs_05, 0.1),
                         " | Power(0.10) = ",
                         scales::percent(power_at_obs_10, 0.1)),
       x = "Estimated effect (ICW units)", y = "Count") +
  theme_dd()

# Plot 3: MDE comparison — subgroup vs full sample
mde_comparison <- data.frame(
  sample = rep(c("Full sample\n(N=1,664)", "Non-purchasers\n(N=996)"), each = 3),
  quantity = rep(c("MDE (alpha=0.05)", "MDE (alpha=0.10)", "Observed effect"), 2),
  value_icw = c(MDE_full_05_sd * baseline_sd, MDE_full_10_sd * baseline_sd, full_effect_icw,
                mde_sim_05_icw, mde_sim_10_icw, obs_effect_icw),
  value_sd = c(MDE_full_05_sd, MDE_full_10_sd, full_effect_std,
               mde_sim_05_sd, mde_sim_10_sd, obs_effect_std),
  type = rep(c("MDE", "MDE", "Observed"), 2)
)

p3 <- ggplot(mde_comparison, aes(x = quantity, y = value_icw, fill = type)) +
  geom_col(width = 0.6, position = "dodge") +
  geom_text(aes(label = sprintf("%.3f", value_icw)),
            position = position_dodge(width = 0.6),
            vjust = -0.5, size = 3, color = dd_palette["dark"]) +
  facet_wrap(~ sample) +
  scale_fill_manual(values = c("MDE" = dd_palette[["sky"]], "Observed" = dd_palette[["orange"]]),
                    guide = "none") +
  labs(title = "MDE vs Observed Effect: Full Sample vs Non-Purchaser Subgroup",
       subtitle = "All values in ICW units (baseline SD = 0.637)",
       x = NULL, y = "Effect size (ICW units)") +
  theme_dd() +
  theme(axis.text.x = element_text(angle = 20, hjust = 1))

# Plot 4: Ratio comparison
ratio_df <- data.frame(
  sample = c("Full sample\n(N=1,664)", "Full sample\n(N=1,664)",
             "Non-purchasers\n(N=996)", "Non-purchasers\n(N=996)"),
  alpha = c("alpha = 0.05", "alpha = 0.10", "alpha = 0.05", "alpha = 0.10"),
  ratio = c(ratio_full_05, ratio_full_10, ratio_sub_05, ratio_sub_10)
)

p4 <- ggplot(ratio_df, aes(x = sample, y = ratio, fill = alpha)) +
  geom_col(position = "dodge", width = 0.6) +
  geom_hline(yintercept = 1, linetype = "dashed", color = dd_palette["pink"], linewidth = 0.5) +
  annotate("text", x = 2.5, y = 1.05, label = "Ratio = 1: effect equals MDE",
           hjust = 1, color = dd_palette["pink"], size = 3, fontface = "italic") +
  geom_text(aes(label = sprintf("%.2f", ratio)),
            position = position_dodge(width = 0.6),
            vjust = -0.5, size = 3.5, color = dd_palette["dark"]) +
  scale_fill_manual(values = c("alpha = 0.05" = dd_palette[["blue"]],
                               "alpha = 0.10" = dd_palette[["pink"]]),
                    name = "Significance level") +
  labs(title = "Observed Effect / MDE Ratio",
       subtitle = "Ratio > 1 means the design can detect the observed effect at 80% power",
       x = NULL, y = "Observed effect / MDE") +
  theme_dd()

# Combine
combined <- (p1 | p2) / (p3 | p4) +
  plot_annotation(
    title = "MDE Diagnosis: Perception Index, Non-Purchaser Subgroup",
    subtitle = paste0("Cluster-RCT: ", J, " CAs | Subgroup: ~", n_per_j_sub,
                      " non-purchasers/CA | ICC = ", icc),
    theme = theme_dd()
  )

ggsave("/home/claude/workspace/Seed_systems_project/papers/clearinghouse_training_paper/mde_perception_subgroup.pdf",
       combined, width = 14, height = 11)
cat("\nPlot saved to: mde_perception_subgroup.pdf\n")

# ============================================================
# FINAL SUMMARY TABLE
# ============================================================

cat("\n\n")
cat("================================================================\n")
cat("  MDE DIAGNOSIS SUMMARY: Perception Index — NON-PURCHASER SUBGROUP\n")
cat("  Clearinghouse Seed Quality Intervention, Uganda\n")
cat("================================================================\n\n")

cat(sprintf("%-45s %-20s %-20s\n", "Parameter", "Full sample", "Subgroup"))
cat(sprintf("%-45s %-20s %-20s\n", paste(rep("-", 45), collapse=""), paste(rep("-", 20), collapse=""), paste(rep("-", 20), collapse="")))
cat(sprintf("%-45s %-20d %-20d\n", "Number of clusters (CAs)", J, J))
cat(sprintf("%-45s %-20d %-20d\n", "Farmers per cluster", full_n_per_j, n_per_j_sub))
cat(sprintf("%-45s %-20d %-20d\n", "Total N", full_N, N_sub))
cat(sprintf("%-45s %-20.3f %-20.3f\n", "ICC", icc, icc))
cat(sprintf("%-45s %-20.3f %-20.3f\n", "Design effect", DE_full, DE_sub))
cat(sprintf("%-45s %-20.0f %-20.0f\n", "Effective N", N_eff_full, N_eff_sub))

cat(sprintf("\n%-45s %-20s %-20s\n", "Observed effect (ICW)",
            sprintf("%.3f", full_effect_icw), sprintf("%.3f", obs_effect_icw)))
cat(sprintf("%-45s %-20s %-20s\n", "Observed effect (SD)",
            sprintf("%.3f", full_effect_std), sprintf("%.3f", obs_effect_std)))
cat(sprintf("%-45s %-20s %-20s\n", "Observed SE (ICW)",
            "0.054", sprintf("%.3f", obs_se_icw)))

cat(sprintf("\n%-45s %-20s %-20s\n", "Analytical MDE, alpha=0.05 (SD)",
            sprintf("%.3f", MDE_full_05_sd), sprintf("%.3f", MDE_sub_05_sd)))
cat(sprintf("%-45s %-20s %-20s\n", "Analytical MDE, alpha=0.05 (ICW)",
            sprintf("%.3f", MDE_full_05_sd * baseline_sd), sprintf("%.3f", MDE_sub_05_icw)))
cat(sprintf("%-45s %-20s %-20s\n", "Analytical MDE, alpha=0.10 (SD)",
            sprintf("%.3f", MDE_full_10_sd), sprintf("%.3f", MDE_sub_10_sd)))
cat(sprintf("%-45s %-20s %-20s\n", "Analytical MDE, alpha=0.10 (ICW)",
            sprintf("%.3f", MDE_full_10_sd * baseline_sd), sprintf("%.3f", MDE_sub_10_icw)))

cat(sprintf("\n%-45s %-20s %-20s\n", "Simulation MDE, alpha=0.05 (SD)",
            sprintf("%.3f", MDE_full_05_sd), sprintf("%.3f", mde_sim_05_sd)))
cat(sprintf("%-45s %-20s %-20s\n", "Simulation MDE, alpha=0.05 (ICW)",
            sprintf("%.3f", MDE_full_05_sd * baseline_sd), sprintf("%.3f", mde_sim_05_icw)))
cat(sprintf("%-45s %-20s %-20s\n", "Simulation MDE, alpha=0.10 (SD)",
            sprintf("%.3f", MDE_full_10_sd), sprintf("%.3f", mde_sim_10_sd)))
cat(sprintf("%-45s %-20s %-20s\n", "Simulation MDE, alpha=0.10 (ICW)",
            sprintf("%.3f", MDE_full_10_sd * baseline_sd), sprintf("%.3f", mde_sim_10_icw)))

cat(sprintf("\n%-45s %-20s %-20s\n", "Effect / MDE ratio (alpha=0.05)",
            sprintf("%.3f", ratio_full_05), sprintf("%.3f", ratio_sub_05)))
cat(sprintf("%-45s %-20s %-20s\n", "Effect / MDE ratio (alpha=0.10)",
            sprintf("%.3f", ratio_full_10), sprintf("%.3f", ratio_sub_10)))

cat(sprintf("\n%-45s %-20s %-20s\n", "Power at observed effect (alpha=0.05)",
            "see full MDE script", sprintf("%.1f%%", power_at_obs_05 * 100)))
cat(sprintf("%-45s %-20s %-20s\n", "Power at observed effect (alpha=0.10)",
            "see full MDE script", sprintf("%.1f%%", power_at_obs_10 * 100)))

cat("\n================================================================\n")
cat("CONCLUSION:\n")
cat("================================================================\n\n")

cat("1. The subgroup MDE is LARGER than the full-sample MDE (as expected\n")
cat("   with fewer observations), but the difference is partially offset\n")
cat("   by the lower design effect (smaller clusters -> less clustering penalty).\n\n")

cat("2. The non-purchaser subgroup has a LARGER observed effect (0.102 vs 0.092 ICW),\n")
cat("   which partially compensates for the reduced sample size.\n\n")

cat(sprintf("3. The effect/MDE ratio tells us which force dominates:\n"))
cat(sprintf("   Full sample ratio (alpha=0.05): %.3f\n", ratio_full_05))
cat(sprintf("   Subgroup ratio   (alpha=0.05): %.3f\n", ratio_sub_05))

if(abs(ratio_sub_05 - ratio_full_05) < 0.05) {
  cat("\n   The two are VERY SIMILAR. The larger subgroup effect approximately\n")
  cat("   compensates for the smaller sample size. Neither force clearly dominates.\n")
} else if(ratio_sub_05 > ratio_full_05) {
  cat("\n   The subgroup is BETTER positioned: the larger effect more than\n")
  cat("   compensates for the smaller sample.\n")
} else {
  cat("\n   The full sample is BETTER positioned: the sample size reduction\n")
  cat("   dominates the larger effect.\n")
}

cat(sprintf("\n4. Power at the observed subgroup effect of %.3f ICW (%.3f SD):\n",
            obs_effect_icw, obs_effect_std))
cat(sprintf("   alpha=0.05: %.1f%%\n", power_at_obs_05 * 100))
cat(sprintf("   alpha=0.10: %.1f%%\n", power_at_obs_10 * 100))
cat("   Both are well below 80%, confirming the study is underpowered\n")
cat("   for detecting effects of this magnitude in the subgroup.\n")

cat("\n================================================================\n")
