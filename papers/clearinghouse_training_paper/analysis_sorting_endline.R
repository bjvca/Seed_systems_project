## ============================================================================
## analysis_sorting_endline.R
##
## Test the dealer sorting channel using endline dyadic data.
##
## The sorting channel predicts that farmers who received clearinghouse ratings
## reallocate purchases toward better-rated dealers. We test this with three
## complementary approaches:
##
##   1. Primary: Rating-conditional retention (dyad-level)
##      Among farmer-dealer pairs where the farmer ever bought from the dealer,
##      does clearing × rating_j predict whether the farmer bought this season?
##
##   2. Supporting: Mean rating of active dealers (farmer-level)
##      Do clearing-arm farmers end up buying from higher-rated dealers on average?
##
##   3. Supporting: New customer relationships post-intervention (farmer-level)
##      Do clearing-arm farmers disproportionately start new dealer relationships
##      after 2020 (when the intervention began)?
##
## Identification assumption: Parallel trends in dealer choice absent clearinghouse.
## Key concern: Low rating dispersion (91%+ rated positively) mechanically limits
## the scope for sorting. β₃ close to zero may reflect ceiling effects, not
## absence of sorting.
##
## Standard errors: clustered at catchID (treatment assignment unit) using
## clubSandwich CR0, consistent with the main analysis scripts.
## ============================================================================

library(clubSandwich)   # clustered SEs (CR0, CR1, CR3)
library(lmtest)         # coeftest for display

## ---- 0. File paths ----------------------------------------------------------

base_dir <- "/home/claude/workspace/Seed_systems_project"

path_dyads    <- file.path(base_dir, "endline/data/farmer/public/endline_rating_dyads.csv")
path_treats   <- file.path(base_dir, "baseline/data/agro_input/public/treats_shop_level.csv")
path_rat_end  <- file.path(base_dir, "endline/data/agro_input/public/reviews_seed.csv")
path_rat_base <- file.path(base_dir, "baseline/data/agro_input/public/reviews_seed.csv")
path_rat_mid  <- file.path(base_dir, "midline/data/agro_input/public/reviews_seed.csv")
path_endline  <- file.path(base_dir, "endline/data/farmer/public/endline.csv")

## ---- 1. Load data -----------------------------------------------------------

dyads    <- read.csv(path_dyads,   stringsAsFactors = FALSE)
treats   <- read.csv(path_treats,  stringsAsFactors = FALSE)
rat_end  <- read.csv(path_rat_end, stringsAsFactors = FALSE)
rat_base <- read.csv(path_rat_base, stringsAsFactors = FALSE)
rat_mid  <- read.csv(path_rat_mid,  stringsAsFactors = FALSE)
endline  <- read.csv(path_endline,  stringsAsFactors = FALSE)

cat("=== Raw data dimensions ===\n")
cat(sprintf("Dyads: %d rows, %d farmers, %d shops\n",
            nrow(dyads), length(unique(dyads$farmer_ID)), length(unique(dyads$shop_ID))))
cat(sprintf("Endline reviews (shop-level): %d shops\n", nrow(rat_end)))
cat(sprintf("Baseline reviews (shop-level): %d shops\n", nrow(rat_base)))
cat(sprintf("Midline reviews (shop-level): %d shops\n", nrow(rat_mid)))
cat(sprintf("Treatment file: %d shops, %d catchments\n",
            nrow(treats), length(unique(treats$catchID))))

## ---- 2. Build catch-level treatment lookup ----------------------------------
## treatment is assigned at the catchment level; one row per catchment

catch_treat <- unique(treats[, c("catchID", "treat", "training", "clearing")])
# Verify one row per catchment
stopifnot(nrow(catch_treat) == length(unique(catch_treat$catchID)))

## Farmer-level treatment: merge catchID from endline survey, then add training
farmer_treat <- merge(
  endline[, c("farmer_ID", "catchID", "clearing")],
  catch_treat[, c("catchID", "training")],
  by = "catchID",
  all.x = TRUE
)
# clearing is already in endline; training comes from the treatment file
farmer_treat$clearing <- as.logical(farmer_treat$clearing)
farmer_treat$training <- as.logical(farmer_treat$training)

cat(sprintf("\nFarmer-level treatment coverage: %d / %d farmers\n",
            sum(!is.na(farmer_treat$training)), nrow(farmer_treat)))
cat("Clearing × Training cross-tab:\n")
print(table(clearing = farmer_treat$clearing, training = farmer_treat$training, useNA = "ifany"))

## ---- 3. Prepare ratings -----------------------------------------------------
## Use score_corrected (catch-mean corrected composite quality score).
## We keep endline and baseline ratings separately; endline ratings may be
## endogenous to the intervention (supply-side channel), so baseline ratings
## serve as a robustness check.

rat_end_shop  <- rat_end[,  c("shop_ID", "score_corrected")]
rat_base_shop <- rat_base[, c("shop_ID", "score_corrected")]
rat_mid_shop  <- rat_mid[,  c("shop_ID", "score_corrected")]

names(rat_end_shop)[2]  <- "score_end"
names(rat_base_shop)[2] <- "score_base"
names(rat_mid_shop)[2]  <- "score_mid"

cat(sprintf("\nRating (score_corrected) summary by wave:\n"))
cat(sprintf("  Endline  — mean: %.3f, sd: %.3f, N: %d\n",
            mean(rat_end_shop$score_end, na.rm=TRUE),
            sd(rat_end_shop$score_end, na.rm=TRUE),
            sum(!is.na(rat_end_shop$score_end))))
cat(sprintf("  Baseline — mean: %.3f, sd: %.3f, N: %d\n",
            mean(rat_base_shop$score_base, na.rm=TRUE),
            sd(rat_base_shop$score_base, na.rm=TRUE),
            sum(!is.na(rat_base_shop$score_base))))
cat(sprintf("  Midline  — mean: %.3f, sd: %.3f, N: %d\n",
            mean(rat_mid_shop$score_mid, na.rm=TRUE),
            sd(rat_mid_shop$score_mid, na.rm=TRUE),
            sum(!is.na(rat_mid_shop$score_mid))))

## ---- 4. Merge dyads with treatment and ratings ------------------------------

dyads2 <- merge(dyads, farmer_treat[, c("farmer_ID","catchID","clearing","training")],
                by = "farmer_ID", all.x = TRUE)
dyads2 <- merge(dyads2, rat_end_shop,  by = "shop_ID", all.x = TRUE)
dyads2 <- merge(dyads2, rat_base_shop, by = "shop_ID", all.x = TRUE)
dyads2 <- merge(dyads2, rat_mid_shop,  by = "shop_ID", all.x = TRUE)

cat(sprintf("\nAfter merge: %d dyad rows, %d farmers, %d shops\n",
            nrow(dyads2),
            length(unique(dyads2$farmer_ID)),
            length(unique(dyads2$shop_ID))))
cat(sprintf("Missing clearing (unmatched farmers): %d\n", sum(is.na(dyads2$clearing))))
cat(sprintf("Missing endline score (unmatched shops): %d\n", sum(is.na(dyads2$score_end))))

## ---- 5. Inspect and clean key outcome variables -----------------------------
## bought_at_dealer: "Yes"/"No"/"n/a"  — Q65: ever bought from this dealer
## bought_last_season: "Yes"/"No"/"n/a" — Q67: bought this season
## duration_customer: date string "YYYY-MM-DD" encoding year customer started

cat("\n=== Variable distributions ===\n")
cat("bought_at_dealer:\n")
print(table(dyads2$bought_at_dealer, useNA = "ifany"))
cat("\nbought_last_season:\n")
print(table(dyads2$bought_last_season, useNA = "ifany"))

## ---- 6. Construct outcomes --------------------------------------------------

## 6a. Retention indicator (dyad-level)
##     Restrict to pairs where farmer ever bought from dealer (Q65 = "Yes")
##     Outcome: retained = 1 if bought_last_season = "Yes", 0 if "No"
##     "n/a" for bought_last_season when bought_at_dealer = "Yes" is unexpected;
##     treat as missing and exclude.

dyads_ever <- dyads2[dyads2$bought_at_dealer == "Yes", ]
cat(sprintf("\nDyads where farmer ever bought from dealer: %d\n", nrow(dyads_ever)))

dyads_ever$retained <- NA
dyads_ever$retained[dyads_ever$bought_last_season == "Yes"] <- 1L
dyads_ever$retained[dyads_ever$bought_last_season == "No"]  <- 0L

cat("retained outcome distribution (among ever-bought dyads):\n")
print(table(dyads_ever$retained, useNA = "ifany"))

## 6b. Standardize ratings within the ever-bought sample
##     Standardizing within-sample ensures β on rating is in SD units,
##     making β₃ interpretable as a relative-to-mean shift.

dyads_ever$rating_end_std  <- scale(dyads_ever$score_end)[,1]
dyads_ever$rating_base_std <- scale(dyads_ever$score_base)[,1]

## Drop rows missing key variables for primary regression
dyads_primary_end  <- dyads_ever[!is.na(dyads_ever$retained) &
                                  !is.na(dyads_ever$clearing) &
                                  !is.na(dyads_ever$rating_end_std), ]
dyads_primary_base <- dyads_ever[!is.na(dyads_ever$retained) &
                                  !is.na(dyads_ever$clearing) &
                                  !is.na(dyads_ever$rating_base_std), ]

cat(sprintf("\nPrimary regression sample (endline rating): %d dyads, %d farmers, %d catchments\n",
            nrow(dyads_primary_end),
            length(unique(dyads_primary_end$farmer_ID)),
            length(unique(dyads_primary_end$catchID))))
cat(sprintf("Primary regression sample (baseline rating): %d dyads, %d farmers, %d catchments\n",
            nrow(dyads_primary_base),
            length(unique(dyads_primary_base$farmer_ID)),
            length(unique(dyads_primary_base$catchID))))

## Retention rates by arm
cat("\nRetention rate by clearing arm (endline rating sample):\n")
print(tapply(dyads_primary_end$retained, dyads_primary_end$clearing, mean, na.rm = TRUE))

## ---- 7. Helper: cluster-robust coeftest ------------------------------------
## CR0 (no small-sample correction) matches the main analysis convention;
## CR1 (Stata-style) reported as robustness.

clust_se <- function(model, cluster_var, type = "CR0") {
  vcov_cr <- vcovCR(model, cluster = cluster_var, type = type)
  coeftest(model, vcov = vcov_cr)
}

## ---- 8. Primary test: Rating-conditional retention (dyad-level) -------------
##
## Model: retained_ij = α + β₁·clearing_i + β₂·rating_j + β₃·clearing_i×rating_j + ε_ij
##
## β₃ > 0: clearing-arm farmers differentially retain high-rated dealers.
## This is the key test of the sorting channel.
##
## Clustering at catchID because treatment is assigned at catchment level.
## Secondary clustering at farmer_ID tests sensitivity to within-farmer
## correlation across their dealer dyads.

cat("\n\n============================================================\n")
cat("PRIMARY TEST: Rating-conditional dealer retention (dyad-level)\n")
cat("============================================================\n")

## 8a. Endline ratings
m1_end <- lm(retained ~ clearing * rating_end_std,
             data = dyads_primary_end)

cat("\n--- Endline ratings ---\n")
cat("OLS (no SEs):\n")
print(summary(m1_end)$coefficients)

cat("\nCluster-robust SEs (CR0, clustered at catchID):\n")
ct1_end_catch <- clust_se(m1_end, dyads_primary_end$catchID, type = "CR0")
print(ct1_end_catch)

cat("\nCluster-robust SEs (CR0, clustered at farmer_ID — robustness):\n")
ct1_end_farm <- clust_se(m1_end, dyads_primary_end$farmer_ID, type = "CR0")
print(ct1_end_farm)

cat(sprintf("\nN dyads: %d  |  N farmers: %d  |  N catchments: %d\n",
            nrow(dyads_primary_end),
            length(unique(dyads_primary_end$farmer_ID)),
            length(unique(dyads_primary_end$catchID))))

## 8b. Baseline ratings (endogeneity robustness)
## NOTE: Baseline seed ratings were collected ONLY in clearing-arm catchments
## (because only clearing-arm farmers rated dealers at baseline). This means
## clearing has no variation in dyads_primary_base — the clearing × rating
## interaction is not identified. We report the unconditional rating effect
## within the clearing arm as a partial check, and note the limitation.

cat("\n--- Baseline ratings (robustness check) ---\n")
cat("NOTE: Baseline ratings exist only for clearing-arm catchments.\n")
cat(sprintf("Clearing variation in baseline sample: %s\n",
            paste(sort(unique(dyads_primary_base$clearing)), collapse = ", ")))
cat("=> clearing × rating_base interaction is NOT identified (no control-arm baseline ratings).\n")
cat("Reporting main effect of baseline rating within clearing arm only.\n\n")

m1_base <- lm(retained ~ rating_base_std,
              data = dyads_primary_base)

cat("Cluster-robust SEs (CR0, clustered at catchID) — clearing arm only:\n")
ct1_base_catch <- clust_se(m1_base, dyads_primary_base$catchID, type = "CR0")
print(ct1_base_catch)

cat("\nCluster-robust SEs (CR0, clustered at farmer_ID):\n")
ct1_base_farm <- clust_se(m1_base, dyads_primary_base$farmer_ID, type = "CR0")
print(ct1_base_farm)

cat(sprintf("\nN dyads: %d  |  N farmers: %d  |  N catchments: %d\n",
            nrow(dyads_primary_base),
            length(unique(dyads_primary_base$farmer_ID)),
            length(unique(dyads_primary_base$catchID))))

## ---- 9. Supporting test: Mean rating of active dealers (farmer-level) -------
##
## For each farmer who bought from at least one dealer this season,
## compute the mean score_corrected of those dealers.
## If sorting occurred, clearing-arm farmers should buy from higher-rated dealers.

cat("\n\n==========================================================\n")
cat("SUPPORTING TEST 1: Mean rating of active dealers (farmer-level)\n")
cat("==========================================================\n")

## Active dealers: bought_last_season == "Yes" and endline rating available
active <- dyads2[!is.na(dyads2$bought_last_season) &
                   dyads2$bought_last_season == "Yes" &
                   !is.na(dyads2$score_end) &
                   !is.na(dyads2$clearing), ]

cat(sprintf("Dyads with active purchase + endline rating: %d\n", nrow(active)))
cat(sprintf("Farmers buying from at least one rated dealer: %d\n",
            length(unique(active$farmer_ID))))

## Farmer-level mean rating of active dealers
farmer_mean_rating <- aggregate(score_end ~ farmer_ID, data = active, FUN = mean)
names(farmer_mean_rating)[2] <- "mean_active_rating"

## Attach clearing and catchID
farmer_mean_rating <- merge(farmer_mean_rating,
                             unique(active[, c("farmer_ID","catchID","clearing","training")]),
                             by = "farmer_ID", all.x = TRUE)

cat(sprintf("\nFarmer-level sample: %d farmers, %d catchments\n",
            nrow(farmer_mean_rating),
            length(unique(farmer_mean_rating$catchID))))

cat("\nMean active-dealer rating by clearing arm:\n")
print(tapply(farmer_mean_rating$mean_active_rating, farmer_mean_rating$clearing, mean, na.rm = TRUE))

m2 <- lm(mean_active_rating ~ clearing, data = farmer_mean_rating)

cat("\nCluster-robust SEs (CR0, clustered at catchID):\n")
ct2 <- clust_se(m2, farmer_mean_rating$catchID, type = "CR0")
print(ct2)
cat(sprintf("N farmers: %d  |  N catchments: %d\n",
            nrow(farmer_mean_rating),
            length(unique(farmer_mean_rating$catchID))))

## Baseline rating robustness check for supporting test 1
## Same limitation applies: baseline ratings only in clearing arm.
active_base <- dyads2[!is.na(dyads2$bought_last_season) &
                        dyads2$bought_last_season == "Yes" &
                        !is.na(dyads2$score_base) &
                        !is.na(dyads2$clearing), ]

cat(sprintf("\nBaseline-rated active dyads: %d (clearing only: %s)\n",
            nrow(active_base),
            paste(sort(unique(active_base$clearing)), collapse=", ")))
cat("NOTE: No clearing variation in baseline-rated sample — cannot test sorting with baseline ratings here.\n")
cat("Reporting mean active-dealer rating within clearing arm for reference:\n")
cat(sprintf("  Mean baseline score_corrected (clearing arm active dealers): %.3f\n",
            mean(active_base$score_base, na.rm=TRUE)))
## Placeholder so summary table entry is informative
ct2b <- NULL

## ---- 10. Supporting test: New customer relationships post-intervention ------
##
## duration_customer is stored as a date string "YYYY-MM-DD" where the year
## records when the farmer became a customer. The intervention started in 2021.
## We define new_relationship = 1 if the farmer has at least one current-season
## dealer (bought_last_season = "Yes") where they became a customer in 2021 or later.
##
## This tests whether clearing-arm farmers form new dealer relationships,
## consistent with sorting toward better dealers not previously visited.

cat("\n\n==========================================================\n")
cat("SUPPORTING TEST 2: New customer relationships post-intervention\n")
cat("==========================================================\n")

## Inspect duration_customer format
cat("duration_customer format examples (non-n/a values):\n")
valid_dur <- dyads2$duration_customer[dyads2$duration_customer != "n/a" &
                                        !is.na(dyads2$duration_customer)]
print(head(sort(unique(valid_dur)), 20))

## Parse year from "YYYY-MM-DD"
dyads2$year_customer <- as.integer(substr(dyads2$duration_customer, 1, 4))
cat("\nYear-customer distribution (among ever-bought dyads):\n")
print(table(dyads2$year_customer[dyads2$bought_at_dealer == "Yes"], useNA = "ifany"))

## Flag: relationship started in 2021 or later (during or after intervention rollout)
dyads2$new_rel <- as.integer(!is.na(dyads2$year_customer) & dyads2$year_customer >= 2021)

## Restrict to active-season dealers (bought_last_season = "Yes")
active2 <- dyads2[!is.na(dyads2$bought_last_season) &
                    dyads2$bought_last_season == "Yes" &
                    !is.na(dyads2$clearing) &
                    !is.na(dyads2$year_customer), ]

cat(sprintf("\nActive-season dyads with valid duration: %d\n", nrow(active2)))
cat("new_rel (started >= 2021) among active-season dyads:\n")
print(table(active2$new_rel, useNA = "ifany"))

## Farmer-level: any new relationship among current-season dealers
farmer_newrel <- aggregate(new_rel ~ farmer_ID, data = active2, FUN = max)
names(farmer_newrel)[2] <- "any_new_rel"

farmer_newrel <- merge(farmer_newrel,
                        unique(active2[, c("farmer_ID","catchID","clearing","training")]),
                        by = "farmer_ID", all.x = TRUE)

cat(sprintf("\nFarmer-level sample: %d farmers\n", nrow(farmer_newrel)))
cat("any_new_rel rate by clearing arm:\n")
print(tapply(farmer_newrel$any_new_rel, farmer_newrel$clearing, mean, na.rm = TRUE))

m3 <- lm(any_new_rel ~ clearing, data = farmer_newrel)

cat("\nCluster-robust SEs (CR0, clustered at catchID):\n")
ct3 <- clust_se(m3, farmer_newrel$catchID, type = "CR0")
print(ct3)
cat(sprintf("N farmers: %d  |  N catchments: %d\n",
            nrow(farmer_newrel),
            length(unique(farmer_newrel$catchID))))

## Also: share of active-season dealers who are "new" (continuous version)
farmer_share_new <- aggregate(new_rel ~ farmer_ID, data = active2, FUN = mean)
names(farmer_share_new)[2] <- "share_new_rel"
farmer_share_new <- merge(farmer_share_new,
                           unique(active2[, c("farmer_ID","catchID","clearing")]),
                           by = "farmer_ID", all.x = TRUE)

m3b <- lm(share_new_rel ~ clearing, data = farmer_share_new)
cat("\nShare of active dealers that are new (continuous) — CR0 catchID SEs:\n")
ct3b <- clust_se(m3b, farmer_share_new$catchID, type = "CR0")
print(ct3b)

## ---- 11. Summary table of all coefficients ----------------------------------

cat("\n\n============================================================\n")
cat("SUMMARY TABLE: All sorting-channel tests\n")
cat("============================================================\n")

## Helper to extract a single row from coeftest output
extract_coef <- function(ct, coef_name, label) {
  if (coef_name %in% rownames(ct)) {
    row <- ct[coef_name, ]
    data.frame(
      Test      = label,
      Coef      = coef_name,
      Estimate  = round(row["Estimate"], 4),
      SE        = round(row["Std. Error"], 4),
      t_stat    = round(row["t value"], 3),
      p_value   = round(row["Pr(>|t|)"], 4),
      stringsAsFactors = FALSE
    )
  } else {
    data.frame(Test=label, Coef=coef_name, Estimate=NA, SE=NA, t_stat=NA, p_value=NA,
               stringsAsFactors=FALSE)
  }
}

summary_rows <- rbind(
  ## Primary test: endline ratings, catchID clustering
  extract_coef(ct1_end_catch, "clearingTRUE",                          "P1-End | clearing"),
  extract_coef(ct1_end_catch, "rating_end_std",                        "P1-End | rating (SD)"),
  extract_coef(ct1_end_catch, "clearingTRUE:rating_end_std",           "P1-End | clearing × rating [KEY]"),
  ## Primary test: baseline ratings (clearing arm only — no control-arm baseline ratings)
  extract_coef(ct1_base_catch,"rating_base_std",                       "P1-Base (clearing arm only) | rating (SD)"),
  ## Primary test: endline ratings, farmer_ID clustering (robustness)
  extract_coef(ct1_end_farm,  "clearingTRUE:rating_end_std",           "P1-End-FarmClust | clearing × rating"),
  ## Supporting test 1: mean rating of active dealers
  extract_coef(ct2,           "clearingTRUE",                          "S1-End | clearing → mean active rating"),
  ## S1-Base not identified (no clearing variation in baseline-rated sample)
  ## Supporting test 2: new relationships
  extract_coef(ct3,           "clearingTRUE",                          "S2 | clearing → any new dealer rel"),
  extract_coef(ct3b,          "clearingTRUE",                          "S2b | clearing → share new dealers")
)

rownames(summary_rows) <- NULL
print(summary_rows, digits = 4)

## ---- 12. Descriptive statistics table ---------------------------------------

cat("\n\n============================================================\n")
cat("DESCRIPTIVE STATISTICS\n")
cat("============================================================\n")

cat("\n--- Dyad-level (ever-bought sample) ---\n")
cat(sprintf("Total dyads (ever bought): %d\n", nrow(dyads_ever)))
cat(sprintf("Farmers represented:       %d\n", length(unique(dyads_ever$farmer_ID))))
cat(sprintf("Shops represented:         %d\n", length(unique(dyads_ever$shop_ID))))
cat(sprintf("Catchments:                %d\n", length(unique(dyads_ever$catchID))))

cat("\nRetention by arm and outcome:\n")
with(dyads_primary_end, {
  cat("  Clearing arm — retained:\n")
  print(table(retained[clearing == TRUE]))
  cat("  Control arm — retained:\n")
  print(table(retained[clearing == FALSE]))
})

cat("\nRetention rates:\n")
cat(sprintf("  Control (clearing=FALSE):  %.1f%%\n",
    100 * mean(dyads_primary_end$retained[dyads_primary_end$clearing == FALSE], na.rm=TRUE)))
cat(sprintf("  Clearing (clearing=TRUE):  %.1f%%\n",
    100 * mean(dyads_primary_end$retained[dyads_primary_end$clearing == TRUE],  na.rm=TRUE)))

cat("\n--- Rating dispersion (endline, all shops) ---\n")
cat(sprintf("Mean:   %.3f\n", mean(rat_end_shop$score_end, na.rm=TRUE)))
cat(sprintf("SD:     %.3f\n", sd(rat_end_shop$score_end, na.rm=TRUE)))
cat(sprintf("Min:    %.3f\n", min(rat_end_shop$score_end, na.rm=TRUE)))
cat(sprintf("Max:    %.3f\n", max(rat_end_shop$score_end, na.rm=TRUE)))
cat(sprintf("%% rated >= 4.0: %.1f%%\n",
    100 * mean(rat_end_shop$score_end >= 4.0, na.rm=TRUE)))
cat(sprintf("%% rated >= 3.5: %.1f%%\n",
    100 * mean(rat_end_shop$score_end >= 3.5, na.rm=TRUE)))

cat("\n--- New relationship analysis ---\n")
cat(sprintf("Active-season dyads with duration info: %d\n", nrow(active2)))
cat(sprintf("Pct started >= 2021: %.1f%%\n",
            100 * mean(active2$new_rel, na.rm=TRUE)))

cat("\n============================================================\n")
cat("Session info:\n")
print(sessionInfo())
