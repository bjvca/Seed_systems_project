rm(list = ls())

# Exploratory mechanism check:
# Are farmers in clearing-house treatment areas more likely to switch agro-dealers
# when ratings reveal more dispersion across dealers in the catchment?

path <- getwd()
path <- strsplit(path, "/papers/clearinghouse_training_paper")[[1]]

read_clean <- function(file) {
  dat <- read.csv(paste(path, file, sep = "/"), stringsAsFactors = FALSE)
  dat[dat == "n/a"] <- NA
  dat
}

make_dispersion <- function(file, wave, one_dealer_zero = FALSE) {
  ratings <- read_clean(file)
  ratings$score_corrected <- as.numeric(ratings$score_corrected)
  ratings <- ratings[!is.na(ratings$catchID) & !is.na(ratings$score_corrected), ]

  agg <- aggregate(
    score_corrected ~ catchID,
    data = ratings,
    FUN = function(x) c(
      n = length(x),
      mean = mean(x),
      sd = if (length(x) > 1) sd(x) else if (one_dealer_zero) 0 else NA,
      range = max(x) - min(x),
      iqr = IQR(x)
    )
  )

  out <- data.frame(catchID = agg$catchID, agg$score_corrected)
  names(out) <- c(
    "catchID",
    paste0(wave, "_n"),
    paste0(wave, "_mean"),
    paste0(wave, "_sd"),
    paste0(wave, "_range"),
    paste0(wave, "_iqr")
  )
  out
}

make_switching <- function(file, period) {
  dat <- read_clean(file)
  dat$clearing <- as.logical(dat$clearing)
  dat <- dat[dat$clearing %in% TRUE, c("farmer_ID", "catchID", "check.maize.q25i")]

  dat$switched <- NA_integer_
  dat$switched[dat$check.maize.q25i == "1"] <- 1L
  dat$switched[dat$check.maize.q25i == "2"] <- 0L
  dat <- dat[!is.na(dat$switched), ]
  dat$period <- period
  dat
}

cluster_vcov <- function(model, cluster) {
  x <- model.matrix(model)
  u <- residuals(model)
  cl <- as.factor(cluster)
  k <- ncol(x)
  n <- nrow(x)
  g <- nlevels(cl)
  meat <- matrix(0, k, k)

  for (level in levels(cl)) {
    idx <- which(cl == level)
    xu <- crossprod(x[idx, , drop = FALSE], u[idx])
    meat <- meat + xu %*% t(xu)
  }

  bread <- solve(crossprod(x))
  small_sample <- (g / (g - 1)) * ((n - 1) / (n - k))
  small_sample * bread %*% meat %*% bread
}

cluster_result <- function(model, var, cluster) {
  vc <- cluster_vcov(model, cluster)
  se <- sqrt(diag(vc))[var]
  est <- coef(model)[var]
  pval <- 2 * pt(abs(est / se), df = length(unique(cluster)) - 1, lower.tail = FALSE)
  c(coef = unname(est), se = unname(se), p = unname(pval))
}

report_split <- function(dat, xvar, nvar, label, cutoff = c("median", "top_quartile")) {
  cutoff <- match.arg(cutoff)
  dat <- dat[!is.na(dat[[xvar]]) & !is.na(dat$switched), ]
  catchment_values <- unique(dat[, c("catchID", xvar)])

  threshold <- if (cutoff == "median") {
    median(catchment_values[[xvar]], na.rm = TRUE)
  } else {
    quantile(catchment_values[[xvar]], 0.75, na.rm = TRUE)
  }

  dat$high_dispersion <- as.integer(dat[[xvar]] > threshold)
  dat$n_dealers_z <- as.numeric(scale(dat[[nvar]]))

  model <- lm(switched ~ high_dispersion, data = dat)
  model_n <- lm(switched ~ high_dispersion + n_dealers_z, data = dat)

  res <- cluster_result(model, "high_dispersion", dat$catchID)
  res_n <- cluster_result(model_n, "high_dispersion", dat$catchID)

  data.frame(
    label = label,
    cutoff = cutoff,
    threshold = as.numeric(threshold),
    farmers = nrow(dat),
    catchments = length(unique(dat$catchID)),
    low_farmers = sum(dat$high_dispersion == 0),
    high_farmers = sum(dat$high_dispersion == 1),
    low_catchments = length(unique(dat$catchID[dat$high_dispersion == 0])),
    high_catchments = length(unique(dat$catchID[dat$high_dispersion == 1])),
    low_switch_rate = mean(dat$switched[dat$high_dispersion == 0]),
    high_switch_rate = mean(dat$switched[dat$high_dispersion == 1]),
    difference = res["coef"],
    clustered_se = res["se"],
    p_value = res["p"],
    difference_control_n = res_n["coef"],
    clustered_se_control_n = res_n["se"],
    p_value_control_n = res_n["p"]
  )
}

run_analysis <- function(one_dealer_zero = FALSE) {
  baseline_dispersion <- make_dispersion(
    "/baseline/data/agro_input/public/reviews_seed.csv",
    "baseline",
    one_dealer_zero = one_dealer_zero
  )
  midline_dispersion <- make_dispersion(
    "/midline/data/agro_input/public/reviews_seed.csv",
    "midline",
    one_dealer_zero = one_dealer_zero
  )

  baseline_to_midline <- merge(
    make_switching("/midline/data/farmer/public/midline.csv", "baseline_to_midline"),
    baseline_dispersion,
    by = "catchID",
    all.x = TRUE
  )
  midline_to_endline <- merge(
    make_switching("/endline/data/farmer/public/endline.csv", "midline_to_endline"),
    midline_dispersion,
    by = "catchID",
    all.x = TRUE
  )

  out <- rbind(
    report_split(
      baseline_to_midline,
      "baseline_sd",
      "baseline_n",
      "Baseline rating SD -> baseline-to-midline switching",
      cutoff = "median"
    ),
    report_split(
      midline_to_endline,
      "midline_sd",
      "midline_n",
      "Midline rating SD -> midline-to-endline switching",
      cutoff = "median"
    ),
    report_split(
      baseline_to_midline,
      "baseline_sd",
      "baseline_n",
      "Baseline rating SD -> baseline-to-midline switching",
      cutoff = "top_quartile"
    ),
    report_split(
      midline_to_endline,
      "midline_sd",
      "midline_n",
      "Midline rating SD -> midline-to-endline switching",
      cutoff = "top_quartile"
    )
  )

  out$one_dealer_zero <- one_dealer_zero
  out
}

results <- rbind(
  run_analysis(one_dealer_zero = FALSE),
  run_analysis(one_dealer_zero = TRUE)
)

print(results, row.names = FALSE)
