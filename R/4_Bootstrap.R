#' Bootstrap Events
#'
#' Bootstrap the number of events observed in each cell of a contingency table,
#' keeping the number of subjects in each cell fixed.
#'
#' @param y0 Events per category in arm 0.
#' @param n0 Subjects per category in arm 0.
#' @param y1 Events per category in arm 1.
#' @param n1 Subjects per category in arm 1.
#' @return List containing the bootstrapped event counts.
BootEvents <- function(y0, n0, y1, n1) {

  # Categories.
  k <- length(n0)

  # Event rates.
  n <- c(n0, n1)
  rates <- c(y0 / n0, y1 / n1)
  cells <- length(rates)

  # Bootstrap counts (vectorized).
  y_boot <- stats::rbinom(n = cells, size = n, prob = rates)

  # Output.
  out <- list()
  out$y0 <- y_boot[1:k]
  out$y1 <- y_boot[(k + 1):(2 * k)]
  return(out)
}


# -----------------------------------------------------------------------------

#' Bootstrap Summary Statistics.
#'
#' @param y0 Events per category in arm 0.
#' @param n0 Subjects per category in arm 0.
#' @param y1 Events per category in arm 1.
#' @param n1 Subjects per category in arm 1.
#' @param weights Stratum mixing weights.
#' @param alpha Type I error rate.
#' @param reps Bootstrap replicates.
#' @return Data.frame containing bootstrap estimates, SEs, confidence limits,
#'   and a two-sided bootstrap p-value (based on risk difference sign flip).
StatsBoot <- function(
  y0,
  n0,
  y1,
  n1,
  weights = NULL,
  alpha = 0.05,
  reps
) {

  # Observed.
  obs <- CalcMargStats(
    y0 = y0,
    n0 = n0,
    y1 = y1,
    n1 = n1,
    weights = weights,
    alpha = alpha
  )
  obs_stats <- obs$Stats
  obs_rd <- obs_stats$Est[1]
  obs_rr <- obs_stats$Est[2]
  obs_or <- obs_stats$Est[3]

  # Bootstrap.
  aux <- function(b) {

    # Bootstrap data.
    boot_data <- BootEvents(y0 = y0, n0 = n0, y1 = y1, n1 = n1)

    # Marginal stats.
    boot <- CalcMargStats(
      y0 = boot_data$y0,
      n0 = n0,
      y1 = boot_data$y1,
      n1 = n1,
      weights = weights
    )
    boot_stats <- boot$Stats
    boot_rd <- boot_stats$Est[1]
    boot_rr <- boot_stats$Est[2]
    boot_or <- boot_stats$Est[3]

    # Output.
    out <- c(
      "RD" = boot_rd,
      "RR" = boot_rr,
      "OR" = boot_or,
      "P" = 1 * (sign(obs_rd) != sign(boot_rd))
    )
    return(out)
  }
  sim <- lapply(seq_len(reps), aux)
  sim <- do.call(rbind, sim)

  # CI.
  alpha2 <- alpha / 2
  cis <- lapply(seq_len(3), function(x) {
    int <- stats::quantile(sim[, x], probs = c(alpha2, 1 - alpha2), na.rm = TRUE)
    return(as.numeric(int))
  })
  cis <- do.call(rbind, cis)
  colnames(cis) <- c("Lower", "Upper")

  # SEs.
  ses <- lapply(seq_len(3), function(x) {
    int <- stats::sd(sim[, x], na.rm = TRUE)
    return(as.numeric(int))
  })
  ses <- do.call(c, ses)

  # Two-sided bootstrap p-value (proportion of replicates with sign flip in RD).
  p_val <- min(2 * mean(sim[, 4], na.rm = TRUE), 1)

  # Output.
  out <- obs_stats[, 1:2]
  out$SE <- ses
  out <- cbind(out, cis)
  out$P <- p_val
  return(out)
}

