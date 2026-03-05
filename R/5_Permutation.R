#' Permute Events
#'
#' Permute the number of events observed in each cell of a contingency table,
#' keeping the number of subjects in each cell fixed, and assuming no difference
#' between treatment arms.
#'
#' @param y0 Events per category in arm 0.
#' @param n0 Subjects per category in arm 0.
#' @param y1 Events per category in arm 1.
#' @param n1 Subjects per category in arm 1.
#' @return List containing the data after permutation.
PermEvents <- function(y0, n0, y1, n1) {
  
  # Number of strata.
  k <- length(y0)
  
  # Event and patient totals.
  yt <- y0 + y1
  nt <- n0 + n1

  # Draw number of events in arm 0 (vectorized).
  y0_perm <- stats::rhyper(nn = k, m = yt, n = nt - yt, k = n0)
  y1_perm <- yt - y0_perm
  
  out <- list(
    "y0" = y0_perm,
    "n0" = n0,
    "y1" = y1_perm,
    "n1" = n1
  )
  return(out)
}


# -----------------------------------------------------------------------------

#' Test the Null Hypothesis via Permutation
#' 
#' @param y0 Events per category in arm 0.
#' @param n0 Subjects per category in arm 0.
#' @param y1 Events per category in arm 1.
#' @param n1 Subjects per category in arm 1.
#' @param weights Stratum mixing weights.
#' @param alpha Type I error rate.
#' @param reps Permutation replicates.
#' @return Data.frame containing the marginal contrast estimates and permutation
#'   p-values for the risk difference, risk ratio, and odds ratio.
TestNull <- function(
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
  
  # Permutation
  aux <- function(b) {
    
    # Permute data.
    perm_data <- PermEvents(y0 = y0, n0 = n0, y1 = y1, n1 = n1)
    
    # Marginal odds ratio.
    perm <- CalcMargStats(
      y0 = perm_data$y0,
      n0 = n0,
      y1 = perm_data$y1,
      n1 = n1,
      weights = weights
    )
    perm_stats <- perm$Stats
    perm_rd <- perm_stats$Est[1]
    perm_rr <- perm_stats$Est[2]
    perm_or <- perm_stats$Est[3]
    
    # Output
    out <- c(
      "RD_P" = (abs(perm_rd) >= abs(obs_rd)),
      "RR_P" = (abs(log(perm_rr)) >= abs(log(obs_rr))),
      "OR_P" = (abs(log(perm_or)) >= abs(log(obs_or)))
    )
    return(out)
  }
  sim <- lapply(seq_len(reps), aux)
  sim <- do.call(rbind, sim)
  # Pseudo-count to avoid p-value of zero (standard practice for permutation tests).
  sim <- rbind(c(1, 1, 1), sim)

  # Output
  out <- obs_stats[, 1:2]
  out$P <- apply(sim, 2, mean)
  return(out)
}

