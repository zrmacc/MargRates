# -----------------------------------------------------------------------------
# Internal: compute risk difference from precomputed marginal rates.
# -----------------------------------------------------------------------------

.RiskDiffFromMarg <- function(marg, n0, n1, alpha = 0.05) {

  p0 <- marg$p0
  r0 <- marg$r0
  p1 <- marg$p1
  r1 <- marg$r1
  weights <- marg$weights

  risk_diff <- p1 - p0
  v <- sum(r1 * (1 - r1) / n1 * weights^2) + sum(r0 * (1 - r0) / n0 * weights^2)
  se <- sqrt(v)
  if (se <= 0) {
    se <- NA
    p_val <- 1
    lower <- upper <- risk_diff
  } else {
    z_stat <- risk_diff / se
    p_val <- 2 * stats::pnorm(q = abs(z_stat), lower.tail = FALSE)
    crit <- stats::qnorm(p = 1 - alpha / 2)
    lower <- risk_diff - crit * se
    upper <- risk_diff + crit * se
  }

  out <- data.frame(
    "Stat" = "RiskDiff",
    "Est" = risk_diff,
    "SE" = se,
    "Lower" = lower,
    "Upper" = upper,
    "P" = p_val
  )
  return(out)
}


# -----------------------------------------------------------------------------
# Asymptotic Risk Difference (exported).
# -----------------------------------------------------------------------------

#' Asymptotic Risk Difference
#'
#' @param y0 Events per category in arm 0.
#' @param n0 Subjects per category in arm 0.
#' @param y1 Events per category in arm 1.
#' @param n1 Subjects per category in arm 1.
#' @param weights Stratum mixing weights.
#' @param alpha Type I error rate.
#' @export
#' @return Data.frame containing:
#' \itemize{
#'   \item 'Est', the estimated risk difference, arm 1 minus arm 0.
#'   \item The standard error 'SE'.
#'   \item The 'Lower' and 'Upper' confidence bounds.
#'   \item The asymptotic 'P' value.
#' }
RiskDiff <- function(y0, n0, y1, n1, weights = NULL, alpha = 0.05) {

  marg <- MargRate(y0, n0, y1, n1, weights)
  out <- .RiskDiffFromMarg(marg, n0, n1, alpha)
  return(out)
}


# -----------------------------------------------------------------------------
# Internal: compute risk ratio from precomputed marginal rates.
# -----------------------------------------------------------------------------

.RiskRatioFromMarg <- function(marg, n0, n1, alpha = 0.05) {

  p0 <- marg$p0
  r0 <- marg$r0
  p1 <- marg$p1
  r1 <- marg$r1
  weights <- marg$weights

  if (p0 <= 0 || !is.finite(p0)) {
    out <- data.frame(
      "Stat" = "RiskRatio",
      "Est" = NA_real_,
      "SE" = NA_real_,
      "Lower" = NA_real_,
      "Upper" = NA_real_,
      "P" = NA_real_
    )
    return(out)
  }

  risk_ratio <- p1 / p0
  v <- sum(r1 * (1 - r1) / n1 * weights^2) / p1^2 +
    sum(r0 * (1 - r0) / n0 * weights^2) / p0^2
  se <- sqrt(v)
  if (se <= 0 || !is.finite(se)) {
    se <- NA_real_
    p_val <- 1
    lower <- upper <- risk_ratio
  } else {
    z_stat <- log(risk_ratio) / se
    p_val <- 2 * stats::pnorm(q = abs(z_stat), lower.tail = FALSE)
    crit <- stats::qnorm(p = 1 - alpha / 2)
    lower <- risk_ratio * exp(-crit * se)
    upper <- risk_ratio * exp(+crit * se)
  }

  out <- data.frame(
    "Stat" = "RiskRatio",
    "Est" = risk_ratio,
    "SE" = se * risk_ratio,
    "Lower" = lower,
    "Upper" = upper,
    "P" = p_val
  )
  return(out)
}


# -----------------------------------------------------------------------------
# Asymptotic Risk Ratio (exported).
# -----------------------------------------------------------------------------

#' Asymptotic Risk Ratio
#'
#' @param y0 Events per category in arm 0.
#' @param n0 Subjects per category in arm 0.
#' @param y1 Events per category in arm 1.
#' @param n1 Subjects per category in arm 1.
#' @param weights Stratum mixing weights.
#' @param alpha Type I error rate.
#' @export
#' @return Data.frame containing:
#' \itemize{
#'   \item 'Est', the risk ratio (arm 1 over arm 0).
#'   \item The standard error 'SE'.
#'   \item The 'Lower' and 'Upper' confidence bounds.
#'   \item The asymptotic 'P' value.
#' }
RiskRatio <- function(y0, n0, y1, n1, weights = NULL, alpha = 0.05) {

  marg <- MargRate(y0, n0, y1, n1, weights)
  out <- .RiskRatioFromMarg(marg, n0, n1, alpha)
  return(out)
}


# -----------------------------------------------------------------------------
# Internal: compute odds ratio from precomputed marginal rates.
# -----------------------------------------------------------------------------

.OddsRatioFromMarg <- function(marg, n0, n1, alpha = 0.05) {

  p0 <- marg$p0
  r0 <- marg$r0
  p1 <- marg$p1
  r1 <- marg$r1
  weights <- marg$weights

  denom0 <- p0 * (1 - p0)
  denom1 <- p1 * (1 - p1)
  if (denom0 <= 0 || denom1 <= 0 || !is.finite(denom0) || !is.finite(denom1)) {
    out <- data.frame(
      "Stat" = "OddsRatio",
      "Est" = NA_real_,
      "SE" = NA_real_,
      "Lower" = NA_real_,
      "Upper" = NA_real_,
      "P" = NA_real_
    )
    return(out)
  }

  odds_ratio <- (p1 / (1 - p1)) / (p0 / (1 - p0))
  v <- sum(r1 * (1 - r1) / n1 * weights^2) / denom1^2 +
    sum(r0 * (1 - r0) / n0 * weights^2) / denom0^2
  se <- sqrt(v)
  if (se <= 0 || !is.finite(se)) {
    se <- NA_real_
    p_val <- 1
    lower <- upper <- odds_ratio
  } else {
    z_stat <- log(odds_ratio) / se
    p_val <- 2 * stats::pnorm(q = abs(z_stat), lower.tail = FALSE)
    crit <- stats::qnorm(p = 1 - alpha / 2)
    lower <- odds_ratio * exp(-crit * se)
    upper <- odds_ratio * exp(+crit * se)
  }

  out <- data.frame(
    "Stat" = "OddsRatio",
    "Est" = odds_ratio,
    "SE" = se * odds_ratio,
    "Lower" = lower,
    "Upper" = upper,
    "P" = p_val
  )
  return(out)
}


# -----------------------------------------------------------------------------
# Asymptotic Odds Ratio (exported).
# -----------------------------------------------------------------------------

#' Asymptotic Odds Ratio
#'
#' @param y0 Events per category in arm 0.
#' @param n0 Subjects per category in arm 0.
#' @param y1 Events per category in arm 1.
#' @param n1 Subjects per category in arm 1.
#' @param weights Stratum mixing weights.
#' @param alpha Type I error rate.
#' @export
#' @return Data.frame containing:
#' \itemize{
#'   \item 'Est', the odds ratio.
#'   \item The standard error 'SE'.
#'   \item The 'Lower' and 'Upper' confidence bounds.
#'   \item The asymptotic 'P' value.
#' }
OddsRatio <- function(y0, n0, y1, n1, weights = NULL, alpha = 0.05) {

  marg <- MargRate(y0, n0, y1, n1, weights)
  out <- .OddsRatioFromMarg(marg, n0, n1, alpha)
  return(out)
}


