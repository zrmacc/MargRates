# -----------------------------------------------------------------------------

#' Marginal Rates
#' 
#' Calculates the marginal event rates in each arm, weighting by the total 
#' stratum size.
#' 
#' @param y0 Events per category in arm 0.
#' @param n0 Subjects per category in arm 0.
#' @param y1 Events per category in arm 1.
#' @param n1 Subjects per category in arm 1.
#' @param weights Stratum mixing weights.
#' @return List containing:
#' \itemize{
#'   \item Marginal rates in each arm, `p0` and `p1`
#'   \item Per-stratum event rates, `r0` and `r1`.
#'   \item Stratum `weights`.
#' }
MargRate <- function(y0, n0, y1, n1, weights = NULL) {
  
  # Stratum proportions.
  n <- sum(n0 + n1)
  if (is.null(weights)) {
    weights <- (n0 + n1) / n
  }
  
  # Stratum specific event rates.
  r0 <- y0 / n0
  r1 <- y1 / n1
  
  # Marginal rates.
  p0 <- sum(r0 * weights)
  p1 <- sum(r1 * weights)
  
  # Output.
  out <- list(
    "p0" = p0,
    "p1" = p1,
    "r0" = r0,
    "r1" = r1,
    "weights" = weights
  )
  return(out)
}

