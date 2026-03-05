# -----------------------------------------------------------------------------
# Input validation (internal).
# -----------------------------------------------------------------------------

#' Validate stratified count inputs.
#'
#' Ensures y0, n0, y1, n1 have matching lengths, valid counts, and optional
#' weights/alpha/reps are in range. Used internally before inference.
#'
#' @param y0 Events per category in arm 0.
#' @param n0 Subjects per category in arm 0.
#' @param y1 Events per category in arm 1.
#' @param n1 Subjects per category in arm 1.
#' @param weights Optional stratum weights (length must match strata).
#' @param alpha Type I error rate (must be in (0, 1)).
#' @param require_reps If TRUE, reps must be a positive integer (for boot/perm).
#' @param reps Number of replicates (only checked when require_reps is TRUE).
#' @return Invisible NULL; throws an error if validation fails.
#' @noRd
.ValidateCounts <- function(
  y0,
  n0,
  y1,
  n1,
  weights = NULL,
  alpha = 0.05,
  require_reps = FALSE,
  reps = NULL
) {

  k <- length(y0)
  if (length(n0) != k || length(y1) != k || length(n1) != k) {
    stop(
      "y0, n0, y1, n1 must have the same length.",
      call. = FALSE
    )
  }
  if (any(n0 <= 0 | n1 <= 0)) {
    stop("n0 and n1 must be positive in every stratum.", call. = FALSE)
  }
  if (any(y0 < 0 | y0 > n0 | y1 < 0 | y1 > n1)) {
    stop(
      "Event counts must satisfy 0 <= y0 <= n0 and 0 <= y1 <= n1.",
      call. = FALSE
    )
  }
  if (!is.null(weights)) {
    if (length(weights) != k) {
      stop("weights must have the same length as the number of strata.", call. = FALSE)
    }
    if (any(weights < 0) || sum(weights) <= 0) {
      stop("weights must be non-negative and sum to a positive value.", call. = FALSE)
    }
  }
  if (alpha <= 0 || alpha >= 1) {
    stop("alpha must be in (0, 1).", call. = FALSE)
  }
  if (require_reps) {
    if (is.null(reps) || length(reps) != 1 || !is.finite(reps) || reps < 1) {
      stop("reps must be a positive integer when boot or perm is TRUE.", call. = FALSE)
    }
  }

  return(invisible(NULL))
}

