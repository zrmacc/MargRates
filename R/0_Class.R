#' Marginal event rates object. 
#'
#' @slot Rates Marginal rates.
#' @slot RD Risk difference.
#' @slot RR Risk ratio.
#' @slot OR Odds ratio.
#' @slot Perm Permutation test.
#' @name margRates-class
#' @rdname margRates-class
#' @exportClass margRates
setClass(
  Class = "margRates",
  representation = representation(
   Rates = "data.frame",
   RD = "data.frame",
   RR = "data.frame",
   OR = "data.frame",
   Perm = "data.frame"
  )
)

# -----------------------------------------------------------------------------
# Print Method
# -----------------------------------------------------------------------------

#' Print Method
#'
#' Print method for objects of class \code{margRates}.
#'
#' @param x An object of class \code{margRates}.
#' @param ... Unused.
#' @export
print.margRates <- function (x, ...) {
  
  # Rates.
  cat('Marginal Rates:\n')
  methods::show(x@Rates)
  cat('\n\n')

  # Risk difference.
  cat('Risk Difference:\n')
  methods::show(x@RD)
  cat('\n\n')

  # Risk ratio.
  cat('Risk Ratio:\n')
  methods::show(x@RR)
  cat('\n\n')

  # Odds ratio.
  cat('Odds Ratio:\n')
  methods::show(x@OR)
  cat('\n\n')

  # Permutation.
  if (nrow(x@Perm) > 0) {
    cat('Permutation test:\n')
    methods::show(x@Perm)
    cat('\n\n')
  }

  return(invisible(NULL))
}

# -----------------------------------------------------------------------------
# Show Method
# -----------------------------------------------------------------------------

#' Show Method
#'
#' @param object An object of class \code{margRates}.
#' @rdname fit-method
setMethod(
  f = "show",
  signature = c(object = "margRates"),
  definition = function(object) {
    print.margRates(x = object)
    return(invisible(NULL))
  }
)

