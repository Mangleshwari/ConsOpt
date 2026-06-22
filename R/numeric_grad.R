#' Numerical Gradient Approximation
#'
#' Computes numerical gradient using central difference.
#'
#' @param fn Objective function
#' @param eps Small perturbation
#'
#' @return A function that computes gradient at given point
#' @keywords internal
numeric_grad <- function(fn, eps = 1e-6) {
  function(x) {
    g <- numeric(length(x))

    for(i in seq_along(x)) {
      x_up <- x
      x_down <- x

      x_up[i] <- x_up[i] + eps
      x_down[i] <- x_down[i] - eps

      g[i] <- (fn(x_up) - fn(x_down)) / (2 * eps)
    }

    return(g)
  }
}
