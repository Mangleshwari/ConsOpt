#' Explain solution sensitivity
#'
#' Computes sensitivity of objective function with respect to parameters.
#'
#' @param fn Objective function
#' @param par Optimal parameters
#' @param eps Small perturbation
#'
#' @return List with sensitivity and importance
#' @keywords internal
explain_solution <- function(fn, par, eps=1e-4) {

  d <- length(par)
  sens <- numeric(d)

  for(i in 1:d) {
    p1 <- par
    p2 <- par

    p1[i] <- p1[i] + eps
    p2[i] <- p2[i] - eps

    sens[i] <- abs(fn(p1) - fn(p2))
  }

  importance <- sens / sum(sens)

  list(
    sensitivity = sens,
    importance = importance
  )
}
