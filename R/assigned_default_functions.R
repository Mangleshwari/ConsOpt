#' Constrained Optimization Function
#'
#' @description Performs constrained optimization with bounds and optional nonlinear constraints.
#'
#' @param fn Objective function
#' @param par Initial parameters
#' @param lower Lower bounds
#' @param upper Upper bounds
#' @param gr Gradient function (optional)
#' @param eq_fn Equality constraint function (optional)
#' @param ineq_fn Inequality constraint function (optional, should return >= 0)
#' @param penalty Penalty weight
#' @param visualize Logical, whether to plot convergence
#' @param method Optimization method
#' @param control List of control parameters
#'
#' @return List with optimal parameters and diagnostics
#' @export
cons_opt <- function(fn, par, lower = NULL, upper = NULL,
                     gr = NULL,
                     eq_fn = NULL,
                     ineq_fn = NULL,
                     penalty = 1e5,
                     visualize = FALSE,
                     method = "L-BFGS-B",
                     control = list()) {

  n <- length(par)
  if (is.null(lower)) lower <- rep(-Inf, n)
  if (is.null(upper)) upper <- rep(Inf, n)

  path <- list()
  iter <- 0

  penalized_fn <- function(p) {
    val <- fn(p)

    # Equality penalty
    if (!is.null(eq_fn)) {
      val <- val + penalty * sum(eq_fn(p)^2)
    }

    # Inequality penalty
    if (!is.null(ineq_fn)) {
      g <- ineq_fn(p)
      val <- val + penalty * sum(pmax(0, -g)^2)
    }

    if (visualize) {
      iter <<- iter + 1
      path[[iter]] <<- c(iter, val)
    }

    return(val)
  }

  res <- optim(par, penalized_fn,
               gr = gr,
               method = method,
               lower = lower,
               upper = upper,
               control = control)

  if (visualize && length(path) > 1) {
    plot_convergence(path)
  }

  return(res)
}

