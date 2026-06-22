#' Run Optimization Solver (Internal)
#'
#' Unified wrapper for different optimization algorithms with path tracking.
#' @keywords internal
run_solver <- function(method, fn, par,
                       lower=NULL, upper=NULL,
                       eq_fn=NULL, ineq_fn=NULL,
                       gr=NULL, control=list(),
                       track=TRUE, ...) {

  # ---- VALIDATION ----
  if(!is.numeric(par)) stop("par must be numeric")
  if(!is.function(fn)) stop("fn must be a function")
  method <- toupper(as.character(method))

  # ---- DEFAULT CONTROL ----
  control <- modifyList(list(maxit = 200, trace = 0), control)

  # ---- PATH TRACKING ----
  path <- list(par = list(), value = c())
  track_fn <- function(x) {
    val <- fn(x)
    if(track) {
      path$par[[length(path$par) + 1]] <<- x
      path$value <<- c(path$value, val)
    }
    return(val)
  }

  # ---- SOLVER SWITCH ----
  res <- NULL

  if(method == "BFGS" || method == "L-BFGS-B" || method == "Nelder-Mead") {

    opt_method <- switch(method,
                         "BFGS" = "BFGS",
                         "L-BFGS-B" = "L-BFGS-B",
                         "Nelder-Mead" = "Nelder-Mead")

    if(is.null(gr)) gr_use <- NULL else gr_use <- gr

    use_bounds <- (opt_method == "L-BFGS-B") &&
      !is.null(lower) && !is.null(upper)

    if(use_bounds) {
      out <- optim(par, fn=track_fn, gr=gr_use,
                   method="L-BFGS-B",
                   lower=lower, upper=upper,
                   control=control, ...)
    } else {
      out <- optim(par, fn=track_fn, gr=gr_use,
                   method=opt_method,
                   control=control, ...)
    }

    res <- list(
      par = out$par,
      value = out$value,
      convergence = out$convergence
    )

  } else if(method == "DEOPTIM") {

    if(is.null(lower) || is.null(upper))
      stop("DEoptim requires lower and upper bounds")

    if(!requireNamespace("DEoptim", quietly = TRUE))
      stop("Install DEoptim for DEoptim method")

    ctrl <- DEoptim::DEoptim.control(
      itermax = control$maxit,
      trace = as.logical(control$trace)
    )

    DE_fn <- function(x) track_fn(x)

    out <- DEoptim::DEoptim(
      fn = DE_fn,
      lower = lower,
      upper = upper,
      control = ctrl
    )

    res <- list(
      par = out$optim$bestmem,
      value = out$optim$bestval,
      convergence = 0
    )
  } else if(method == "NLOPTR") {

    if(!requireNamespace("nloptr", quietly = TRUE))
      stop("Install nloptr for nloptr method")

    # Choose algorithm based on constraints and gradient
    algo <- if(!is.null(eq_fn) || !is.null(ineq_fn)) {
      "NLOPT_LN_COBYLA"      # constraint solver, derivative-free
    } else if(!is.null(gr)) {
      "NLOPT_LD_LBFGS"       # gradient-based local solver
    } else {
      "NLOPT_LN_BOBYQA"      # derivative-free fallback
    }

    opts <- list(
      algorithm = algo,
      maxeval = control$maxit
    )

    out <- nloptr::nloptr(
      x0 = par,
      eval_f = function(x) track_fn(x),
      lb = lower, ub = upper,
      eval_g_eq = eq_fn,
      eval_g_ineq = ineq_fn,
      opts = opts
    )

    res <- list(
      par = out$solution,
      value = out$objective,
      convergence = out$status
    )

  } else {
    stop("Unknown method: ", method)
  }

  # ---- ATTACH PATH & METADATA ----
  res$method <- method
  res$path <- path

  return(res)
}
