#' ConsOpt: Intelligent Constrained Optimization
#'
#' `consopt` is a hybrid optimization function that intelligently selects
#' the most suitable optimization algorithm based on the problem landscape,
#' gradient availability, constraints, and smoothness. It can perform both
#' local and global optimization, automatically analyzes the objective function,
#' and provides optional visualizations and explanations.
#'
#' @param fn A numeric function to minimize. Should accept a numeric vector `x`
#'   and return a single numeric value.
#' @param par Numeric vector of starting parameters.
#' @param lower Optional numeric vector specifying lower bounds for parameters.
#' @param upper Optional numeric vector specifying upper bounds for parameters.
#' @param eq_fn Optional function specifying equality constraints.
#' @param ineq_fn Optional function specifying inequality constraints.
#' @param gr Optional gradient function. If `NULL`, a numerical gradient is computed.
#' @param plot_results Logical. If `TRUE`, plots the optimization path and convergence.
#' @param hybrid Logical. If `TRUE`, performs global search if landscape is rough.
#' @param analyze Logical. If `TRUE`, analyzes the function landscape for smoothness and convexity.
#' @param explain Logical. If `TRUE`, generates a textual explanation of the solution.
#' @param ... Additional arguments passed to the solver functions.
#'
#' @return A list of class `"consopt"` containing:
#' \describe{
#'   \item{par}{Optimized parameters (solution).}
#'   \item{value}{Objective function value at the solution.}
#'   \item{method}{Algorithm used.}
#'   \item{start_par}{Initial parameters.}
#'   \item{landscape}{List of analyzed landscape metrics (smoothness, convexity, etc.).}
#'   \item{diagnostics}{Optional diagnostics of the optimization process.}
#'   \item{explanation}{Text explanation of the solution (if `explain = TRUE`).}
#'   \item{method_reason}{Reason why a particular method was chosen.}
#' }
#'
#' @details
#' The function automatically selects the optimization method:
#' - `BFGS` for smooth, convex problems with gradients.
#' - `L-BFGS-B` if bounds are present.
#' - `Nelder-Mead` as a derivative-free fallback.
#' - `DEoptim` for non-smooth or rough landscapes.
#' - `nloptr` when constraints are detected.
#'
#' It can perform a hybrid optimization by running a global optimizer
#' first to locate a promising starting point before refining with a local solver.
#'
#' If `plot_results = TRUE`, the function generates:
#' - A convergence plot of the objective value over iterations.
#' - A 2D contour plot (if dimension ≤ 2) showing the optimization path.
#'
#' ## Writing the Objective Function
#' The function must take a vector `x` as input:
#'
#' ```
#' function(x) {
#'   x[1]^2 + x[2]^2
#' }
#' ```
#'
#' ## Equality Constraints
#' Must be written so that:
#' ```
#' eq_fn(x) = 0
#' ```
#'
#' Example:
#' ```
#' eq_fn <- function(x) {
#'   x[1] + x[2] - 1
#' }
#' ```
#'
#' ## Inequality Constraints
#' Must satisfy:
#' ```
#' ineq_fn(x) <= 0
#' ```
#'Examples
#'
#' @examples
#' ## ---------------------------------
#' ## 1. Basic unconstrained optimization
#' ## ---------------------------------
#' f <- function(x) x[1]^2 + x[2]^2
#' res1 <- consopt(f, par = c(2, 3))
#' res1$par        # optimized parameters
#' res1$value      # objective value at optimum
#'
#' ## ---------------------------------
#' ## 2. With bounds
#' ## ---------------------------------
#' res2 <- consopt(
#'   fn = f,
#'   par = c(2, 3),
#'   lower = c(0, 0),
#'   upper = c(5, 5)
#' )
#' res2$par
#'
#' ## ---------------------------------
#' ## 3. Equality constraint
#' ## ---------------------------------
#' eq_fn <- function(x) x[1] + x[2] - 1
#' res3 <- consopt(f, par = c(0,0), eq_fn = eq_fn)
#' res3$par
#' eq_fn(res3$par)  # should be close to 0
#'
#' ## ---------------------------------
#' ## 4. Inequality constraint
#' ## ---------------------------------
#' ineq_fn <- function(x) x[1]^2 + x[2]^2 - 1
#' res4 <- consopt(f, par = c(0.8, 0.8), ineq_fn = ineq_fn)
#' res4$par
#' ineq_fn(res4$par)  # should be <= 0
#'
#' ## ---------------------------------
#' ## 5. Hybrid / global-local optimization
#' ## ---------------------------------
#' f_global <- function(x) sin(5*x[1]) + x[1]^2 / 5
#' res5 <- consopt(f_global, par = c(0), lower = -5, upper = 5, hybrid = TRUE)
#' res5$par
#' res5$method
#'
#' ## ---------------------------------
#' ## 6. Optional: Animate optimization path
#' ## Note: requires tracking and gganimate
#' \dontrun{
#' animate_consopt(res1)
#' }
#' @export
consopt <- function(fn, par, obj = NULL,
                    lower=NULL, upper=NULL,
                    eq_fn=NULL, ineq_fn=NULL,
                    A=NULL, b=NULL, dir=NULL,
                    linear=FALSE,
                    gr=NULL,
                    plot_results=TRUE,
                    hybrid=TRUE,
                    analyze=TRUE,
                    explain=TRUE,
                    ...) {

  # ---- VALIDATION ----

  if(!is.numeric(par)) stop("par must be numeric")

  if(!isTRUE(linear)) {
    if(!is.function(fn)) stop("fn must be a function")
  }

  # ---- AUTO GRADIENT ----
  if(!isTRUE(linear)) {
    if(is.null(gr)) {
      gr <- numeric_grad(fn)
    }
  }

  # ---- 1a. Problem Type Detection ----

  if(isTRUE(linear)) {

    if(is.null(obj)) {
      stop("For linear problems, provide 'obj'")
    }

    if(is.null(A) || is.null(b) || is.null(dir)) {
      stop("For linear problems, provide A, b, and dir")
    }

    if(!requireNamespace("lpSolve", quietly = TRUE)) {
      stop("Install lpSolve")
    }

    method <- "lpSolve"
    landscape <- NULL
    smooth <- NA
    convex <- NA

    cat("Detected Linear Programming Problem → using lpSolve\n")

  } else {

    # 1b. LANDSCAPE (only nonlinear)

    landscape <- if(analyze) analyze_landscape(fn, par) else NULL

    smooth <- if(!is.null(landscape)) landscape$smooth else NA
    convex <- if(!is.null(landscape)) landscape$convex else NA

  # ---- 2. Method Selection ----
    method <- choose_method(par, lower, upper,
                            eq_fn, ineq_fn,
                            gr, landscape)

    cat("Selected method:", method, "\n")
  }

  # ---- 3. Hybrid Optimization ----
  if(!isTRUE(linear) && hybrid && !isTRUE(smooth)) {

    if(!is.null(lower) && !is.null(upper)) {
      res_global <- run_solver("DEoptim", fn, par,
                               lower=lower, upper=upper)
      start_par <- res_global$par
    } else {
      start_par <- par
    }

  } else {
    start_par <- par
  }

  # ---- 4. Run Final Solver ----

  if(method == "lpSolve") {

    res_lp <- lpSolve::lp(
      direction = "min",
      objective.in = obj,
      const.mat = A,
      const.dir = dir,
      const.rhs = b
    )

    res <- list(
      par = res_lp$solution,
      value = res_lp$objval,
      method = "lpSolve",
      convergence = res_lp$status
    )

  } else if(method == "nloptr") {

    res <- run_solver(method, fn, start_par,
                      lower=lower, upper=upper,
                      eq_fn=eq_fn, ineq_fn=ineq_fn,
                      gr=NULL, ...)

  } else {

    res <- run_solver(method, fn, start_par,
                      lower=lower, upper=upper,
                      eq_fn=eq_fn, ineq_fn=ineq_fn,
                      gr=gr, ...)
  }

  # ---- 4.5 Constraint Validation ----
  if(!isTRUE(linear)) {

    tol <- 1e-6

  # Inequality check
  if(!is.null(ineq_fn)) {
    ineq_val <- tryCatch(ineq_fn(res$par), error = function(e) NA)

    if(any(is.na(ineq_val))) {
      warning("ineq_fn could not be evaluated at solution")
    } else if(any(ineq_val > tol)) {
      warning(
        paste0("Inequality constraint violated. Max violation = ",
               round(max(ineq_val), 6))
      )
    } else {
      message("Inequality constraints satisfied (within tolerance).")
    }
  }

  # Equality check
  if(!is.null(eq_fn)) {
    eq_val <- tryCatch(eq_fn(res$par), error = function(e) NA)

    if(any(is.na(eq_val))) {
      warning("eq_fn could not be evaluated at solution")
    } else if(any(abs(eq_val) > tol)) {
      warning(
        paste0("Equality constraint not satisfied. Max deviation = ",
               round(max(abs(eq_val)), 6))
      )
    } else {
      message("Equality constraints satisfied (within tolerance).")
    }
  }
  }

  # ---- 5. Add Metadata ----
  res$fn <- if(!isTRUE(linear)) fn else NULL
  res$method <- method
  res$start_par <- par
  res$landscape <- landscape
  res$eq_fn <- eq_fn
  res$ineq_fn <- ineq_fn
  res$diagnostics <- if(method == "lpSolve") {
    list(message = "Solved using linear programming")
  } else {
    diagnose_optimization(res)
  }

  # ---- 6. Explanation ----
  if(explain && !isTRUE(linear)) {
    res$explanation <- explain_solution(fn, res$par)
  }

  # ---- 7. Method Reason (CONSISTENT) ----
  res$method_reason <- switch(method,
                              "lpSolve"   = "Linear programming problem detected → solved using lpSolve",
                              "nloptr"    = "Constraints detected → using nloptr",
                              "DEoptim"   = "Non-smooth landscape → global optimization",
                              "BFGS"      = "Convex + gradient → fast local optimization",
                              "L-BFGS-B"  = "Bounds present → bounded optimization",
                              "Nelder-Mead" = "Fallback derivative-free method",
                              "Auto-selected method"
  )

  # ---- 8. Class ----
  class(res) <- "consopt"

  # ---- 9. Plot ----
  if(plot_results && !isTRUE(linear)) {
    plot(res)
  }

  return(res)
}
