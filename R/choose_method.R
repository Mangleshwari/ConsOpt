choose_method <- function(par, lower, upper,
                          eq_fn, ineq_fn,
                          gr, landscape) {

  # ---- 1. Constraints (highest priority) ----
  if(!is.null(eq_fn) || !is.null(ineq_fn)) {
    return("nloptr")
  }

  # ---- 2. Bounds only ----
  if(!is.null(lower) && !is.null(upper)) {
    return("L-BFGS-B")
  }

  # ---- 3. Gradient-based methods ----
  if(!is.null(gr)) {
    return("BFGS")
  }

  # ---- 4. Use landscape ONLY as soft signal ----
  if(!is.null(landscape)) {

    # clearly non-smooth → avoid gradient
    if(!isTRUE(landscape$smooth)) {
      return("Nelder-Mead")
    }

    # smooth → try fast method
    if(isTRUE(landscape$smooth)) {
      return("BFGS")
    }
  }

  # ---- 5. Default fallback ----
  return("Nelder-Mead")
}
