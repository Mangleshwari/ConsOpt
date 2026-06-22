#' Summary of ConsOpt Result
#'
#' @param object A consopt object
#' @param ... Additional arguments
#'
#' @export
#' @method summary consopt
summary.consopt <- function(object, ...) {

  cat("========== ConsOpt Summary ==========\n")

  cat("Method used       :", object$method, "\n")
  cat("Final value       :", object$value, "\n")
  cat("Convergence code  :", object$convergence, "\n")

  if(!is.null(object$time)) {
    cat("Execution time    :", round(object$time, 4), "seconds\n")
  }

  # ---- Diagnostics ----
  if(!is.null(object$diagnostics)) {
    cat("\n--- Diagnostics ---\n")
    cat(object$diagnostics, "\n")
  }

  # ---- Landscape ----
  if(!is.null(object$landscape)) {

    cat("\n--- Landscape Analysis ---\n")
    cat("Smooth            :", object$landscape$smooth, "\n")
    cat("Convex            :", object$landscape$convex, "\n")

    if(!is.na(object$landscape$min_eigenvalue)) {
      cat("Min eigenvalue    :", round(object$landscape$min_eigenvalue, 6), "\n")
    }

    # Smart interpretation
    if(isTRUE(object$landscape$convex)) {
      cat("Interpretation    : Convex problem (global minimum guaranteed)\n")
    } else if(!is.na(object$landscape$min_eigenvalue) &&
              object$landscape$min_eigenvalue < 0) {
      cat("Interpretation    : Non-convex (multiple minima possible)\n")
    }
  }

  # ---- Variable Importance ----
  if(!is.null(object$explanation)) {

    cat("\n--- Variable Importance ---\n")

    imp <- object$explanation$importance
    names(imp) <- paste0("x", seq_along(imp))

    print(round(imp, 3))
  }

  cat("\n=====================================\n")
}
