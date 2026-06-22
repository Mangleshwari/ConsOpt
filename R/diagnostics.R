diagnose_optimization <- function(res) {

  # Successful case
  if(res$convergence == 0) {
    return("Optimization successful")
  }

  msg <- c()

  # Max iterations
  if(res$convergence == 1) {
    msg <- c(msg, "Max iterations reached")
  }

  # Numerical issues
  if(is.nan(res$value) || is.infinite(res$value)) {
    msg <- c(msg, "Numerical instability detected")
  }

  # Fallback
  if(length(msg) == 0) {
    msg <- "Unknown issue"
  }

  return(paste(msg, collapse = "; "))
}
