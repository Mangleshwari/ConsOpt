get_method_type <- function(method) {
  if(method == "Nelder-Mead") return("simplex")
  if(method %in% c("BFGS", "L-BFGS-B")) return("gradient")
  if(method == "DEoptim") return("population")
  if(method == "nloptr") return("constrained")
  return("generic")
}
