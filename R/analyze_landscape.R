analyze_landscape <- function(fn, par, n = 30, eps = 1e-4) {

  d <- length(par)

  # ---- Adaptive scale ----
  scale <- max(abs(par), 1)

  # ---- Sampling ----
  samples <- matrix(0, nrow = n, ncol = d)
  for (i in 1:n) {
    samples[i, ] <- par + rnorm(d, sd = 0.1 * scale)
  }

  vals <- apply(samples, 1, fn)

  # ---- Gradient Estimates ----
  grad_estimates <- matrix(0, nrow = n, ncol = d)

  for (i in 1:n) {
    x <- samples[i, ]

    for (j in 1:d) {
      x_up <- x; x_down <- x

      x_up[j] <- x_up[j] + eps
      x_down[j] <- x_down[j] - eps

      grad_estimates[i, j] <- (fn(x_up) - fn(x_down)) / (2 * eps)
    }
  }

  grad_norms <- apply(grad_estimates, 1, function(g) sqrt(sum(g^2)))

  roughness <- var(grad_norms)

  # ---- Normalize roughness ----
  norm_factor <- mean(abs(vals)) + 1e-6
  roughness_scaled <- roughness / norm_factor

  smooth <- roughness_scaled < 1

  # ---- Convexity (ONLY for small d) ----
  convex <- NA
  min_eigenvalue <- NA

  if(d <= 10) {
    hessian <- matrix(0, d, d)

    for (i in 1:d) {
      for (j in 1:d) {

        x_pp <- par; x_pm <- par
        x_mp <- par; x_mm <- par

        x_pp[c(i,j)] <- x_pp[c(i,j)] + c(eps, eps)
        x_pm[c(i,j)] <- x_pm[c(i,j)] + c(eps, -eps)
        x_mp[c(i,j)] <- x_mp[c(i,j)] + c(-eps, eps)
        x_mm[c(i,j)] <- x_mm[c(i,j)] + c(-eps, -eps)

        hessian[i, j] <- (fn(x_pp) - fn(x_pm) - fn(x_mp) + fn(x_mm)) / (4 * eps^2)
      }
    }

    eigvals <- eigen(hessian, symmetric = TRUE)$values

    convex <- all(eigvals >= -1e-6)
    min_eigenvalue <- min(eigvals)
  }

  list(
    roughness = roughness_scaled,
    convex = convex,
    smooth = smooth,
    grad_variability = roughness_scaled,
    min_eigenvalue = min_eigenvalue
  )
}
