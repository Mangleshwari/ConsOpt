#' Animate Optimization Path (Optional)
#'
#' This function creates an animated visualization of the optimization process.
#' It works best for 2D problems or 2D projections of higher-dimensional problems.
#' It requires tracking to be enabled in \code{consopt} (track = TRUE), and
#' \code{ggplot2} + \code{gganimate} packages installed.
#'
#' @param res A \code{consopt} object with tracking enabled.
#' @param dims Integer vector of length 2 specifying which dimensions
#'   to visualize for high-dimensional problems.
#'
#' @details
#' Use this function only to visualize paths for methods like BFGS, Nelder-Mead,
#' or L-BFGS-B. It may fail or produce incomplete results for global optimizers
#' (DEoptim), linear programming, or very high-dimensional problems.
#'
#' @return A GIF animation object (if successful).
#'
#' @examples
#' f <- function(x) (1 - x[1])^2 + 100 * (x[2] - x[1]^2)^2
#' res <- consopt(f, c(-1.2, 1), track = TRUE)
#'
#' \dontrun{
#' animate_consopt(res)
#' }
#'
#' @export
animate_consopt <- function(res, dims = c(1,2)) {

  # ---- CHECK PATH ----
  if(is.null(res$path) || length(res$path$par) == 0) {
    stop("No optimization path found. Enable tracking in run_solver.")
  }

  # ---- CHECK FUNCTION ----
  if(is.null(res$fn)) {
    stop("Objective function not found in result object.")
  }

  # ---- PACKAGE CHECK ----
  if(!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Install ggplot2")
  }
  if(!requireNamespace("gganimate", quietly = TRUE)) {
    stop("Install gganimate")
  }

  fn <- res$fn

  # ---- PATH MATRIX ----
  path_mat <- do.call(rbind, lapply(res$path$par, as.numeric))
  d <- ncol(path_mat)

  # ---- DIMS CHECK ----
  if(length(dims) != 2) stop("dims must be length 2")
  if(any(dims > d)) stop("dims exceed dimension")

  i <- dims[1]
  j <- dims[2]

  if(d > 2) {
    message("Projecting onto dims: ", i, " and ", j)
  }

  # ---- DATA ----
  df <- data.frame(
    x1 = path_mat[, i],
    x2 = path_mat[, j],
    step = seq_len(nrow(path_mat))
  )

  # ---- GRID ----
  x_seq <- seq(min(df$x1) - 2, max(df$x1) + 2, length.out = 80)
  y_seq <- seq(min(df$x2) - 2, max(df$x2) + 2, length.out = 80)

  grid <- expand.grid(x1 = x_seq, x2 = y_seq)

  # ---- CRITICAL FIX (same as plot) ----
  grid$z <- apply(grid, 1, function(v) {
    p <- res$par
    p[i] <- v[1]
    p[j] <- v[2]
    fn(p)
  })

  # ---- SIMPLEX (only if Nelder-Mead) ----
  df_simplex <- NULL

  if(res$method %in% c("Nelder-Mead") && nrow(path_mat) >= 3) {

    frames <- list()

    for(k in 3:nrow(path_mat)) {

      simplex <- path_mat[(k-2):k, ]

      frames[[k]] <- data.frame(
        x = c(simplex[, i], simplex[1, i]),
        y = c(simplex[, j], simplex[1, j]),
        step = k,
        group = k
      )
    }

    df_simplex <- do.call(rbind, frames)
  }

  # ---- FINAL POINT ----
  df_final <- data.frame(
    x = path_mat[nrow(path_mat), i],
    y = path_mat[nrow(path_mat), j]
  )

  # ---- PLOT ----
  p <- ggplot2::ggplot() +

    ggplot2::geom_contour(
      data = grid,
      ggplot2::aes(x = x1, y = x2, z = z),
      bins = 20,
      alpha = 0.6
    ) +

    ggplot2::geom_path(
      data = df,
      ggplot2::aes(x = x1, y = x2),
      color = "blue",
      linewidth = 1
    ) +

    ggplot2::geom_point(
      data = df,
      ggplot2::aes(x = x1, y = x2),
      color = "red",
      size = 2
    ) +

    ggplot2::geom_point(
      data = df_final,
      ggplot2::aes(x = x, y = y),
      color = "green",
      size = 4
    ) +

    ggplot2::labs(title = "Step: {current_frame}")

  # ---- SIMPLEX ADD ----
  if(!is.null(df_simplex)) {
    p <- p +
      ggplot2::geom_polygon(
        data = df_simplex,
        ggplot2::aes(x = x, y = y, group = group),
        fill = NA,
        color = "blue",
        linewidth = 1.2
      )
  }

  # ---- ANIMATION ----
  p <- p +
    gganimate::transition_manual(step) +
    gganimate::ease_aes("linear")

  gganimate::animate(
    p,
    renderer = gganimate::gifski_renderer(),
    width = 600,
    height = 500
  )
}
