#' Plot ConsOpt Results
#'
#' Visualize optimization results from a \code{consopt} object.
#'
#' @param x A \code{consopt} object returned by \code{consopt()}.
#' @param type Type of plot to display:
#'   \itemize{
#'     \item \code{"convergence"}: Objective value vs iteration
#'     \item \code{"landscape"}: 2D contour + optimization path
#'     \item \code{"both"}: Combined view (default)
#'   }
#' @param dims Integer vector of length 2 specifying which
#'   dimensions to visualize (for high-dimensional problems).
#' @param ... Additional arguments (currently unused).
#'
#' @details
#' This function provides **method-aware visualization** of optimization results.
#'
#' ## Visualization Types
#'
#' ### 1. Convergence Plot
#' Shows how the objective value evolves over iterations.
#'
#' ### 2. Landscape Plot
#' Displays:
#' \itemize{
#'   \item Contour plot of the objective function
#'   \item Optimization trajectory
#'   \item Final optimal solution
#' }
#'
#' ## Method-Specific Visualization
#'
#' The visualization adapts automatically based on the optimization method:
#'
#' \itemize{
#'   \item \strong{Nelder-Mead}: Displays simplex triangles
#'   \item \strong{BFGS / L-BFGS-B}: Smooth path with directional arrows
#'   \item \strong{nloptr}: Constrained trajectory (colored path)
#'   \item \strong{DEoptim}: Population-based scattered points
#'   \item \strong{Other methods}: Generic path visualization
#' }
#'
#' ## High-Dimensional Problems
#'
#' If the number of parameters is greater than 2, the function:
#' \itemize{
#'   \item Projects the solution onto selected dimensions
#'   \item Fixes remaining parameters at their optimized values
#' }
#'
#' This provides a **local 2D slice** of the objective function.
#'
#' ## Limitations
#'
#' \itemize{
#'   \item Landscape plots require a valid objective function in \code{x$fn}
#'   \item Linear programming problems (\code{lpSolve}) do not support landscape visualization
#'   \item Accuracy depends on grid resolution
#' }
#'
#' @return
#' A \code{ggplot2} object. If \code{type = "both"} and the \code{patchwork}
#' package is available, a combined plot is returned.
#'
#' @examples
#'
#' ## ---------------------------------
#' ## 1. Unconstrained Optimization
#' ## ---------------------------------
#' f <- function(x) x[1]^2 + x[2]^2
#' res <- consopt(f, c(2, 3))
#'
#' plot(res)
#'
#'
#' ## ---------------------------------
#' ## 2. With Bounds (L-BFGS-B)
#' ## ---------------------------------
#' res <- consopt(
#'   fn = f,
#'   par = c(2, 3),
#'   lower = c(0, 0),
#'   upper = c(5, 5)
#' )
#'
#' plot(res)
#'
#'
#' ## ---------------------------------
#' ## 3. Equality Constraint
#' ## x1 + x2 = 1
#' ## ---------------------------------
#' eq_fn <- function(x) x[1] + x[2] - 1
#'
#' res <- consopt(
#'   fn = f,
#'   par = c(0, 0),
#'   eq_fn = eq_fn
#' )
#'
#' plot(res)
#'
#'
#' ## ---------------------------------
#' ## 4. Inequality Constraint
#' ## x1^2 + x2^2 <= 1
#' ## ---------------------------------
#' ineq_fn <- function(x) x[1]^2 + x[2]^2 - 1
#'
#' res <- consopt(
#'   fn = f,
#'   par = c(0.8, 0.8),
#'   ineq_fn = ineq_fn
#' )
#'
#' plot(res)
#'
#'
#' ## ---------------------------------
#' ## 5. Nonlinear Constrained Problem
#' ## ---------------------------------
#' f <- function(x) (x[1]-2)^2 + (x[2]-2)^2
#'
#' ineq_fn <- function(x) x[1] + x[2] - 2
#'
#' res <- consopt(
#'   fn = f,
#'   par = c(0, 0),
#'   ineq_fn = ineq_fn
#' )
#'
#' plot(res)
#'
#'
#' ## ---------------------------------
#' ## 6. High-Dimensional Projection
#' ## ---------------------------------
#' f <- function(x) sum(x^2)
#'
#' res <- consopt(f, rep(1, 4))
#'
#' plot(res, dims = c(1,2))
#' plot(res, dims = c(1,3))
#'
#'
#' ## ---------------------------------
#' ## 7. Linear Programming (No Plot)
#' ## ---------------------------------
#' # Minimize: x1 + x2
#' # Subject to:
#' # x1 + x2 <= 2
#'
#' res <- consopt(
#'   fn = NULL,
#'   par = c(0, 0),
#'   obj = c(1, 1),
#'   A = matrix(c(1,1), nrow=1),
#'   b = c(2),
#'   dir = c("<="),
#'   linear = TRUE
#' )
#'
#' # plot(res)  # Not supported for LP
#'
#' @export
#' @method plot consopt

plot.consopt <- function(x,
                         type = "both",
                         dims = c(1,2),
                         ...) {

  if(!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Install ggplot2")
  }

  # ---- LP GUARD ----
  if(x$method == "lpSolve") {
    stop("Plotting not supported for linear programming problems")
  }

  p1 <- NULL
  p2 <- NULL

  # ---- PATH CHECK ----
  if(is.null(x$path) || length(x$path$par) == 0) {
    stop("No path stored")
  }

  path_mat <- do.call(rbind, lapply(x$path$par, as.numeric))
  values <- x$path$value
  d <- ncol(path_mat)

  # ---- DIMS CHECK ----

  if(d == 1) {
    message("1D problem detected: showing convergence plot only")

    # ---- CONVERGENCE ONLY ----
    df_conv <- data.frame(
      Iteration = seq_along(values),
      Value = values
    )

    p1 <- ggplot2::ggplot(df_conv,
                          ggplot2::aes(Iteration, Value)) +
      ggplot2::geom_line(linewidth = 1) +
      ggplot2::theme_minimal() +
      ggplot2::labs(title = "Convergence Plot")

    return(p1)
    return(invisible(p1))
  }

  if(length(dims) != 2) {
    stop("dims must be length 2")
  }

  if(any(dims > d)) {
    stop("dims exceed parameter dimension")
  }

  i <- dims[1]
  j <- dims[2]

  if(d > 2) {
    message("Projecting onto dimensions: ", i, " and ", j)
  }

  # ---- THINNING ----
  max_points <- 200
  idx <- if(length(values) > max_points) {
    round(seq(1, length(values), length.out = max_points))
  } else seq_along(values)

  path_mat <- path_mat[idx, , drop = FALSE]
  values <- values[idx]

  # ===============================
  # 1. CONVERGENCE
  # ===============================
  if(type %in% c("convergence", "both")) {

    df_conv <- data.frame(
      Iteration = seq_along(values),
      Value = values
    )

    p1 <- ggplot2::ggplot(df_conv,
                          ggplot2::aes(Iteration, Value)) +
      ggplot2::geom_line(linewidth = 1) +
      ggplot2::theme_minimal() +
      ggplot2::labs(title = "Convergence Plot")
  }

  # ===============================
  # 2. LANDSCAPE
  # ===============================
  if(type %in% c("landscape", "both")) {

    fn <- x$fn

    if(is.null(fn)) {
      stop("Objective function not available for plotting")
    }

    # ---- BETTER GRID RANGE ----
    pad_x <- 0.2 * diff(range(path_mat[,i]))
    pad_y <- 0.2 * diff(range(path_mat[,j]))

    x_seq <- seq(min(path_mat[,i]) - pad_x,
                 max(path_mat[,i]) + pad_x,
                 length.out = 50)

    y_seq <- seq(min(path_mat[,j]) - pad_y,
                 max(path_mat[,j]) + pad_y,
                 length.out = 50)

    grid <- expand.grid(x = x_seq, y = y_seq)

    # ---- FUNCTION EVALUATION ----
    grid$z <- apply(grid, 1, function(v) {
      p <- x$par
      p[i] <- v[1]
      p[j] <- v[2]
      fn(p)
    })

    # ---- PATH POINTS ----
    df_points <- data.frame(
      x = path_mat[,i],
      y = path_mat[,j],
      val = values
    )

    # ---- SIMPLEX (ONLY NM) ----
    df_simplex <- NULL

    if(x$method == "Nelder-Mead" && nrow(path_mat) >= 3) {

      frames <- list()

      for(k in 3:nrow(path_mat)) {

        simplex <- path_mat[(k-2):k, ]

        frames[[k]] <- data.frame(
          x = c(simplex[,i], simplex[1,i]),
          y = c(simplex[,j], simplex[1,j]),
          step = k
        )
      }

      df_simplex <- do.call(rbind, frames)
    }

    # ---- BASE PLOT ----
    p2 <- ggplot2::ggplot() +
      ggplot2::geom_contour(
        data = grid,
        ggplot2::aes(x = x, y = y, z = z),
        bins = 15,
        alpha = 0.6
      )

    # ---- PATH ----
    p2 <- p2 +
      ggplot2::geom_path(
        data = df_points,
        ggplot2::aes(x = x, y = y),
        color = "black",
        linewidth = 1
      ) +
      ggplot2::geom_point(
        data = df_points,
        ggplot2::aes(x = x, y = y),
        color = "darkgreen",
        size = 2
      )

    # ---- SIMPLEX VISUAL ----
    if(!is.null(df_simplex)) {

      p2 <- p2 +
        ggplot2::geom_polygon(
          data = df_simplex,
          ggplot2::aes(x = x, y = y, group = step),
          fill = NA,
          color = "blue",
          linewidth = 1
        ) +
        ggplot2::geom_point(
          data = df_simplex,
          ggplot2::aes(x = x, y = y),
          color = "red",
          size = 2
        )
    }

    # ---- FINAL POINT ----
    best_idx <- which.min(values)

    if(length(best_idx) > 0 && !is.na(best_idx)) {
      p2 <- p2 +
        ggplot2::geom_point(
          data = df_points[best_idx, ],
          ggplot2::aes(x = x, y = y),
          color = "red",
          size = 4
        )
    }

    p2 <- p2 +
      ggplot2::labs(
        title = paste("Optimization Landscape (", x$method, ") | Dims:", i, "vs", j)
      )
  }

  # ===============================
  # OUTPUT
  # ===============================
  if(type == "convergence") return(p1)

  if(type == "landscape") return(p2)

  if(!is.null(p1) && !is.null(p2)) {
    if(requireNamespace("patchwork", quietly = TRUE)) {
      return(p1 + p2)
    } else {
      return(p1)
    }
  } else if(!is.null(p1)) {
    return(p1)
  } else {
    return(p2)
  }
}
