# ConsOpt: Simplified Constrained Optimization With Automatic Bounds Handling

**ConsOpt** ia a user-friendly R package that simplifies constrained and unconstrained optimization with automatic bounds handling and optional convergence visualization.

Many optimization packages in R are powerful, but choosing the right optimizer often feels like a guessing game. Should you use Nelder-Mead or BFGS? Is your objective function smooth? Does it contain equality or inequality constraints? Will the selected solver even converge?

Inspired by the ease-of-use philosophy of MATLAB's optimization toolbox, ConsOpt aims to provide a unified interface that automatically selects suitable optimization algorithms based on the problem structure, reducing the burden of manual solver selection.

The package automatically selects suitable optimization algorithms based on the problem structure, supporting:

* Unconstrained optimization
* Box-constrained optimization
* Equality constraints
* Inequality constraints
* Linear programming
* Non-smooth optimization
* Global optimization
* Hybrid global-local optimization

## Features

* Automatic optimizer selection

* Support for smooth and non-smooth objective functions

* Equality and inequality constraints

* Linear programming interface

* Global optimization via Differential Evolution

* Hybrid optimization (global + local refinement)

* Built-in visualization of optimization results

## Traditional Workflow

For a simple objective function,

```r
f <- function(x) x[1]^2 + x[2]^2
```

users often need to experiment with different optimizers manually:

```r
optim(c(2,3), f, method = "Nelder-Mead")

optim(c(2,3), f, method = "BFGS")
```

Selecting the appropriate method requires prior knowledge of the problem characteristics and optimizer behavior.

---

## Meet ConsOpt

ConsOpt does not simply solve optimization problems—it analyzes them.

The package automatically inspects the objective function and constraints to choose a suitable optimization strategy, whether that is:

BFGS for smooth differentiable objectives
Nelder-Mead for derivative-free optimization
Differential Evolution for global optimization
Hybrid approaches combining global and local search

## Installation

```r
# Install from GitHub
install.packages("remotes")
remotes::install_github("yourusername/ConsOpt")
```

Load the package:

```r
library(ConsOpt)
```
---
## Quick Start
library(ConsOpt)
```r
f <- function(x) x[1]^2 + x[2]^2

res <- consopt(
  f,
  par = c(2,3),
  plot_results = TRUE
)
```
Animate the optimization path:

```r
animate_consopt(res)  # Requires gganimate (optional)
```

Visualize convergence:

```r
plot(res)
```
---

## Visualization

ConsOpt not only finds solutions but also shows how they are obtained.

The package provides:

Convergence plots
Contour visualizations
Optimization path tracking
Animated optimization trajectories

These tools help users understand optimizer behavior, diagnose convergence issues, and compare algorithm performance.

---

## Constraint Handling

ConsOpt automatically validates and manages:

Equality constraints
Inequality constraints
Variable bounds
Linear constraints

allowing users to focus on modeling rather than solver-specific implementation details.

## Goal

The goal of ConsOpt is to make optimization in R more accessible, reproducible, and intuitive by combining automatic solver selection, constraint management, and rich visualization within a single framework.

## Basic Usage

### Unconstrained Optimization

```r
f <- function(x) x[1]^2 + x[2]^2

res <- consopt(f, c(3, -2))

res$par
plot(res)
```

Expected optimum:

```r
[1] 0 0
```

---

## Box-Constrained Optimization

```r
f <- function(x) (x[1]-3)^2 + (x[2]-3)^2

res <- consopt(
  fn = f,
  par = c(0,0),
  lower = c(1,1),
  upper = c(2,2)
)

res$par
```

---

## Equality Constraints

```r
f <- function(x) x[1]^2 + x[2]^2

eq_fn <- function(x) x[1] + x[2] - 1

res <- consopt(
  fn = f,
  par = c(0,0),
  eq_fn = eq_fn
)

res$par
```

Constraint:

[
x_1 + x_2 = 1
]

---

## Inequality Constraints

```r
f <- function(x) x[1]^2 + x[2]^2

ineq_fn <- function(x)
  x[1]^2 + x[2]^2 - 1

res <- consopt(
  fn = f,
  par = c(0.8, 0.8),
  ineq_fn = ineq_fn
)
```

---

## Global Optimization

```r
f <- function(x)
  sin(5*x[1]) + x[1]^2/5

res <- consopt(
  fn = f,
  par = c(0),
  lower = -5,
  upper = 5
)

res$method
res$par
```

Uses Differential Evolution for multi-modal objective functions.

---

## Hybrid Optimization

```r
f <- function(x)
  x[1]^4 - 3*x[1]^2 + x[1]

res <- consopt(
  fn = f,
  par = c(2),
  lower = -3,
  upper = 3,
  hybrid = TRUE
)

res$method
res$par
```

Hybrid mode combines:

1. Global search
2. Local refinement

for improved convergence.

---

## Linear Programming

```r
res <- consopt(
  fn = NULL,
  par = c(0,0),
  obj = c(1,2),
  A = matrix(
    c(1,1,
      1,0,
      0,1),
    nrow = 3,
    byrow = TRUE
  ),
  b = c(4,2,3),
  dir = c("<=","<=","<="),
  linear = TRUE
)

res$par
res$value
```

---

## Visualization

Optimization results can be visualized directly:

```r
plot(res)
```

High-dimensional projections:

```r
plot(res, dims = c(1,2))
plot(res, dims = c(2,5))
```

---

## Supported Solvers

| Problem Type           | Solver                 |
| ---------------------- | ---------------------- |
| Smooth unconstrained   | BFGS                   |
| Box-constrained        | L-BFGS-B               |
| Non-smooth             | Nelder-Mead            |
| Equality constraints   | nloptr                 |
| Inequality constraints | nloptr                 |
| Linear programming     | lpSolve                |
| Global optimization    | DEoptim                |
| Hybrid optimization    | DEoptim + Local Search |

---

## Demonstration

The package includes a complete demonstration script (`runme.R`) covering:

* Convex optimization
* Bound constraints
* Non-smooth objectives
* Multi-modal functions
* Equality constraints
* Inequality constraints
* Linear programming
* High-dimensional problems
* Hybrid optimization
* Edge cases

Run:

```r
source("runme.R")
```

to execute all examples.

---

## Example Output

```r
runme.R executed all test cases successfully!
```

---

## License

MIT + file LICENSE
