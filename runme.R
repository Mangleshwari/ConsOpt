# runme.R
# Demonstration of ConsOpt package

library(ConsOpt)  # your package

# Helper to check constraints quickly
check_constraints <- function(res) {
  if(!is.null(res$eq_fn)) cat("Equality:", res$eq_fn(res$par), "\n")
  if(!is.null(res$ineq_fn)) cat("Inequality:", res$ineq_fn(res$par), "\n")
}

# -----------------------------
# 1. Smooth convex (BFGS)
f1 <- function(x) x[1]^2 + x[2]^2
res1 <- consopt(f1, c(3, -2))
print(res1$par)
plot(res1)

# -----------------------------
# 2. With bounds (L-BFGS-B)
f2 <- function(x) (x[1]-3)^2 + (x[2]-3)^2
res2 <- consopt(fn = f2, par = c(0,0), lower = c(1,1), upper = c(2,2))
print(res2$par)
plot(res2)

# -----------------------------
# 3. Non-smooth (Nelder-Mead)
f3 <- function(x) abs(x[1]) + abs(x[2])
res3 <- consopt(f3, c(2,-3))
print(res3$method)
plot(res3)

# -----------------------------
# 4. Multi-modal (DEoptim)
f4 <- function(x) sin(5*x[1]) + x[1]^2 / 5
res4 <- consopt(fn = f4, par = c(0), lower = -5, upper = 5)
print(res4$method)
print(res4$par)

# -----------------------------
# 5. Equality constraint (nloptr)
f5 <- function(x) x[1]^2 + x[2]^2
eq_fn5 <- function(x) x[1] + x[2] - 1
res5 <- consopt(fn = f5, par = c(0,0), eq_fn = eq_fn5)
print(res5$par)
check_constraints(res5)
plot(res5)

# -----------------------------
# 6. Inequality constraint (nloptr)
f6 <- function(x) x[1]^2 + x[2]^2
ineq_fn6 <- function(x) x[1]^2 + x[2]^2 - 1
res6 <- consopt(fn = f6, par = c(0.8,0.8), ineq_fn = ineq_fn6)
print(res6$par)
check_constraints(res6)
plot(res6)

# -----------------------------
# 7. Constraint changes solution
f7 <- function(x) (x[1]-2)^2 + (x[2]-2)^2
ineq_fn7 <- function(x) x[1] + x[2] - 2
res7 <- consopt(fn = f7, par = c(0,0), ineq_fn = ineq_fn7)
print(res7$par)
plot(res7)

# -----------------------------
# 8. Linear Programming (lpSolve)
res8 <- consopt(fn = NULL, par = c(0,0),
                obj = c(1,2),
                A = matrix(c(1,1, 1,0, 0,1), nrow=3, byrow=TRUE),
                b = c(4,2,3),
                dir = c("<=","<=","<="),
                linear = TRUE)
print(res8$par)
print(res8$value)

# -----------------------------
# 9. High-dimensional projection check
f9 <- function(x) sum(x^2)
res9 <- consopt(f9, rep(1,5))
plot(res9, dims=c(1,2))
plot(res9, dims=c(2,5))

# -----------------------------
# 10. Hybrid behavior (global + local)
f10 <- function(x) x[1]^4 - 3*x[1]^2 + x[1]
res10 <- consopt(fn = f10, par = c(2), lower=-3, upper=3, hybrid=TRUE)
print(res10$method)
print(res10$par)

# -----------------------------
# 11. Edge case: flat function
f11 <- function(x) 5
res11 <- consopt(f11, c(1,1))
print(res11$par)

# -----------------------------
# 12. Edge case: bad start
f12 <- function(x) log(x[1]^2 + x[2]^2 + 1)
res12 <- consopt(f12, c(100,-100))

cat("\n✅ runme.R executed all test cases successfully!\n")