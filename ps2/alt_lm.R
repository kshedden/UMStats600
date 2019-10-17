# beta_tilde is the estimator that takes all possible secant lines
# between pairs of points in the sample, and averages their slopes.
# This estimator is linear and unbiased, but in general is not
# identical to OLS.  Here we show that for fixed n, there exists
# a design (set of covariate values x1, ..., xn) for which these
# two estimators are identical.
#
# The code below uses broadcasting tricks to make the gradient
# and function run fast.  This is an optional skill for this
# course.  You should be able to get the same answer using
# a more basic approach with nested loops.

# Number of design points
n = 10

# Starting design
x = seq(-1, 1, length.out=n)
x = sqrt(n) * x / sqrt(sum(x^2))

# Return the coefficients that define beta_tilde as a linear
# function of y.  The constant factor (n choose 2) is omitted.
estimator_coef = function(x) {
    n = length(x)
    cf = array(0, n)
    for (j in 1:n) {
        cf[j] = sum(1 / (x[j] - x[-j]))
    }
    return(cf)
}

# The variance of beta_tilde, omitting the constant factor.
va_fun = function(x) {
    cf = estimator_coef(x)
    return(sum(cf^2))
}


# The variance of beta_tilde, and its gradient with respect
# to the design, both omitting the constant factor.
va_fun_grad = function(x) {

    n = length(x)
    cf = estimator_coef(x)

    # g[j,k] is the derivative of c[j]^2 with respect to x[k]
    d = array(x, c(n, n)) - t(array(x, c(n, n)))
    d = 1 / d^2
    diag(d) = 0
    g = 2 * d * as.vector(cf)
    diag(g) = -2 * cf * apply(d, 1, sum)

    gr = apply(g, 2, sum)
    return(list(fun=sum(cf^2), grad=gr))
}

# Minimize the design variance
for (itr in 1:1000) {

    r = va_fun_grad(x)

    f0 = r$fun

    # Get the search direction
    d = r$grad
    d = d - sum(d * x) * x / sum(x * x)

    # Try to find a downhill step in the search direction
    lam = 0.1
    success = FALSE
    for (il in 1:50) {
        u = x - lam*d
        u = sqrt(n) * u / sqrt(sum(u**2))
        f = va_fun(u)
        if (f < f0) {
            x = u
            success = TRUE
            break
        }
        lam = lam / 2
    }

    if (!success) {
        cat(sprintf("Line search failed at iteration %d\n", itr))
        break
    }
}

# Now check to see
cat(sprintf("Variance under least squares: %f\n", 1/n))
f = n * (n - 1) / 2
v = va_fun(x)
cat(sprintf("Variance of beta_tilde: %f\n", v / f^2))

# Compare the OLS coefficients to the beta_tilde
# coefficients
m_ols = x / n
m_tilde = estimator_coef(x) / f
cat("Difference between OLS coefficients and beta_tilde coefficients:\n")
print(m_ols - m_tilde)
