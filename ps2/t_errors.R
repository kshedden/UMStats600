# Calculate the variance of sigma_hat^2 when the errors
# follow a t-distribution.

# Calculate the E[t^4] - E[t^2]^2, where
# t is a Student-t random variable with df
# degrees of freedom.
vs = function(df) {

    # Log of the 4th moment
    v1 = lgamma(5 / 2)
    v1 = v1 + lgamma((df - 4) / 2)
    v1 = v1 + 2 * log(df)
    v1 = v1 - lgamma(df / 2)
    v1 = v1 - log(pi) / 2

    # Log of the square of the second moment
    v2 = 2 * log(df / (df - 2))

    return(exp(v1) - exp(v2))
}

# The number of cases in the data set
n = 30

# The number of variables in the data set
p = 5

# The residual degrees of freedom
qd = n - p

# Generate a design matrix
xm = rnorm(n*qd)
xm = array(xm, c(n, qd))

# Get the projection onto the columnspace of the design matrix.
u = svd(xm)$u
proj = u %*% t(u)
s1 = sum(diag(proj)^2)
s2 = (sum(proj^2) - s1) / 2

ra = NULL

# Loop over these values for the degrees of freedom of the error distribution.
for (df in 5:100) {

    # The variance of sigma_hat^2
    tm = vs(df)
    va = s1 * vs(df) + 4 * s2 * (df / (df - 2))^2
    va = va / (n - p)^2

    ra = rbind(ra, c(df, va))
}

plot(ra[,1], ra[,2], xlab="Degrees of freedom", ylab="Var sigma_hat^2")
