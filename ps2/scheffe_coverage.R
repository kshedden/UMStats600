# Estimate the actual coverage probabilty of a Scheffe confidence
# band using simulation.

library(splines)

# Sample size of actual data
n = 100

# Number of simulation replications
nrep = 500

# Degrees of freedom for time splines
tdf = 3

# Actual covariate data
time = rnorm(n)
xv = rnorm(n)

# Extend to include data for estimating the mean function at x=0,
# for 0 <= t <= 1.
time = append(time, seq(0, 1, length.out=100))
xv = append(xv, array(0, 100))

# Use weights to exclude the last 100 cases (the data used for
# estimating the mean function) when fitting the model.
wgt = array(1, n+100)
wgt[(n+1):(n+100)] = 0

# The design matrix for the time effects
tm = bs(time, tdf)

# Get the Scheffe multiplier
q = tdf + 1
p = tdf + 2
smf = sqrt(q * qf(0.95, q, n - p))

# The fraction of intervals that simultaneously cover their target
cover_frac = 0

for (rp in 1:nrep) {

    # Simulate the response data (response values for the last 100
    # cases are not used)
    Ey = tm %*% seq(-1, 1, length.out=tdf)
    y = Ey + rnorm(n + 100)

    # Fit the model
    md = lm(y ~ xv + bs(time, tdf), weights=wgt)

    # The design matrix after expanding the formula
    mm = model.matrix(md)

    # Check that the spline matrix is as expected
    if (dim(mm)[2] != p) {
        stop("The dimension parameter is incorrect")
    }
    mt = mm[, grepl("time", colnames(mm))]
    if (max(abs(mt - tm)) > 1e-6) {
        stop("The model is not set up correctly")
    }

    # Covariance matrix of the parameter estimates
    cm = vcov(md)

    # Parameter estimates
    cf = coef(md)

    # Fitted values (we only need the last 100 values)
    fitval = md$fitted.values

    # Estimate of the unexplained
    sig = summary(md)$sigma

    # Check for coverage on a finite grid of time points
    cover = TRUE
    for (i in 1:100) {

        # The design effect (v_theta in the notes) time sigma_hat
        theta = mm[n + i,]
        de = sqrt(theta %*% cm %*% theta)

        target = Ey[n + i]
        estimate = fitval[n + i]

        # Check if the interval covers the target at one time point
        if ((estimate < target - smf*de) || (estimate > target + smf*de)) {
            cover = FALSE
        }
    }

    cover_frac = cover_frac + cover
}

cover_frac = cover_frac / nrep
