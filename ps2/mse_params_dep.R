# Find evidence that sigma_hat and beta_hat can be dependent in a
# linear model where the errors are not Gaussian.  While it is
# possible to do this by looking at the correlation coefficient
# between sigma_hat and beta_hat, since we are focusing on dependence
# not correlation here, it is also possible to use transformations f,
# g so that the correlation between f(beta_hat) and g(sigma_hat) is
# enhanced.  Doing this makes it easier to confirm that the
# correlations we are seeing are indicative of the underlying
# population correlation coefficient being nonzero.

# Sample size
n = 100

# Number of covariates
p = 3

# Number of simulation replications
nrep = 10000

# Population slope parameters
slopes = seq(-1, 1, length.out=p)

# Design matrix
xm = array(rnorm(n*p), c(n, p))

# Expected values of the response variable
Ey = xm %*% slopes

rd = NULL
for (i in 1:nrep) {

    # Response data values (simulated using the model)
    y = Ey + rt(n, df=4)

    # Fit the model using OLS
    model = lm(y ~ xm)

    # The usual estimate of the error standard deviation
    sigma = summary(model)$sigma

    # One parameter to focus on
    cf = coef(model)[2]

    rd = rbind(rd, c(cf, sigma))
}

rd = data.frame(rd)
colnames(rd) = c("beta", "sigma")

# The standard correlation
print(cor(rd$beta-slopes[1], rd$sigma))

# The correlation among transformed values
print(cor(abs(rd$beta-slopes[1]), rd$sigma))
