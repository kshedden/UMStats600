# Construct Z-scores for a sequence of contrasts in a functional
# regression.  The regression function is E[insulin | age, bmi].
# The contrasts are:
#
#  C(a) = E[insulin | age=a, bmi=b2] - E[insulin | age=a, bmi=b1]
#
# We wish to test C(a) = 0 for each a in a series of values.
# b1 and b2 are given fixed values.

library(splines)

# Sample size
n = 2000

# Simulate some actual data
age = rnorm(n)
bmi = rnorm(n)
insulin = 0.2 * bmi / (1 + age^2) + rnorm(n)
da = data.frame(insulin=insulin, age=age, bmi=bmi, w=1)

# The fake data used for testing only
a = seq(-1, 1, length.out=100)
age = c(a, a)
b = array(1, 100)
bmi = c(-b, b)
db = data.frame(insulin=0, age=age, bmi=bmi, w=0)

# Append the actual and fake data
da = rbind(da, db)

# Fit a model, using weights so that the fake data is ignored
# in the fitting
mo = lm(insulin ~ bs(age, 4) * bs(bmi, 4), weights=w, data=da)

# Get the design matrices for the two parts of fake data that
# we want to contrast
dmat = model.matrix(mo)
dmat1 = dmat[(n+1):(n+100), ] # bmi=-1
dmat2 = dmat[(n+101):(n+200), ] # bmi=1
dmd = dmat1 - dmat2

# The contrasts we are testing
znum = dmd %*% coef(mo)

# Get the sampling variance/covariance matrix of the parameters
vc = vcov(mo)

# Get the standard deviations of the contrasts.
va = dmd %*% vc %*% t(dmd)
se = sqrt(diag(va))

# The Z-scores for the contrasts
zscore = znum / se
