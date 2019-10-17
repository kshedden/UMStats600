# Assess power for two null hypotheses:
# (i) beta_1 = 0
# (ii) beta_1 = beta_2_

# The sample size.
n = 50

# True values of the parameters
b = c(0.5, 0.25)

# Possible values of the correlation between x1 and x2.
ra = seq(-0.95, 0.95, length.out=100)

# Power for the null hypothesis that beta_1 is not zero.
power1 = 1 - pnorm(2 - sqrt(n * (1 - ra^2)) * b[1])

# Power for the null hypothesis that beta_1 differs from beta_2.
power2 = 1 - pnorm(2 - sqrt(2 * n * (1 - ra)) * (b[1] - b[2]))

