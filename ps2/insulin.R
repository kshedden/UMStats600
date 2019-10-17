# Construct Z-scores for a sequence of contrasts in a functional
# regression.  The regression function is E[insulin | age, sex, bmi].
# The first two contrasts are:
#
#  C(a, s) = E[insulin | age=a, sex=s, bmi=30] - E[insulin | age=a, sex=s, bmi=25]
#
# where s = female (i) and male (ii), and a is a fixed age.
#
# The third contrast is the difference between contrast (i) and contrast (ii).

library(splines)
library(haven)
library(dplyr)
library(tidyr)
library(ggplot2)

# Read all the data files
files = c("DEMO_I.XPT", "BMX_I.XPT", "INS_I.XPT")
das = list()
for (f in files) {
    dx = read_xpt(f)
    das[[length(das)+1]] = dx
}

# Merge the three data sets
dx = left_join(das[[1]], das[[2]])
dx = left_join(dx, das[[3]])

# Drop the variables that we don't need
dx = select(dx, SEQN, RIDAGEYR, RIAGENDR, BMXBMI, LBXIN)

# Drop any row with missing values
dx = drop_na(dx)

# Create a log-transformed version of the insulin variable
dx = mutate(dx, log_LBXIN=log(LBXIN))

dx$RIAGENDR = as.factor(dx$RIAGENDR)

dx$wgt = 1

pr = NULL

# Loop over three nul hypotheses
for (hyp in 1:3) {

    da = dx[1:200, ]
    a = seq(18, 80, length.out=100)
    da$RIDAGEYR = c(a, a)
    da$BMXBMI[1:100] = 30
    da$BMXBMI[101:200] = 25
    da$wgt = 0 # Don't use these rows when fitting the model

    if (hyp == 1) {
        da$RIAGENDR = 2 # Code for female
    } else if (hyp == 2) {
        da$RIAGENDR = 1 # Code for male
    } else {
        da1 = da
        da1$RIAGENDR = 2 # Code for female
        da2 = da
        da2$RIAGENDR = 1 # Code for male
        da = rbind(da1, da2)
    }
    da$wgt = 0 # Don't use these rows when fitting the model

    dz = rbind(dx, da)

    result = lm(log_LBXIN ~ RIAGENDR*(bs(RIDAGEYR, 5) + bs(BMXBMI, 5)) + RIAGENDR:RIDAGEYR:BMXBMI,
                weight=wgt, data=dz)

    # The parameters
    pa = coef(result)

    # The covariance matrix of the parameters
    cm = vcov(result)

    # Get the contrasts
    mm = model.matrix(result)
    n = dim(dx)[1]
    if (hyp < 3) {
        ct = mm[(n+1):(n+200),]
        ct = ct[1:100,] - ct[101:200,]
    } else {
        ct = mm[(n+1):(n+400),]
        ct = ct[1:100,] - ct[101:200,] - (ct[201:300,] - ct[301:400,])
    }

    # The numerator of the Z-scores
    znum = ct %*% pa

    # The denominator of the Z-scores
    zdenom = sqrt(diag(ct %*% cm %*% t(ct)))

    zscores = znum / zdenom

    dp = data.frame(age=a, zscores=zscores, hyp=hyp)
    pr = rbind(pr, dp)
}

pr$hyp = as.factor(pr$hyp)
plt = ggplot(data=pr, aes(x=age, y=zscores, group=hyp))
plt = plt + xlab("Age")
plt = plt + ylab("Z-score")
plt = plt + geom_line(aes(color=hyp))
ggsave("insulin.pdf", plt)
