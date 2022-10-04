library(MASS)

set.seed(17)
n <- 500
d <- 100
y <- rnorm(n)
x <- matrix(rnorm(n*d),nrow=n)
df <- data.frame(y=y,x)

m1 <- lm(y ~ ., data = df)
summary(m1)

back_aic <- stepAIC(m1, direction = "backward",
                    trace = 0)

summary(back_aic)

# now generate new data

n <- 500
d <- 100
y <- rnorm(n)
x <- matrix(rnorm(n*d),nrow=n)
df_new <- data.frame(y=y,x)

m2 <- lm(y ~ X22 + X30 + X32 + X33 + X34 + X35 + X36 + X40 + 
           X44 + X47 + X51 + X52 + X61 + X65 + X75 + X77 + X79 + X83 + 
           X91 + X92, data = df_new)

summary(m2)
