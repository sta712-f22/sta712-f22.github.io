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

m2 <- lm(y ~ X1 + X3 + X7 + X28 + X30 + X35 + X37 + X39 + 
           X44 + X50 + X51 + X55 + X71 + X78 + X84 + X90 + X92 + X94 + 
           X98 + X99, data = df_new)

summary(m2)
