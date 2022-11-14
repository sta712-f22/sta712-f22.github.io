# diagnostics for ZIP models
library(pscl)
library(tidyverse)
library(statmod)

## fitting a positive poisson glm
## input is a poisson glm (used to specify regression formula
## and give an initial guess at the regression coefficients)

glm.pospois <- function(poisson_glm){
  x_mat <- model.matrix(poisson_glm)
  y <- poisson_glm$y
  
  y_pos <- y[y > 0]
  x_mat_pos <- x_mat[y > 0,]
  beta0 <- poisson_glm$coefficients
  
  U <- function(beta){
    l <- exp(c(x_mat_pos %*% beta))
    c(t(x_mat_pos) %*% (y_pos - l/(1 - exp(-l))))
  }
  
  I <- function(beta){
    l <- exp(c(x_mat_pos %*% beta))
    D <- diag(l * (1 - exp(-l)*(1 + l))/((1 - exp(-l))^2))
    t(x_mat_pos) %*% D %*% x_mat_pos
  }
  
  beta_old <- beta0
  for(i in 1:10){
    beta_new <- beta_old + solve(I(beta_old)) %*% U(beta_old)
    beta_old <- beta_new
  }
  
  output <- list("coefficients" = beta_new,
                 "y" = y_pos,
                 "x" = x_mat_pos)
  return(output)
  
}


## functions for quantile residual plots

## randomized quantile residuals for positive poisson data
## m = fitted positive poisson regression model
qresid_positive_poisson <- function(m){
  y <- m$y
  lambdas <- exp(m$x %*% m$coefficients)
  
  resids <- c()
  for(i in 1:length(y)){
    cdf_b <- (ppois(y[i], lambdas[i]) - ppois(0, lambdas[i]))/(1 - ppois(0, lambdas[i]))
    cdf_a <- (ppois((y[i]-1), lambdas[i]) - ppois(0, lambdas[i]))/(1 - ppois(0, lambdas[i]))
    resids[i] <- qnorm(runif(1, cdf_a, cdf_b))
  }
  return(resids)
}


## randomized quantile residuals for a ZIP model
## zip_m = fitted ZIP model from pscl
qresid_zeroinfl <- function(zip_m){
  y <- zip_m$y
  pred_probs <- predict(zip_m, type="prob")
  resids <- c()
  for(i in 1:length(y)){
    cdf_b <- sum(pred_probs[i,1:(y[i]+1)])
    if(y[i] == 0){
      cdf_a <- 0
    } else {
      cdf_a <- sum(pred_probs[i,1:y[i]])
    }
    
    resids[i] <- qnorm(runif(1, cdf_a, cdf_b))
  }
  return(resids)
}


# Now let's see how to do diagnostics for ZIP models

## Simulate some data where both the Poisson and logistic assumptions
## are met

x <- rnorm(1000)
alpha <- exp(x)/(1 + exp(x))
lambda <- exp(1 + x)
z <- rbinom(1000, 1, prob=alpha)
y <- 0*z + rpois(1000, lambda)*(1 - z)

## Fit a ZIP model and make a quantile residual plot
## Estimated coefficients are close to the true parameters
## The plot looks good

m <- zeroinfl(y ~ x | x)
summary(m)

data.frame(x = x, resids = qresid_zeroinfl(m)) %>%
  ggplot(aes(x = x, y = resids)) +
  geom_point() +
  geom_smooth() +
  theme_bw()




## Now simulate data where the Poisson assumption is violated

x <- rnorm(1000)
alpha <- exp(x)/(1 + exp(x))
lambda <- exp(1 + x + 0.5*x^2)
z <- rbinom(1000, 1, prob=alpha)
y <- 0*z + rpois(1000, lambda)*(1 - z)

## Fit a ZIP model and make a quantile residual plot
## Estimated coefficients are wrong
## The plot looks bad

m <- zeroinfl(y ~ x | x)
summary(m)

data.frame(x = x, resids = qresid_zeroinfl(m)) %>%
  ggplot(aes(x = x, y = resids)) +
  geom_point() +
  geom_smooth() +
  theme_bw()

## Ok, but how do we know it is the Poisson component, and 
## not the zero inflated component, that is wrong?
## Fit a positive poisson regression model, and assess the 
## quantile residual plot
## Quantile residual plot looks bad!

m0 <- glm(y ~ x, family = poisson)
m_pospois <- glm.pospois(m0)
m_pospois$coefficients

data.frame(x = x[y > 0], resids = qresid_positive_poisson(m_pospois)) %>%
  ggplot(aes(x = x, y = resids)) +
  geom_point() +
  geom_smooth() +
  theme_bw()




## Finally, simulate data where the logistic assumption is violated
## Quantile residual plot doesn't look right!

x <- rnorm(1000)
alpha <- exp(x^2)/(1 + exp(x^2))
lambda <- exp(1 + x)
z <- rbinom(1000, 1, prob=alpha)
y <- 0*z + rpois(1000, lambda)*(1 - z)

m <- zeroinfl(y ~ x | x)
summary(m)

data.frame(x = x, resids = qresid_zeroinfl(m)) %>%
  ggplot(aes(x = x, y = resids)) +
  geom_point() +
  geom_smooth() +
  theme_bw()

## But what about the Poisson component?

m0 <- glm(y ~ x, family = poisson)
m_pospois <- glm.pospois(m0)
m_pospois$coefficients

data.frame(x = x[y > 0], resids = qresid_positive_poisson(m_pospois)) %>%
  ggplot(aes(x = x, y = resids)) +
  geom_point() +
  geom_smooth() +
  theme_bw()

## For comparison, here is what a quantile residual plot on the
## Poisson data would look like
## The two plots look very similar
## Note also that the estimated coefficients are very close to the estimates
## for the positive Poisson model!

m2 <- glm(y[z == 0] ~ x[z == 0], family = poisson)
m2$coefficients
m_pospois$coefficients

data.frame(x = x[z == 0], resids = qresid(m2)) %>%
  ggplot(aes(x = x, y = resids)) +
  geom_point() +
  geom_smooth() +
  theme_bw()


