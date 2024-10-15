
calculate_probs <- function(y, mu0, mu1, var0, var1, alpha){
  alpha*dnorm(y, mean = mu1, sd = sqrt(var1))/(
    alpha*dnorm(y, mean = mu1, sd = sqrt(var1)) + 
      (1 - alpha)*dnorm(y, mean = mu0, sd = sqrt(var0))
  )
}

update_mu0 <- function(y, probs){
  sum(y*(1 - probs))/sum(1-probs)
}

update_mu1 <- function(y, probs){
  sum(y*probs)/sum(probs)
}

update_alpha <- function(probs){
  sum(probs)/length(probs)
}

update_var0 <- function(y, mu0, probs){
  sum((y - mu0)^2 * (1-probs))/sum(1-probs)
}

update_var1 <- function(y, mu1, probs){
  sum((y - mu1)^2 * probs)/sum(probs)
}


z <- rbinom(1000, 1, 0.3)
y <- rnorm(1000)*(1 - z) + rnorm(1000, mean=4)*z

mu0 <- 0
mu1 <- 1
var0 = 0.5
var1 = 0.5
alpha = 0.5

for(i in 1:100){
  probs <- calculate_probs(y, mu0, mu1, var0, var1, alpha)
  
  alpha = update_alpha(probs)
  mu0 = update_mu0(y, probs)
  mu1 = update_mu1(y, probs)
  var0 = update_var0(y, mu0, probs)
  var1 = update_var1(y, mu1, probs)
  
  #print(c(alpha, mu0, mu1, var0, var1))
}

print(c(alpha, mu0, mu1, var0, var1))
