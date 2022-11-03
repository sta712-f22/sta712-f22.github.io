m1 <- zeroinfl(drinks ~ FirstYear + OffCampus + sex | 
                 FirstYear + OffCampus + sex, 
               data = wdrinks)
summary(m1)





### initialization

# initialize poisson
initial_poisson <- glm(drinks ~ FirstYear + OffCampus + sex,
                       family = poisson, data = wdrinks)

beta <- initial_poisson$coefficients
X <- model.matrix(initial_poisson)

# initialize logistic
phat <- mean(wdrinks$drinks == 0)

gamma <- c(log(phat/(1 - phat)), 0, 0, 0)

# initialize zs

alphas <- exp(X %*% gamma)/(1 + exp(X %*% gamma))

lambdas <- exp(X %*% beta)

zs <- ifelse(wdrinks$drinks > 0, 0, alphas/(alphas + exp(-lambdas)*(1 - alphas)))


for(i in 1:10){
  beta <- glm(drinks ~ FirstYear + OffCampus + sex,
              weights = 1 - zs,
              family = poisson, data = wdrinks)$coefficients
  
  wdrinks_large <- wdrinks %>%
    mutate(drinks = (drinks == 0)) %>%
    rbind(wdrinks %>% mutate(drinks = 0))
  
  gamma <- glm(drinks ~ FirstYear + OffCampus + sex, 
               weights = c(zs, 1 - zs),
               family = binomial, data = wdrinks_large)$coefficients
  
  alphas <- exp(X %*% gamma)/(1 + exp(X %*% gamma))
  
  lambdas <- exp(X %*% beta)
  
  zs <- ifelse(wdrinks$drinks > 0, 0, alphas/(alphas + exp(-lambdas)*(1 - alphas)))
  
}
