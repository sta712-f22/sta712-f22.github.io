library(pscl)
library(tidyverse)

# import the data
wdrinks <- read.csv("~/Documents/Teaching/sta279-s22.github.io/slides/weekendDrinks.csv")

FirstYear <- ifelse( wdrinks$dorm %in% c("mohn","kittlesby", "kildahl"), "TRUE", "FALSE")
OffCampus <- ifelse( wdrinks$dorm == "off campus", "TRUE", "FALSE")

wdrinks <- cbind(wdrinks, FirstYear, OffCampus)
wdrinks <- wdrinks |>
  dplyr::select(drinks, FirstYear, OffCampus, sex)

### ZIP model fit using the pscl package
m1 <- zeroinfl(drinks ~ FirstYear + OffCampus + sex, 
               dist = "poisson",
               data = wdrinks)
summary(m1)

### Our goal: use the EM algorithm to estimate the regression coefficients,
### instead of using the zeroinfl function

### initialization

# initialize poisson
initial_poisson <- glm(drinks ~ FirstYear + 
                         OffCampus + sex,
                       family = poisson, 
                       data = wdrinks)

beta <- initial_poisson$coefficients
X <- model.matrix(initial_poisson)

# initialize logistic
phat <- mean(wdrinks$drinks == 0)

gamma <- c(log(phat/(1- phat)), 0, 0, 0)


for(i in 1:20){
  
  # E step: guesses for zs
  
  alphas <- exp(X %*% gamma)/(1 + exp(X %*% gamma))
  
  lambdas <- exp(X %*% beta)
  
  zs <- ifelse(wdrinks$drinks > 0, 0, 
               alphas/(alphas + exp(-lambdas)*(1 - alphas)))
  
  
  # M-step: parameter estimation
  beta <- glm(drinks ~ FirstYear + OffCampus + sex,
              weights = 1 - zs,
              family = poisson, data = wdrinks)$coefficients
  
  wdrinks_large <- wdrinks %>%
    mutate(drinks = (drinks == 0)) %>%
    rbind(wdrinks %>% mutate(drinks = 0))
  
  gamma <- glm(drinks ~ FirstYear + OffCampus + sex,
               weights = c(zs, 1-zs),
               family = binomial, 
               data =wdrinks_large)$coefficients
  
}

# here are our estimated coefficients
beta
gamma

# they are very close to what we saw from the zeroinfl function!
coefficients(m1)
