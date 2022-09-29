# Part I

time=c(1.67,2.20,2.51,3.00,2.90,4.70,7.53,14.70,27.8,37.4,
       .8,1,1.37,2.25,2.95,3.70,6.07,6.65,7.05,7.37,
       .102, .18, .2, .24, .26, .32, .32, .42, .44, .88, 
       .073, .098, .117, .135, .175, .262, .270, .350, .386, .456)
stress = c(rep(.87,10), rep(.99,10), rep(1.09,10), rep(1.18,10))

steel <- data.frame(stress, time)
write.csv(steel, "~/Documents/Teaching/sta712-f22.github.io/exams/steel.csv")


X <- cbind(1, steel$stress)
y <- steel$time

beta0 <- c(1, 0)
lambda <- function(beta){
  return(c(1/(X %*% beta)))
}

U <- function(beta){
  return(c(t(X) %*% (lambda(beta) - y)))
}

I <- function(beta){
  W <- diag(lambda(beta))^2
  return(t(X) %*% W %*% X)
}




earthquake <- read.csv("https://sta279-s22.github.io/labs/EarthquakeData.csv")

earthquake_small <- earthquake %>%
  select(Damage, age, land_surface_condition) %>%
  mutate(Damage = (Damage != 'none') + 0) %>%
  rename(Age = age,
         Surface = land_surface_condition)

write.csv(earthquake_small, "~/Documents/Teaching/sta712-f22.github.io/exams/earthquake_small.csv")


m1 <- glm(Damage ~ age*land_surface_condition, data = earthquake_small,
          family = binomial)

summary(m1)
