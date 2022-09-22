set.seed(23)

sba_small <- sba %>%
  slice_sample(n = 5000)

write_csv(sba_small, "sba_small.csv")

sba_small <- sba_small %>%
  filter(NewExist != 0) %>%
  mutate(Default = (MIS_Status == "CHGOFF"),
         Amount = log(GrAppv),
         NewBusiness = ifelse(NewExist == 1, 0, 1),
         UrbanRural = as.factor(UrbanRural)) %>%
  dplyr::select(Default, Amount, NewBusiness, UrbanRural) %>%
  drop_na()




# logodds_plot(sba_small, 12, "equal_size", "Amount", "Default", 
#              grouping = "UrbanRural",
#              reg_formula = y ~ x) +
#   labs(x = "Loan amount (US $)", color = "New \n Business?", 
#        shape = "New \n Business?")


m1 <- glm(Default ~ Amount * NewBusiness + UrbanRural, 
          data = sba_small, family = binomial)
# summary(m1)
# 
# sba_small %>%
#   mutate(pred = m1$fitted.values) %>%
#   filter(UrbanRural == 1, NewBusiness == T) %>%
#   ggplot(aes(x = Amount, y = pred)) +
#   geom_point()



sigma <- vcov(m1)
m1_coefs <- coef(m1)

(-0.847 - m1_coefs[1] - m1_coefs[3] - m1_coefs[4])/(m1_coefs[2] + m1_coefs[6])

grad <- c(-1/(m1_coefs[2] + m1_coefs[6]),
          -(-0.847 - m1_coefs[1] - m1_coefs[3] - m1_coefs[4])/(m1_coefs[2] + m1_coefs[6])^2,
          -1/(m1_coefs[2] + m1_coefs[6]),
          -1/(m1_coefs[2] + m1_coefs[6]),
          0,
          -(-0.847 - m1_coefs[1] - m1_coefs[3] - m1_coefs[4])/(m1_coefs[2] + m1_coefs[6])^2)

sqrt(t(grad) %*% sigma %*% grad)



