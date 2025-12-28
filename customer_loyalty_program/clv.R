#CLV prediction
cust_purc_2019_clean$retention_prob <- 1 - cust_purc_2019_clean$churn_prob
#CLV prediction at the individual level
cust_purc_2019_clean$est_clv <- (cust_purc_2019_clean$retention_prob * 
  cust_purc_2019_clean$TotalSpend) - 0.01 * cust_purc_2019_clean$TotalSpend
cust_purc_2019_clean <- cust_purc_2019_clean %>% 
  rename(clv_est_2020 = est_clv)

unique_members_reg <- unique_members_2019 %>%
  mutate(
    Gender = as.factor(Gender),
    Race = as.factor(Race),
    OwnCar = as.factor(OwnCar),
    OwnCreditCard = as.factor(OwnCreditCard)
  )

spend_model <- lm(TotalSpend ~ Race + freq + NetPromoter, data = unique_members_reg)
summary(spend_model)

names(unique_members_2019)