#select base variables for clustering
install.packages("cluster")
library(cluster)
k_cluster_vars <- cust_purc_2019_clean %>%
  select(OwnCar, Age, Sat_Program, HomeCity)
k_cluster_vars <- k_cluster_vars %>%
  mutate(across(everything(), ~ as.factor(.)))
k_cluster_vars_num <- model.matrix(~ . - 1, data = k_cluster_vars)

kmeans_result <- kmeans(k_cluster_vars_num, centers = 4, nstart = 25)
k_clustered <- k_cluster_vars_num
k_clustered$Cluster <- as.factor(kmeans_result$cluster)

pca_result <- prcomp(k_cluster_vars_num, scale. = FALSE)
pca_df <- as.data.frame(pca_result$x[, 1:2])
pca_df$Cluster <- k_clustered$Cluster

library(ggplot2)

ggplot(pca_df, aes(x = PC1, y = PC2, color = Cluster)) +
  geom_point(alpha = 0.6, size = 1.5) +
  labs(
    title = "K-Means Clustering (PCA Projection)",
    x = "Principal Component 1",
    y = "Principal Component 2"
  ) +
  theme_minimal() +
  scale_color_brewer(palette = "Set1")

  aggregate(k_cluster_vars_num, by = list(Cluster = kmeans_result$cluster), mean)
  
  customer_clusters <- data.frame(
    MemberID = cust_purc_2019_clean$MemberID,
    Cluster = as.factor(kmeans_result$cluster)
  )
  
  ##-- sorted into clusters --
  cust_unique_2019 <- cust_purc_2019_clean %>%
    group_by(MemberID) %>%
    slice(1) %>%
    ungroup()
  
  cust_2019_with_clusters <- cust_unique_2019 %>%
    left_join(customer_clusters, by = "MemberID")
  
  unique_members <- cust_2019_with_clusters %>%
    group_by(MemberID) %>%
    slice(1) %>%  
    ungroup()
  
  names(unique_members)
  
  purchase_share <- cust_2019_with_clusters %>%
    group_by(Cluster, SalesFirm) %>%
    summarise(TotalValue = sum(SalesAmt), .groups = "drop") %>%
    group_by(Cluster) %>%
    mutate(
      ClusterTotal = sum(TotalValue),
      PercentByFirm = round(100 * TotalValue / ClusterTotal, 1)
    ) %>%
    arrange(Cluster, desc(PercentByFirm))
  
  
  
  cluster_averages <- unique_members %>%
    group_by(Cluster) %>%
    summarise(
      Count = n(),  
      Avg_Age = mean(Age, na.rm = TRUE),
      Avg_TotalSpend = mean(TotalSpend, na.rm = TRUE),
      Avg_Recency_days = mean(Recency_days, na.rm = TRUE),
      Avg_freq = mean(freq, na.rm = TRUE),
      Avg_AvgSpend = mean(AvgSpend, na.rm = TRUE),
      Avg_churn_prob = mean(churn_prob, na.rm = TRUE),
      Avg_clv_est_2020 = mean(clv_est_2020, na.rm = TRUE),
      Avg_retention_prob = mean(retention_prob, na.rm = TRUE),
      Avg_RegisterYear = mean(RegisterYear, na.rm = TRUE)
    )
  redeem_2019 <- data_red %>% 
    filter(year(RedeemDate) == 2019)
  
  redeem_summary <- redeem_2019 %>%
    group_by(MemberID) %>%
    summarise(
      TotalRedeemPoints = sum(RedeemPoint, na.rm = TRUE),
      RedeemCount = n(),
      .groups = "drop"
    )
  install.packages("tidyr")
  library(tidyr)
  
  unique_members_2019 <- unique_members %>%
    left_join(redeem_summary, by = "MemberID") %>%
    mutate(
      TotalRedeemPoints = replace_na(TotalRedeemPoints, 0),
      RedeemCount = replace_na(RedeemCount, 0)
    )
  
  cluster_sats <- unique_members_2019 %>% 
    group_by(Cluster) %>% 
    summarise(
      Count = n(),
      Avg_Sat_Program = mean(Sat_Program, na.rm = TRUE),
      Avg_Sat_Grocery = mean(Sat_Grocery, na.rm = TRUE),
      Avg_Sat_FastFood = mean(Sat_FastFood, na.rm = TRUE),
      Avg_Sat_Petrol = mean(Sat_Petrol, na.rm = TRUE),
      Avg_Total_Redeem = mean(TotalRedeemPoints, na.rm = TRUE)
    )
  
  count_percent <- function(x) {
    tbl <- table(x)
    pct <- round(100 * prop.table(tbl), 1)
    result <- paste0(names(tbl), ": ", as.integer(tbl), " (", pct, "%)")
    return(result)
  }
  
  unique_members_2019 %>%
    pivot_longer(cols = c(Gender, Race, OwnCar, OwnCreditCard, HomeCity),
                 names_to = "Demographic", values_to = "Value") %>%
    count(Cluster, Demographic, Value) %>%
    group_by(Cluster, Demographic) %>%
    mutate(Percent = round(n / sum(n) * 100, 1)) %>%
    arrange(Cluster, Demographic, desc(Percent)) %>% 
    print(n = 61)
  
  nps_summary <- unique_members_2019 %>%
    group_by(Cluster) %>%
    summarise(
      AvgNPS = round(mean(NetPromoter, na.rm = TRUE), 1),
      Count = n(),  # Optional: number of records contributing to NPS
      .groups = "drop"
    ) %>%
    arrange(Cluster, desc(AvgNPS))

names(unique_members_2019)

usable_vars <- unique_members_2019 %>% 
  select(c(Age, OwnCar, OwnCreditCard, Gender, SalesFirm, NetPromoter,
           RedeemCount, RegisterYear, Race, Sat_Grocery, Sat_FastFood, 
           Sat_Program, NetPromoter, Sat_FastFood))

names(usable_vars)






