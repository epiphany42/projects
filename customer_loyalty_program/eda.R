install.packages("readxl")
library(readxl)
data_cust <- read_excel("Loyalty Program Data (1).xlsx", sheet = 'customers')
data_purc <- read_excel("Loyalty Program Data (1).xlsx", sheet = 'purchase records')
data_red <- read_excel("Loyalty Program Data (1).xlsx", sheet = 'redeem records')

-- #EDA -- perform exploratory data analysis
#check structure
str(data_cust)
str(data_purc)
str(data_red)

head(data_cust)
head(data_purc)
head(data_red)

tail(data_cust)
tail(data_purc)
tail(data_red)

names(data_cust)
names(data_purc)
names(data_red)

#check missing values
colSums(is.na(data_cust))
colSums(is.na(data_purc))
colSums(is.na(data_red)) #no missing values

#check birth year
install.packages("ggplot2")
install.packages("dplyr")
library(ggplot2)
library(dplyr)

#summary statistics of key variables
install.packages("psych")
library(psych)

#numeric variables
data_cust <- data_cust %>% 
  filter(1925 <= BirthYear) #remove rows where participant's age is not known
data_cust$Age <- 2025 - data_cust$BirthYear
describe(data_cust$Age)
describe(data_cust$Sat_Program)
describe(data_cust$Sat_FastFood)
describe(data_cust$Sat_Grocery)
describe(data_cust$Sat_Petrol)
describe(data_cust$NetPromoter)

describe(data_purc$SalesAmt) 
describe(data_red$RedeemPoint)

#categorical variables
table(data_cust$HomeCity)
prop.table(table(data_cust$HomeCity)) * 100

table(data_purc$SalesFirm)
prop.table(table(data_purc$SalesFirm)) * 100

table(data_cust$Gender)
prop.table(table(data_cust$Gender)) * 100

table(data_cust$Race)
prop.table(table(data_cust$Race)) * 100

table(data_cust$OwnCar)
prop.table(table(data_cust$OwnCar)) * 100

table(data_cust$OwnCreditCard)
prop.table(table(data_cust$OwnCreditCard)) * 100

table(data_cust$Active2019)
prop.table(table(data_cust$Active2019)) * 100

pie_with_percent <- function(x, title) {
  counts <- table(x)
  pct <- round(100 * counts / sum(counts), 1)
  labels <- paste(names(counts), "\n", pct, "%")
  
  pie(counts,
      labels = labels,
      main = title,
      col = rainbow(length(counts)),
      clockwise = TRUE)
}


homecity_counts <- table(data_cust$HomeCity)
pie_with_percent(data_cust$HomeCity, "Customer Distribution by Home City")

race_counts <- table(data_cust$Race)
pie_with_percent(data_cust$Race, "Customer Distribution by Race")

salesfirm_counts <- table(data_purc$SalesFirm)
pie_with_percent(data_purc$SalesFirm, "Purchase Distribution by Sales Firm")

creditcard_counts <- table(data_cust$OwnCreditCard)
pie_with_percent(data_cust$OwnCreditCard, "Credit Card Ownership")

#time variables
install.packages("lubridate")
library(lubridate)

data_purc$SalesDate <- ymd_hms(data_purc$SalesDate)
data_purc$Year <- year(data_purc$SalesDate)
data_purc$Month <- month(data_purc$SalesDate, label = TRUE)
data_purc$Day <- day(data_purc$SalesDate)
data_purc$Hour <- hour(data_purc$SalesDate)
data_purc$Weekday <- wday(data_purc$SalesDate, label = TRUE)

install.packages("ggplot2")
library(ggplot2)
data_purc %>%
  group_by(SalesFirm) %>%
  summarise(TotalPurchases = n())
ggplot(data_purc, aes(x = SalesFirm)) +
  geom_bar(fill = "darkgreen") +
  labs(title = "Number of Purchases by Type", x = "Purchase Type", y = "Count")

#correlation matrices
install.packages("corrplot")
library(corrplot)

col <- colorRampPalette(c("cyan", "white", "red"))(200)
numeric_data <- data_cust[sapply(data_cust, is.numeric)]
numeric_data$MemberID <- NULL
cor_matrix <- cor(numeric_data, use = "pairwise.complete.obs")
corrplot(cor_matrix, method = "color", addCoef.col = "black", number.cex = 0.7, col = col)

#histograms
#distribution of customer age
hist(data_cust$Age,
     main = "Distribution of Customer Age",
     xlab = "Age",
     col = "skyblue",
     border = "white",
     breaks = 20,
     xlim = c(0, 100))

#sales amount
ggplot(data_purc[data_purc$SalesAmt <= 200, ], aes(x = SalesAmt)) +
  geom_histogram(binwidth = 5, fill = "skyblue", color = "black") +
  labs(title = "Histogram of Sales Amount <= $200",
       x = "Sales Amount",
       y = "Frequency")

ggplot(data_purc[data_purc$SalesAmt > 200, ], aes(x = SalesAmt)) +
  geom_histogram(binwidth = 100, fill = "skyblue", color = "black") +
  labs(title = "Histogram of Sales Amount >$200",
       x = "Sales Amount",
       y = "Frequency") + xlim(200, NA)