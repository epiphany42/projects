install.packages("readxl")
library(readxl)
data.cust <- read_excel("Loyalty Program Data (1).xlsx", sheet = "customers")
data.precs <- read_excel("Loyalty Program Data (1).xlsx", sheet = "purchase records")
data.rrecs <- read_excel("Loyalty Program Data (1).xlsx", sheet = "redeem records")

install.packages("ggplot2")

# Load the library
library(ggplot2)


max(data.precs$SalesAmt)

# Create the histogram
ggplot(data.precs[data.precs$SalesAmt <= 200, ], aes(x = SalesAmt)) +
  geom_histogram(binwidth = 5, fill = "skyblue", color = "black") +
  labs(title = "Histogram of Sales Amount <= $200",
       x = "Sales Amount",
       y = "Frequency")

ggplot(data.precs[data.precs$SalesAmt > 200, ], aes(x = SalesAmt)) +
  geom_histogram(binwidth = 100, fill = "skyblue", color = "black") +
  labs(title = "Histogram of Sales Amount >$200",
       x = "Sales Amount",
       y = "Frequency") + xlim(500, NA)

summary(data.precs$SalesAmt)

summary(data.cust$HomeCity)

prop.table(table(data.cust$HomeCity))


city_freq <- table(data.cust$HomeCity)

pie(city_freq,
    main = "Home City Distribution",
    col = rainbow(length(city_freq)))

city_percent <- round(100 * city_freq / sum(city_freq), 1)
labels <- paste(names(city_freq), ": ", city_percent, "%", sep = "")
pie(city_freq, labels = labels, col = rainbow(length(city_freq)), main = "Home City Distribution")