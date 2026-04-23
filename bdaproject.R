install.packages("ggplot2")
install.packages("dplyr")
install.packages("readr")
install.packages("lubridate")
library(ggplot2)
library(dplyr)
library(readr)
library(lubridate)
transactions <- read.csv("/Users/bethiikshitha/Desktop/bank_transactions_data_2.csv")
head(transactions)
str(transactions)
summary(transactions)
colnames(transactions)
dim(transactions)
colSums(is.na(transactions))
transactions <- na.omit(transactions)
dim(transactions)
head(transactions)
transactions$TransactionDate <- ymd_hms(transactions$TransactionDate)
transactions$PreviousTransactionDate <- ymd_hms(transactions$PreviousTransactionDate)
str(transactions$TransactionDate)
nrow(transactions)
mean(transactions$TransactionAmount)
max(transactions$TransactionAmount)
min(transactions$TransactionAmount)
mean(transactions$AccountBalance)
transaction_type_count <- transactions %>%
  group_by(TransactionType) %>%
  summarise(Count = n())

print(transaction_type_count)
channel_count <- transactions %>%
  group_by(Channel) %>%
  summarise(Count = n())

print(channel_count)
location_count <- transactions %>%
  group_by(Location) %>%
  summarise(Count = n()) %>%
  arrange(desc(Count))

print(location_count)
age_summary <- transactions %>%
  summarise(
    MinAge = min(CustomerAge),
    MaxAge = max(CustomerAge),
    AvgAge = mean(CustomerAge)
  )

print(age_summary)
occupation_count <- transactions %>%
  group_by(CustomerOccupation) %>%
  summarise(Count = n()) %>%
  arrange(desc(Count))

print(occupation_count)
login_attempts_summary <- transactions %>%
  summarise(
    MinAttempts = min(LoginAttempts),
    MaxAttempts = max(LoginAttempts),
    AvgAttempts = mean(LoginAttempts)
  )

print(login_attempts_summary)
duration_summary <- transactions %>%
  summarise(
    MinDuration = min(TransactionDuration),
    MaxDuration = max(TransactionDuration),
    AvgDuration = mean(TransactionDuration)
  )

print(duration_summary)
high_transactions <- transactions %>%
  filter(TransactionAmount > 1000)

head(high_transactions)
nrow(high_transactions)
top_transactions <- transactions %>%
  arrange(desc(TransactionAmount)) %>%
  head(10)

print(top_transactions)
transactions$DateOnly <- as.Date(transactions$TransactionDate)
daily_transactions <- transactions %>%
  group_by(DateOnly) %>%
  summarise(TransactionCount = n())

print(head(daily_transactions))
#1. Transaction Amount Distribution
ggplot(transactions, aes(x = TransactionAmount)) +
  geom_histogram(bins = 30, fill = "skyblue", color = "black") +
  ggtitle("Transaction Amount Distribution") +
  xlab("Transaction Amount") +
  ylab("Frequency") +
  theme_minimal()

# 2. Transaction Type Distribution
ggplot(transaction_type_count, aes(x = TransactionType, y = Count, fill = TransactionType)) +
  geom_bar(stat = "identity") +
  ggtitle("Transaction Type Distribution") +
  xlab("Transaction Type") +
  ylab("Count") +
  scale_fill_brewer(palette = "Set2") +
  theme_minimal() +
  theme(legend.position = "none")

# 3. Channel-wise Transaction Count
ggplot(channel_count, aes(x = Channel, y = Count, fill = Channel)) +
  geom_bar(stat = "identity") +
  ggtitle("Channel-wise Transaction Count") +
  xlab("Channel") +
  ylab("Count") +
  scale_fill_brewer(palette = "Pastel1") +
  theme_minimal() +
  theme(legend.position = "none")

# 4. Top 10 Locations by Transaction Count
ggplot(location_count[1:10, ], aes(x = reorder(Location, Count), y = Count, fill = Location)) +
  geom_bar(stat = "identity") +
  ggtitle("Top 10 Locations by Transaction Count") +
  xlab("Location") +
  ylab("Count") +
  coord_flip() +
  scale_fill_brewer(palette = "Set3") +
  theme_minimal() +
  theme(legend.position = "none")

# 5. Occupation-wise Transaction Count
ggplot(occupation_count, aes(x = reorder(CustomerOccupation, Count), y = Count, fill = CustomerOccupation)) +
  geom_bar(stat = "identity") +
  ggtitle("Occupation-wise Transaction Count") +
  xlab("Customer Occupation") +
  ylab("Count") +
  coord_flip() +
  scale_fill_brewer(palette = "Paired") +
  theme_minimal() +
  theme(legend.position = "none")

# 6. Daily Transaction Trend
ggplot(daily_transactions, aes(x = DateOnly, y = TransactionCount)) +
  geom_line(color = "blue", linewidth = 1) +
  ggtitle("Daily Transaction Trend") +
  xlab("Date") +
  ylab("Number of Transactions") +
  theme_minimal()

# 7. Top 10 Highest Transactions
ggplot(top_transactions, aes(x = reorder(TransactionID, TransactionAmount), y = TransactionAmount, fill = TransactionID)) +
  geom_bar(stat = "identity") +
  ggtitle("Top 10 Highest Transactions") +
  xlab("Transaction ID") +
  ylab("Transaction Amount") +
  coord_flip() +
  scale_fill_brewer(palette = "Accent") +
  theme_minimal() +
  theme(legend.position = "none")

cat("Total Transactions:", nrow(transactions), "\n")
cat("Average Transaction Amount:", mean(transactions$TransactionAmount), "\n")
cat("Maximum Transaction Amount:", max(transactions$TransactionAmount), "\n")
cat("Minimum Transaction Amount:", min(transactions$TransactionAmount), "\n")
cat("Average Account Balance:", mean(transactions$AccountBalance), "\n")
cat("Number of High-Value Transactions:", nrow(high_transactions), "\n")
cat("Most Common Transaction Type:", names(sort(table(transactions$TransactionType), decreasing = TRUE))[1], "\n")
cat("Most Common Channel:", names(sort(table(transactions$Channel), decreasing = TRUE))[1], "\n")
cat("Most Active Location:", names(sort(table(transactions$Location), decreasing = TRUE))[1], "\n")



