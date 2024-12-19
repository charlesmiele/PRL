library(ggplot2)

data <- read.csv("data_collection/Sarbanes_Oxley_Causal_Data.csv", header = TRUE, stringsAsFactors = FALSE)

# Convert SOX_Implementation to a factor with Pre-SOX as baseline
data$SOX_Implementation <- factor(data$SOX_Implementation, levels = c("Pre-SOX", "Post-SOX"))

# If Date is a year, make sure it's numeric
data$Date <- as.numeric(data$Date)

# Check summary
summary(data)

# Plot IPO_Count over time, color by SOX_Implementation
ggplot(data, aes(x=Date, y=IPO_Count, color=SOX_Implementation)) +
  geom_point() +
  geom_line() +
  theme_minimal() +
  labs(title="IPO Count Over Time by SOX Period",
       x="Year",
       y="Number of IPOs")

# Model 1: Just SOX dummy
model1 <- lm(IPO_Count ~ SOX_Implementation, data=data)
summary(model1)

# Model 2: Add controls (GDP_Growth, Interest_Rate, etc.)
# You might consider transformations (log) or lags, depending on theory.
model2 <- lm(IPO_Count ~ SOX_Implementation + GDP_Growth + Interest_Rate + NASDAQ_Return_1.yr.lag, data=data)
summary(model2)

t.test(IPO_Count ~ SOX_Implementation, data=data)
