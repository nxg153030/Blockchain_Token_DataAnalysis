# load token data
token_data <- read.table("networkzrxTX.txt")
names(token_data) <- c("From Node ID", "To Node ID", "unixTime", "token amount")
head(token_data, 5)

# remove outliers
token_data = token_data[!token_data$`token amount` > 10^27,]

# sellers and buyers
sellers = as.data.frame(table(token_data["From Node ID"]))
names(sellers) <- c("Node ID", "SellingFrequency")
head(sellers, 5)
buyers = as.data.frame(table(token_data["To Node ID"]))
names(buyers) <- c("Node ID", "BuyingFrequency")
#head(buyers, 5)


# sell frequency vs No. of sells at that frequency
library(ggplot2)
sell_counts = data.frame(table(sellers$SellingFrequency))
ggplot(sell_counts, aes(x=as.numeric(Var1), y=Freq)) + geom_bar(stat="identity") + 
  labs(x="Selling Frequency", y="number of sells at that frequency") + xlim(0,30) + geom_line(data=sell_counts,  aes(x=as.numeric(Var1), y=Freq), colour="red")


buy_counts = data.frame(table(buyers$BuyingFrequency))
ggplot(buy_counts, aes(x=as.numeric(Var1), y=Freq)) + geom_bar(stat="identity") + 
  labs(x="Buying Frequency", y="number of buys at that frequency") + 
  xlim(0,30) + geom_line(data=buy_counts,  aes(x=as.numeric(Var1), y=Freq), colour="red")

# fit distribution
library(MASS)
bp <- barplot(sell_counts$Freq, xlab="Selling Frequency", ylab="Number of sells at that frequency")
axis(1, at=bp, labels=sell_counts$Var1)
expfit = fitdistr(sellers$SellingFrequency, "exponential")$estimate
x <- rexp(nrow(sellers), rate= expfit)
curve(rexp, add = TRUE, col = "red", lwd=2)


# convert date to merge token and price data
date_converter <- function(x) anydate(x)
token_data[3] <- lapply(token_data[3], date_converter)
head(token_data,5)

# load token price data
price_data <- read.table("zrx",header=TRUE, fill=TRUE)
price_data$Cap <- NULL
colnames(price_data)[7] <- "Market Cap"
#head(price_data,5)
price_data <- transform(price_data, Date = as.Date(as.character(Date), "%m/%d/%Y"))

# merge token and price data
merged <- merge(token_data, price_data, by= "Date")
head(merged,5)


# get the number of transactions for each day
library(plyr)
datecounts = count(merged$Date)
colnames(datecounts)[1] <- "Date"
#head(datecounts,5)
merged_datecount <- merge(merged, datecounts, by  = "Date")
head(merged_datecount)


# Open price of token vs number of transactions for each day plot
dropped <- c("From Node ID","To Node ID", "token amount")
merged_trimmed <- merged_datecount[ , !(names(merged_datecount) %in% dropped)]
merged_trimmed <- merged_trimmed[!duplicated(merged_trimmed$Date),]
#head(merged_trimmed, 4)
ggplot(merged_trimmed, aes(x=Open, y=freq)) + geom_point() + geom_line(color="red") + xlab("Open Price in dollars($)") + ylab("Number of transactions")


# estimate the number of layers using the maximum transaction amount
max_t = 540141324 * 10^18
n = 0.1
for(i in 1:15){
  layer = merged[merged$`token amount` > n*max_t,]
  datecounts_layer = count(layer$Date)
  colnames(datecounts_layer)[1] <- "Date"
  layer_with_datecount <- merge(layer, datecounts_layer, by = "Date")
  dropped <- c("From Node ID","To Node ID", "token amount")
  layer_trimmed <- layer_with_datecount[ , !(names(layer_with_datecount) %in% dropped)]
  layer_trimmed <- layer_trimmed[!duplicated(layer_trimmed$Date),]
  #print("n=",n, " correlation: ", cor(layer_trimmed$Open, layer_trimmed$freq))
  cat(sprintf("n= %s correlation: %f number of transactions in the layer= %s \n", n, cor(layer_trimmed$Open, layer_trimmed$freq), nrow(layer_with_datecount)))
  n <- n/5
}

# Add new columns - prev_low, prev_open, prev_high - get the low, open and high values from day T-1 and add it as a feature of day T
# Compute price return using price return formula
library(dplyr)
sorted_prices <- price_data[order(price_data$Date),]
newprices <- mutate(sorted_prices, prev_high= lag(High), prev_low=lag(Low),prev_open=lag(Open))
newprices <- newprices[-1,]
newprices <- transform(newprices, price_return_open= (Open-prev_open)/prev_open, price_return_low=(Low-prev_low)/prev_low, price_return_high= (High-prev_high)/prev_high)
merged <- merge(token_data, newprices, by= "Date") 
#head(merged)

# merge number of transactions with token and price data
merged_datecount <- merge(merged, datecounts, by  = "Date")
dropped <- c("From Node ID","To Node ID", "token amount")
merged_trimmed <- merged_datecount[ , !(names(merged_datecount) %in% dropped)]
merged_trimmed <- merged_trimmed[!duplicated(merged_trimmed$Date),]
head(merged_trimmed, 10)

# plot linear regression model
ggplotRegression <- function (fit) {
  require(ggplot2)
  ggplot(fit$model, aes_string(x = names(fit$model)[2], y = names(fit$model)[1])) + 
    geom_point() +
    stat_smooth(method = "lm", col = "red") +
    labs(title = paste("Adj R2 = ",signif(summary(fit)$adj.r.squared, 5),
                       "Intercept =",signif(fit$coef[[1]],5 ),
                       " Slope =",signif(fit$coef[[2]], 5),
                       " P =",signif(summary(fit)$coef[2,4], 5)))
}

fit <- lm(price_return_open ~ freq, data=merged_trimmed)
ggplotRegression(fit)