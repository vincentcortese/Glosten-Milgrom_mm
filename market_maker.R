library(quantmod)
library(alphavantager)

# Creating the Myopically optimizing market maker - looking to implement the math
# Then move to python once we can get something like this running

# Potentially look to test this on stocks

# av_api_key("UQV2D45VFXJGX00J")
# aapl <- as.data.frame(av_get(symbol = "AAPL", av_fun = "TIME_SERIES_INTRADAY",
#                              interval = "1min", outputsize = "compact"))

setwd("C:/Users/Vincent/Documents/Bitkub/Market Maker")
ba <- read.csv(file = 'BA.csv')
# This data is actually really precise, link:
# https://finance.yahoo.com/quote/BA/history?period1=1272646800&period2=1272906000&interval=1d&filter=history&frequency=1d

# We can test this with live data from AAPL and trade accordingly
# Use BA data instead, shows alot of information 

# The total volume
sum(ba$Volume, na.rm = T)

# Removing the unncessary data
ba <- ba[, c(-1, -2, -4, -5, -8, -11, -14)]
min(ba$Price, na.rm = T)
max(ba$Price, na.rm = T)

summary(ba$Price, na.rm = T)
sd(ba$Price, na.rm = T)

ba$Spread <- ba$Ask.Price - ba$Bid.Price
# ===================================== Initialize the numbers =============================

# First step is to set a bid and ask price at the start of the day, and determining 
# the initial true value


# Consider percentage of uninformed vs informed traders 
I <- .6
v0 <- mean(c(ba$Bid.Price[1], ba$Ask.Price[1]))

# initial normal model given the first distribution
vi <- rnorm(100, mean = v0, sd = sd(ba$Price, na.rm = T))
min(vi)
max(vi)

# So this will have to be updated to new values as we take on new values

vi <- as.numeric(format(round(vi, 2), nsmall = 2))

# =====================================================
# This is a basic example of creating the Psell and Pb formulas
# Definitely have to articulate them and make it cohesive
# Also need to work in the white noise

lst1 <- seq(from = v0 - 4*sd(ba$Price, na.rm = T), to = ba$Bid.Price[1] - 1, by = 1)

lst2 <- seq(from = ba$Bid.Price[1], to = v0 + 4*sd(ba$Price, na.rm = T), by = 1)

lst <- c(lst1, lst2)

count <- 0
for(i in 1:length(lst)){
  temp <- (I + (1 - I)*.2)*dnorm(lst[i], v0, sd(ba$Price, na.rm = T))
  count <- count + temp
}
count

count2 <- 0
for(i in 1:length(lst)){
  temp2 <- (I + (1 - I)*.2)*lst[i]*dnorm(lst[i], v0, sd(ba$Price, na.rm = T))
  count2 <- temp2 + count2
}

count2 * 1/count
ba$Bid.Price[1]


Psell <- function(I, 
                  v0, 
                  sd, 
                  Bid_Price,
                  trade_prob){
  lst1 <- seq(from = v0 - 4*sd, to = Bid_Price - 1, by = 1)
  lst2 <- seq(from = Bid_Price, to = v0 + 4*sd, by = 1)
  lst <- c(lst1, lst2)
  count <- 0
  for(i in 1:length(lst)){
    temp <- (I + (1 - I)*trade_prob)*dnorm(lst[i], v0, sd(ba$Price, na.rm = T))
    count <- count + temp
  }
  return(count)
}

Pb <- function(I, 
               v0, 
               sd, 
               Bid_Price,
               trade_prob){
  lst1 <- seq(from = v0 - 4*sd, to = Bid_Price - 1, by = 1)
  lst2 <- seq(from = Bid_Price, to = v0 + 4*sd, by = 1)
  lst <- c(lst1, lst2)
  count2 <- 0
  for(i in 1:length(lst)){
    temp2 <- (I + (1 - I)*trade_prob)*lst[i]*dnorm(lst[i], v0, sd(ba$Price, na.rm = T))
    count2 <- temp2 + count2
  }
  return (1/Psell(I, v0, sd, Bid_Price, trade_prob) * count2)
}

Pb(.5, 72.78, sd(ba$Price, na.rm = T), ba$Bid.Price[311], .2)


# ==================================== Bid Price Equations =====================================

# Let us implement these formulas with noise
# As we put in noise, I think the number should directly be related to how large the spreads are
P_noise_sell <- function(I, 
                  v0, 
                  sd, 
                  Bid_Price,
                  trade_prob,
                  noise_sd){
  lst1 <- seq(from = v0 - 4*sd, to = Bid_Price - 1, by = 1)
  lst2 <- seq(from = Bid_Price, to = v0 + 4*sd, by = 1)
  count <- 0
  for(i in 1:length(lst1)){
    temp <- (I*(pnorm(rnorm(1, 0, noise_sd), (Bid_Price - lst1[i]), .1)) + 
               (1 - I)*trade_prob)*dnorm(lst1[i], v0, sd(ba$Price, na.rm = T))
    count <- count + temp
  }
  count1 <- 0
  for(i in 1:length(lst2)){
    temp <- (I*(1 - pnorm(rnorm(1, 0, noise_sd), (Bid_Price - lst2[i]), .1)) + 
               (1 - I)*trade_prob)*dnorm(lst2[i], v0, sd(ba$Price, na.rm = T))
    count1 <- count1 + temp
  }
  return(count + count1)
}

P_noise_sell(.7, 72.48, sd(ba$Price, na.rm = T), ba$Bid.Price[1], .2, .1)

P_noise_b <- function(I, 
               v0, 
               sd, 
               Bid_Price,
               trade_prob, 
               noise_sd){
  lst1 <- seq(from = v0 - 4*sd, to = Bid_Price - 1, by = 1)
  lst2 <- seq(from = Bid_Price, to = v0 + 4*sd, by = 1)
  lst <- c(lst1, lst2)
  count <- 0
  for(i in 1:length(lst1)){
    temp <- (I*(pnorm(rnorm(1, 0, noise_sd), (Bid_Price - lst1[i]), .1)) + 
               (1 - I)*trade_prob)*lst1[i]* dnorm(lst1[i], v0, sd(ba$Price, na.rm = T))
    count <- count + temp
  }
  count1 <- 0
  for(i in 1:length(lst2)){
    temp <- (I*(1 - pnorm(rnorm(1, 0, noise_sd), (Bid_Price - lst2[i]), .1)) + 
               (1 - I)*trade_prob)*lst2[i]*dnorm(lst2[i], v0, sd(ba$Price, na.rm = T))
    count1 <- count1 + temp
  }
  return (1/P_noise_sell(I, v0, sd, Bid_Price, trade_prob, noise_sd) * (count + count1))
}

P_noise_b(I = .2, 
          v0 = 72.48, 
          sd = sd(ba$Price, na.rm = T), 
          Bid_Price = ba$Bid.Price[1], 
          trade_prob = .5, 
          noise_sd = .0001)

ba$Bid.Price[1]


# ====================================== buy/ask with noise ===========================

P_noise_buy <- function(I, 
                         v0, 
                         sd, 
                         Ask_Price,
                         trade_prob,
                         noise_sd){
  lst1 <- seq(from = v0 - 4*sd, to = Ask_Price, by = 1)
  lst2 <- seq(from = Ask_Price + 1, to = v0 + 4*sd, by = 1)
  lst <- c(lst1, lst2)
  count <- 0
  for(i in 1:length(lst1)){
    temp <- (I*(1 - pnorm(rnorm(1, 0, noise_sd), (Ask_Price - lst1[i]), .1)) + 
               (1 - I)*trade_prob)*dnorm(lst1[i], v0, sd(ba$Price, na.rm = T))
    count <- count + temp
  }
  count1 <- 0
  for(i in 1:length(lst2)){
    temp <- (I*(pnorm(rnorm(1, 0, noise_sd), (Ask_Price - lst2[i]), .1)) + 
               (1 - I)*trade_prob)*dnorm(lst2[i], v0, sd(ba$Price, na.rm = T))
    count1 <- count1 + temp
  }
  return(count + count1)
}

P_noise_buy(.7, 72.48, sd(ba$Price, na.rm = T), ba$Ask.Price[1], .2, .1)

P_noise_ask <- function(I, 
                      v0, 
                      sd, 
                      Ask_Price,
                      trade_prob, 
                      noise_sd){
  lst1 <- seq(from = v0 - 4*sd, to = Ask_Price, by = 1)
  lst2 <- seq(from = Ask_Price + 1, to = v0 + 4*sd, by = 1)
  lst <- c(lst1, lst2)
  count <- 0
  for(i in 1:length(lst1)){
    temp <- (I*(1 - pnorm(rnorm(1, 0, noise_sd), (Ask_Price - lst1[i]), .1)) + 
               (1 - I)*trade_prob)*lst1[i]* dnorm(lst1[i], v0, sd(ba$Price, na.rm = T))
    count <- count + temp
  }
  count1 <- 0
  for(i in 1:length(lst2)){
    temp <- (I*(pnorm(rnorm(1, 0, noise_sd), (Ask_Price - lst2[i]), .1)) + 
               (1 - I)*trade_prob)*lst2[i]*dnorm(lst2[i], v0, sd(ba$Price, na.rm = T))
    count1 <- count1 + temp
  }
  return (1/P_noise_buy(I, v0, sd, Ask_Price, trade_prob, noise_sd) * (count + count1))
}

P_noise_ask(I = .5, 
          v0 = 72.5, 
          sd = sd(ba$Price, na.rm = T), 
          Ask_Price = ba$Ask.Price[1], 
          trade_prob = .2, 
          noise_sd = .001)

ba$Ask.Price[1]


# ============================== Probability of no orders =======================
no_order1 <- function(I, v0, Bid_Price, trade_prob, noise_sd){
  (1 - I)*(1 - 2*trade_prob) + I *(pnorm(rnorm(1, 0, noise_sd), (Bid_Price - v0), 
                                         .1, lower.tail = F)) 
}

# Potentially shift the places of the rnorm and price differences.
no_order2 <- function(I, v0, Bid_Price, Ask_Price, trade_prob, noise_sd){
  x <- rnorm(1, 0, noise_sd)
  (1 - I)*(1 - 2*trade_prob) + I * (pnorm((Bid_Price - v0), x,.1) + 
                                      (pnorm((v0 - Ask_Price),x, .1)))
}
no_order3 <- function(I, v0, Ask_Price, trade_prob, noise_sd){
  (1 - I)*(1 - 2*trade_prob) + I * (1 - pnorm(rnorm(1, 0, noise_sd), (v0 - Ask_Price),.1))
}
no_order1(I = .7, v0 = 72, Bid_Price = ba$Bid.Price[1], trade_prob = .2, noise_sd = .001)
no_order2(I = .7, v0 = 72, Bid_Price = ba$Bid.Price[1], Ask_Price = ba$Ask.Price[1],
          trade_prob = .2, noise_sd = .001)
no_order3(I = .7, v0 = 72,Ask_Price = ba$Ask.Price[1],trade_prob = .2, noise_sd = .001)


# =================================== actual equation ========================
# definitely possible to loop through the list and update the prices as we do
# what conditions would we update prices, and what values would we use

# how do spreads get updated as we move through the data, how do we account for no trades
# how often do we update the true price estimate

# Every six trades, update the value to test in the equations, after a any trade, 
# bid and ask should get updated

# Creating data i want to use for the model, have our calculated bid and ask
# A column for v0
# Calculated spread

v0 <- mean(c(ba$Bid.Price[1], ba$Ask.Price[1]))
count <- 0
sd <- sd(ba$Price, na.rm = T)
lst <- seq(from = v0 - 4 * sd, to = v0 + 4*sd, by = 1)
test <- ba[c(-1,-2)]
test$calc <- NA
for(j in 1:nrow(ba)){
  if(ba$Type[j] == 'Trade'){
    v0 <- ba$Price[j]
    test$calc[j] <- v0
    lst <- seq(from = v0 - 4 * sd, to = v0 + 4*sd, by = 1)
    next
  }
  if(v0 > max(lst) || v0 < min(lst)){
    lst <- seq(from = v0 - 4 * sd, to = v0 + 4*sd, by = 1)
  }
  if(count >= 6){
    v0 <- mean(c(ba$Bid.Price[j], ba$Ask.Price[j]))
    count <- 0
  } else {
    tryCatch({
    test$bid[j] <- P_noise_b(I = .7, v0 = v0 - .1, sd = sd, 
                           Bid_Price = ba$Bid.Price[j], trade_prob = .5, noise_sd = .000025)
    test$ask[j] <- P_noise_ask(I = .7, v0 = v0, sd = sd, 
                             Ask_Price = ba$Ask.Price[j], trade_prob = .5, noise_sd = .000025)
  count <- count + 1
    }, error = function(e){})
  }
}
test$Calc_spread <- test$ask - test$bid

# ===================== p and l =======================
# in addition, we have to do a profit and loss calculation as we make each trade
# consider how much we have at the start, and how does each order affect this
# consider price we would buy and sell at, and how much we got it for
# keep track of quantity and price i bought something at






test <- ba[c(-1:-100)]
test <- test[c(-101:-nrow(test)),]
for(j in 1:100){
  test$bid[j] <- P_noise_b(I = .7, v0 = 72.4, sd = sd(ba$Price, na.rm = T), 
                           Bid_Price = ba$Bid.Price[1], trade_prob = .5, noise_sd = .000025)
  test$ask[j] <- P_noise_ask(I = .7, v0 = 72.48, sd = sd(ba$Price, na.rm = T), 
                             Ask_Price = ba$Ask.Price[1], trade_prob = .5, noise_sd = .000025)
}

test$spread <- test$ask - test$bid
max(test$spread)
min(test$spread)
