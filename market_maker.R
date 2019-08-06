# library(quantmod)
# library(alphavantager)
# av_api_key("UQV2D45VFXJGX00J")
# aapl <- as.data.frame(av_get(symbol = "AAPL", av_fun = "TIME_SERIES_INTRADAY",
#                              interval = "1min", outputsize = "compact"))

setwd("C:/Users/Vincent/Documents/Bitkub/glosten-milgrom_mm")
ba <- read.csv(file = 'BA.csv')
# This data is actually really precise, link:
# https://finance.yahoo.com/quote/BA/history?period1=1272646800&period2=1272906000&interval=1d&filter=history&frequency=1d

# The total volume
sum(ba$Volume, na.rm = T)

# Removing the unncessary data
ba <- ba[, c(-1, -2, -4, -5, -8, -11, -14)]

summary(ba$Price, na.rm = T)

ba$Spread <- ba$Ask.Price - ba$Bid.Price

# ==================================== Bid Price Equations =====================================

# Let us implement these formulas with noise
# As we put in noise, I think the number should directly be related to how large the spreads are
P_noise_sell <- function(I, 
                  v0, 
                  sd, 
                  Bid_Price,
                  trade_prob,
                  noise_sd){
  if(Bid_Price > v0 + 4 * sd || Bid_Price < v0 - 4*sd){
    v0 <- Bid_Price
  }
  x <- rnorm(1, 0, noise_sd)
  lst1 <- seq(from = v0 - 4*sd, to = Bid_Price - 1, by = sd)
  lst2 <- seq(from = Bid_Price, to = v0 + 4*sd, by = sd)
  count <- 0
  for(i in 1:length(lst1)){ 
    temp <- (I*(pnorm(x, (Bid_Price - lst1[i]), .1)) + 
               (1 - I)*trade_prob)*dnorm(lst1[i], v0, sd)
    count <- count + temp
  }
  count1 <- 0
  for(i in 1:length(lst2)){
    temp <- (I*(1 - pnorm(x, (Bid_Price - lst2[i]), .1)) + 
               (1 - I)*trade_prob)*dnorm(lst2[i], v0, sd)
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
  if(Bid_Price > v0 + 4 * sd || Bid_Price < v0 - 4*sd){
    v0 <- Bid_Price
  }
  x <- rnorm(1, 0, noise_sd)
  lst1 <- seq(from = v0 - 4*sd, to = Bid_Price - 1, by = sd)
  lst2 <- seq(from = Bid_Price, to = v0 + 4*sd, by = sd)
  count <- 0
  for(i in 1:length(lst1)){
    temp <- (I*(pnorm(x, (Bid_Price - lst1[i]), .1)) + 
               (1 - I)*trade_prob)*lst1[i]* dnorm(lst1[i], v0, sd)
    count <- count + temp
  }
  count1 <- 0
  for(i in 1:length(lst2)){
    temp <- (I*(1 - pnorm(x, (Bid_Price - lst2[i]), .1)) + 
               (1 - I)*trade_prob)*lst2[i]*dnorm(lst2[i], v0, sd)
    count1 <- count1 + temp
  }
  return (1/P_noise_sell(I, v0, sd, Bid_Price, trade_prob, noise_sd) * (count + count1))
}

P_noise_b(I = .2, 
          v0 = mean(ba$Bid.Price[1], ba$Ask.Price[1]), 
          sd = sd(ba$Price, na.rm = T), 
          Bid_Price = 75, 
          trade_prob = .8, 
          noise_sd = .001)

# Reducing the amount of I, decreases spread
# v0 plays a significant role in determining the price
# increasing trade_prob, decreases spread
# 


ba$Bid.Price[1]


# ====================================== buy/ask with noise ===========================

P_noise_buy <- function(I, 
                         v0, 
                         sd, 
                         Ask_Price,
                         trade_prob,
                         noise_sd){
  if(Ask_Price > v0 + 4 * sd || Ask_Price < v0 - 4*sd){
    v0 <- Ask_Price
  }
  x <- rnorm(1, 0, noise_sd)
  lst1 <- seq(from = v0 - 4*sd, to = Ask_Price, by = sd)
  lst2 <- seq(from = Ask_Price + 1, to = v0 + 4*sd, by = sd)
  count <- 0
  for(i in 1:length(lst1)){
    temp <- (I*(1 - pnorm(x, (Ask_Price - lst1[i]), .1)) + 
               (1 - I)*trade_prob)*dnorm(lst1[i], v0, sd)
    count <- count + temp
  }
  count1 <- 0
  for(i in 1:length(lst2)){
    temp <- (I*(pnorm(x, (lst2[i] - Ask_Price), .1)) + 
               (1 - I)*trade_prob)*dnorm(lst2[i], v0, sd)
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
  if(Ask_Price > v0 + 4 * sd || Ask_Price < v0 - 4*sd){
    v0 <- Ask_Price
  }
  x <- rnorm(1, 0, noise_sd)
  lst1 <- seq(from = v0 - 4*sd, to = Ask_Price, by = sd)
  lst2 <- seq(from = Ask_Price + 1, to = v0 + 4*sd, by = sd)
  count <- 0
  for(i in 1:length(lst1)){
    temp <- ((1 - I)*trade_prob + I*(1 - pnorm(x, (Ask_Price - lst1[i]), .1))) * 
      lst1[i]* dnorm(lst1[i], v0, sd)
    count <- count + temp
  }
  count1 <- 0
  for(i in 1:length(lst2)){
    temp <- ((1 - I)*trade_prob + I*(pnorm(x, (lst2[i] - Ask_Price), .1))) * 
      lst2[i]*dnorm(lst2[i], v0, sd)
    count1 <- count1 + temp
  }
  return (1/P_noise_buy(I, v0, sd, Ask_Price, trade_prob, noise_sd) * (count + count1))
}

P_noise_ask(I = .2, 
          v0 = 73, 
          sd = sd(ba$Price, na.rm = T), 
          Ask_Price = ba$Ask.Price[1], 
          trade_prob = .2, 
          noise_sd = .01)

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
    test$calc[j] <- mean(test$bid[j - 1], test$ask[j - 1])
    lst <- seq(from = v0 - 4 * sd, to = v0 + 4*sd, by = 1)
  }
  if(v0 > max(lst) || v0 < min(lst)){
    lst <- seq(from = v0 - 4 * sd, to = v0 + 4*sd, by = 1)
  }
  if(count >= 6){
    v0 <- mean(c(test$bid[j], test$ask[j]))
    count <- 0
  } else {
    tryCatch({
    test$bid[j] <- P_noise_b(I = .3, v0 = v0 - .2, sd = sd, 
                           Bid_Price = ba$Bid.Price[j], trade_prob = .5, noise_sd = .00025)
    test$ask[j] <- P_noise_ask(I = .3, v0 = v0 + .3, sd = sd, 
                             Ask_Price = ba$Ask.Price[j], trade_prob = .5, noise_sd = .00025)
  count <- count + 1
    }, error = function(e){})
  }
}
test$Calc_spread <- test$ask - test$bid
summary(test$Calc_spread)


# This test is not working perfectly, we can create a list to 

P_noise_b(I = .3, v0 = v0, sd = sd, 
                         Bid_Price = ba$Bid.Price[1], trade_prob = .5, noise_sd = .00025)
P_noise_ask(I = .3, v0 = v0, sd = sd, 
                           Ask_Price = ba$Ask.Price[1], trade_prob = .5, noise_sd = .00025)





# ===================== p and l =======================
# in addition, we have to do a profit and loss calculation as we make each trade
# consider how much we have at the start, and how does each order affect this
# consider price we would buy and sell at, and how much we got it for
# keep track of quantity and price i bought something at






test <- ba[c(-1:-100)]
test <- test[c(-101:-nrow(test)),]
for(j in 1:100){
  tryCatch({
  test$bid[j] <- P_noise_b(I = .7, v0 = 72.4, sd = sd(ba$Price, na.rm = T), 
                           Bid_Price = ba$Bid.Price[j], trade_prob = .5, noise_sd = .000025)
  test$ask[j] <- P_noise_ask(I = .7, v0 = 73.05, sd = sd(ba$Price, na.rm = T), 
                             Ask_Price = ba$Ask.Price[j], trade_prob = .5, noise_sd = .000025)
  }, error = function(e){})
}

test$spread <- test$ask - test$bid
max(test$spread)
min(test$spread)
# ligma
(73.05 -72.4) /2
