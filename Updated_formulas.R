library(gdata)
# Testing conditional expectation logic
# One dimension will be the sequence of values


# Also need to create a separate matrix with white noise and the bid price - the value

price_dist <- seq(from = v0 - 4*sd, to = v0 + 4*sd, by = .01)

(price_dist - v0)[295]
price_dist <- as.numeric(format(round(price_dist, 2), nsmall = 2))
x <- rnorm(1)

setwd("C:/Users/Vincent/Documents/Bitkub/glosten-milgrom_mm")
btc <- read.csv(file = "BTCUSD.csv")

btc$spead <- btc$Ask.price - btc$Bid.price
summary(btc$spead)
summary(btc$Ask.price)

v0 <- mean(btc$Bid.price[1], btc$Ask.price[1])
sd <- sd(btc$Ask.price)
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
  lst1 <- seq(from = v0 - 4*sd, to = Bid_Price, by = .1)
  lst2 <- seq(from = Bid_Price, to = v0 + 4*sd, by = .1)
  y <- length(which(x < (Bid_Price - lst1))) / length(lst1)
  z <- length(which(x > (lst2 - Bid_Price))) / length(lst2)
  count <- 0
  for(i in 1:length(lst1)){ 
    temp <- (I*y + (1 - I)*trade_prob) * (1/length(lst1))
    count <- count + temp
  }
  count1 <- 0
  for(i in 1:length(lst2)){
    temp <- (I*z + (1 - I)*trade_prob) * (1/length(lst2))
    count1 <- count1 + temp
  }
  return(count + count1)
}

P_noise_bid <- function(I, 
                      v0, 
                      sd, 
                      Bid_Price,
                      trade_prob, 
                      noise_sd){
  if(Bid_Price > v0 + 4 * sd || Bid_Price < v0 - 4*sd){
    v0 <- Bid_Price
  }
  x <- rnorm(1, 0, noise_sd)
  lst1 <- seq(from = v0 - 4*sd, to = Bid_Price, by = .1)
  lst2 <- seq(from = Bid_Price, to = v0 + 4*sd, by = .1)
  y <- length(which(x < (Bid_Price - lst1))) / length(lst1)
  z <- length(which(x > (lst2 - Bid_Price))) / length(lst2)
  count <- 0
  for(i in 1:length(lst1)){
    temp <- (I*y + (1 - I)*trade_prob)* lst1[i] * (1/length(lst1))
    count <- count + temp
  }
  count1 <- 0
  for(i in 1:length(lst2)){
    temp <- (I*z + (1 - I)*trade_prob)*lst2[i]*(1/length(lst2))
    count1 <- count1 + temp
  }
  return (1/P_noise_sell(I, v0, sd, Bid_Price, trade_prob, noise_sd) * (count + count1))
}

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
  lst1 <- seq(from = v0 - 4*sd, to = Ask_Price, by = .1)
  lst2 <- seq(from = Ask_Price, to = v0 + 4*sd, by = .1)
  y <- length(which(x > (Ask_Price - lst1))) / length(lst1)
  z <- length(which(x < (lst2 - Ask_Price))) / length(lst2)
  count <- 0
  for(i in 1:length(lst1)){
    temp <- (I*y + (1 - I)*trade_prob)*(1/length(lst1))
    count <- count + temp
  }
  count1 <- 0
  for(i in 1:length(lst2)){
    temp <- (I*z + (1 - I)*trade_prob)*(1/length(lst2))
    count1 <- count1 + temp
  }
  return(count + count1)
}

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
  lst1 <- seq(from = v0 - 4*sd, to = Ask_Price, by = .1)
  lst2 <- seq(from = Ask_Price, to = v0 + 4*sd, by = .1)
  y <- length(which(x > (Ask_Price - lst1))) / length(lst1)
  z <- length(which(x < (lst2 - Ask_Price))) / length(lst2)
  count <- 0
  for(i in 1:length(lst1)){
    temp <- ((1 - I)*trade_prob + I*y) * lst1[i]* (1/length(lst1))
    count <- count + temp
  }
  count1 <- 0
  for(i in 1:length(lst2)){
    temp <- ((1 - I)*trade_prob + I*z) * lst2[i]* (1/length(lst2))
    count1 <- count1 + temp
  }
  return (1/P_noise_buy(I, v0, sd, Ask_Price, trade_prob, noise_sd) * (count + count1))
}


# ============================= Updated No order ===================================
no_order1 <- function(I, v0, sd, Bid_Price, trade_prob, noise_sd){
  x <- rnorm(1, 0, noise_sd)
  lst <- seq(from = v0 - 4*sd, to = Bid_Price, by = .01)
  y <- length(which(x > (Bid_Price - lst))) / length(lst)
  (1 - I)*(1 - 2*trade_prob) + I *y
}

# Potentially shift the places of the rnorm and price differences.
no_order2 <- function(I, v0, sd, Bid_Price, Ask_Price, trade_prob, noise_sd){
  x <- rnorm(1, 0, noise_sd)
  lst <- seq(from = Bid_Price, to = Ask_Price, by = 0.01)
  y <- length(which(x > (Bid_Price - lst))) / length(lst)
  z <- length(which(x > (Ask_Price - lst))) / length(lst)
  (1 - I)*(1 - 2*trade_prob) + I *(y + z)
}
no_order3 <- function(I, v0, sd, Ask_Price, trade_prob, noise_sd){
  x <- rnorm(1, 0, noise_sd)
  lst <- seq(from = Ask_Price, to = v0 + 4* sd, by = .01)
  y <- length(which(x > (Ask_Price - lst))) / length(lst)
  (1 - I)*(1 - 2*trade_prob) + I * y
}

no_order1(I = .25, v0 = 71, sd = sd, Bid_Price = 72.3, trade_prob = .5, noise_sd = .001)
no_order2(I = .25, v0 = 71, sd = sd, Bid_Price = 72.3, Ask_Price = 72.66,
          trade_prob = .5, noise_sd = .001)
no_order3(I = .25, v0 = 71, sd = sd, Ask_Price = 72.66 ,trade_prob = .5, noise_sd = .001)

P_noise_bid(I = .175, v0 = v0, sd = sd/10, 
            Bid_Price = btc$Bid.price[33618], trade_prob = .5, noise_sd = .05)
P_noise_ask(I = .175, v0 = v0, sd = sd/10, 
            Ask_Price = btc$Ask.price[33618], trade_prob = .5, noise_sd = .05)


# ===================================== Test BTC ===================================
summary(btc$Bid.price)
test <- btc[c(-1000:-nrow(btc)), ]
v0 <- mean(c(test$Bid.price[1], test$Ask.price[1]))
sd <- sd(btc$Bid.price, na.rm = T)
count <- 0
for(j in 1:nrow(test)){
  if(count >= 6){
    v0 <- mean(c(test$bid[j - 1], test$ask[j - 1]))
    count <- 0
  } else {
      test$bid[j] <- P_noise_bid(I = .175, v0 = v0, sd = sd/10, 
                               Bid_Price = btc$Ask.price[j], trade_prob = .5, noise_sd = .05)
      test$ask[j] <- P_noise_ask(I = .175, v0 = v0, sd = sd/10, 
                                 Ask_Price = btc$Bid.price[j], trade_prob = .5, noise_sd = .05)
      count <- count + 1
  }
}
test$Calc_spread <- test$ask - test$bid
summary(test$Calc_spread)

# Test P and L in the above, assume that we start with a given amount and then compare the price
# from spreads
time.vec <- seq(ISOdatetime(2019,6,23,09,01,04), ISOdatetime(2019,6,23,21,03,40), by=(60*.05))
head(time.vec)
strptime(time.vec, format = "%y-%m-%d %H:%M:%OS")
# Ideally I will create a time vec to execute trades every half second

# although I will create a basic test first for the p&l based off the initial quantity
# Assume actual price is the mean between the previous spread, and then whatever people
# buy and sell at is our gain
for(i in 1:nrow(test)){
  test$real_price[i] <- mean(c(test$Bid.price[i], test$Ask.price[i]))
  test$price[i] <- mean(c(test$bid[i], test$ask[i]))
}

# p_l_test <- function(I, v0, sd, trade_prob, noise_sd, quantity){
  # Just assume that we buy x amount of btc at time 0
  # Will just average buy and sell costs for p and l
  # just calculate unrealized and realized p and l every 6 steps
  test$quantity[1] <- quantity
  test$cost[1] <- test$quantity[1] * -test$price[1]
  count <- 0
  for(j in 2:nrow(test)){
    if(count >= 6){
      v0 <- mean(c(test$bid[j - 1], test$ask[j - 1]))
      count <- 0
      total_buy <- sum(c(test$ask_quantity[j-1],test$ask_quantity[j-2], test$ask_quantity[j-3],
                         test$ask_quantity[j-4], test$ask_quantity[j-5], test$ask_quantity[j-6]),
                       na.rm = TRUE)
      total_sell <- sum(c(test$bid_quantity[j-1],test$bid_quantity[j-2], test$bid_quantity[j-3],
                          test$bid_quantity[j-4], test$bid_quantity[j-5], test$bid_quantity[j-6]),
                        na.rm = TRUE)
      avg_cost <- mean(c(test$price[j-1],test$price[j-2], test$price[j-3],
                        test$price[j-4], test$price[j-5], test$price[j-6]),
                      na.rm = TRUE)
      buy_cost <- sum(c(test$buy_cost[j-1],test$buy_cost[j-2], test$buy_cost[j-3],
                        test$buy_cost[j-4], test$buy_cost[j-5], test$buy_cost[j-6]),
                      na.rm = TRUE)
      sell_cost <- sum(c(test$sell_cost[j-1],test$sell_cost[j-2], test$sell_cost[j-3],
                         test$sell_cost[j-4], test$sell_cost[j-5], test$sell_cost[j-6]),
                       na.rm = TRUE)
      if(is.na(total_buy) || is.na(total_sell) || is.na(buy_cost) || is.na(sell_cost)){
        next
      }
      test$p_l[j] <- (sell_cost / total_sell) - (buy_cost / total_buy)
    }
    test$bid[j] <- P_noise_bid(I = I, v0 = v0, sd = sd/10, 
                             Bid_Price = btc$Ask.price[j], trade_prob = trade_prob, 
                             noise_sd = noise_sd)
    test$ask[j] <- P_noise_ask(I = I, v0 = v0, sd = sd/10, 
                               Ask_Price = btc$Bid.price[j], trade_prob = trade_prob, 
                               noise_sd = noise_sd)
    count <- count + 1
  
      if(test$Bid.volume[j] > test$Ask.volume[j]){ #More bid prices, so we sell to the difference
        test$bid_quantity[j] <- (test$Bid.volume[j] - test$Ask.volume[j])
        test$quantity[j] <- test$quantity[j - 1] - test$bid_quantity[j]
        test$sell_cost[j] <- test$bid_quantity[j] * mean(c(test$bid[j], test$ask[j]))
        # test$p_l[j] <- x * mean(c(test$bid[j], test$ask[j])) - x * test$price[j]
      }
      if(test$Bid.volume[j] < test$Ask.volume[j]){ # More ask prices, so we would buy
        test$ask_quantity[j] <- (test$Ask.volume[j] - test$Bid.volume[j])
        test$quantity[j] <- test$quantity[j - 1] + test$ask_quantity[j]
        test$buy_cost[j] <- test$ask_quantity[j] * mean(c(test$bid[j], test$ask[j]))
      }
    test$u_p_l[j] <- test$quantity[j] * test$price[j] - test$quantity[1] * test$price[1]
      }
# }

p_l_test(I = .175,
         v0 = mean(c(test$Bid.price[1], test$Ask.price[1])),
         sd = sd(btc$Bid.price, na.rm = T),
         trade_prob = .5,
         noise_sd = .025,
         quantity = 200)

(btc$Bid.volume[1] - btc$Ask.volume[1]) > 0

which((test$Bid.volume  - test$Ask.volume) < 0)

sum(test$Bid.volume)
sum(test$Ask.volume)


# =============================== P_L logic ============================
# Initializing the data and variables
test <- btc[c(-1000:-nrow(btc)), ]
test$position[1] <- 20
real_price <- numeric(nrow(test))
for(i in 1:nrow(test)){
  real_price[i] <- mean(c(test$Bid.price[i], test$Ask.price[i]))
}
test$real_price <- real_price
sd <- sd(btc$Bid.price, na.rm = T)

# now setting our bid and ask prices
bid <- numeric(nrow(test))
ask <- numeric(nrow(test))
for(j in 1:nrow(test)){
    bid[j] <- P_noise_bid(I = .175, v0 = real_price[j], sd = sd/10, 
                             Bid_Price = test$Ask.price[j], trade_prob = .5, noise_sd = .05)
    ask[j] <- P_noise_ask(I = .175, v0 = real_price[j], sd = sd/10, 
                               Ask_Price = test$Bid.price[j], trade_prob = .5, noise_sd = .05)
    # if((bid[j] > ask[j]) && (abs(ask[j] - bid[j]) < 25)){
    #   x <- bid[j]
    #   y <- ask[j]
    #   bid[j] <- y
    #   ask[j] <- x
    # } else {
    #   bid[j] <- P_noise_bid(I = .175, v0 = real_price[j], sd = sd/10, 
    #               Bid_Price = test$Bid.price[j], trade_prob = .5, noise_sd = .05)
    #   ask[j] <- P_noise_ask(I = .175, v0 = real_price[j], sd = sd/10, 
    #                         Ask_Price = test$Ask.price[j], trade_prob = .5, noise_sd = .05)
    # }
}
test$bid <- bid
test$ask <- ask
test$new_spread <- test$ask - test$bid 
summary(test$new_spread)

price <- numeric(nrow(test))
for(i in 1:nrow(test)){
  price[i] <- mean(c(test$bid[i], test$ask[i]))
}
test$price <- price

test$p_l[1] <- 0
test$avg_open[1] <- 10800


cond1 <- test$Bid.volume == test$Ask.volume
cond2 <- test$Bid.volume > test$Ask.volume # We would sell to the extra buyers

# if Ask is greater, then we would buy from the extra sellers to maintain even spreads

for(j in 2:nrow(test)){
  test$avg_open[j] <- test$avg_open[j - 1]
  test$p_l[j] <- test$p_l[j - 1]
  if(cond1[j]){next}
  if(cond2[j]){
    x <- test$Bid.volume[j] - test$Ask.volume[j]
    test$position[j] <- test$position[j-1] - x
    test$p_l[j] <- test$p_l[j - 1] + ((test$ask[j] - test$avg_open[j]) * x)
  } else {
    y <- test$Ask.volume[j] - test$Bid.volume[j]
    test$position[j] <- test$position[j - 1] + y
    test$avg_open[j] <- (((y * test$bid[j]) + (test$position[j - 1] * test$avg_open[j - 1]))
    / test$position[j])
  }
}

# bug check, the spread spikes here
length(which(test$Bid.volume > test$Ask.volume))
length(which(test$Bid.volume == test$Ask.volume))

# ================================= Function with time ===================================
op <- options(digits.secs = 2)

# Happy with tests, let us create a function to test and execute trades every half second

# First we will change all timestamps to as.posix values
strptime(as.character(test$Timestamp[1]), format = "%m-%d-%Y %H:%M:%OS")


# Testing transactions at this interval, I need to decrease the interval
# this looks likes an interval every half second
time.vec <- seq(ISOdatetime(2019,6,23,09,01,04), ISOdatetime(2019,6,23,09,19,20), by=(60*.0085))
time.vec[3] > strptime(as.character(test$Timestamp[1]), format = "%m-%d-%Y %H:%M:%OS")

head(time.vec)
# Actually implementing the time vec, and will execute trades every half second
# I will start with already calculated bid and ask prices

time.vec[3] > strptime(as.character(test$Timestamp[1]),format = "%m-%d-%Y %H:%M:%OS") 

# Check the code again
bid.vol <- 0
ask.vol <- 0
for(i in 2:(nrow(test))){
  for(j in 1:(length(time.vec) - 1)){
    if(time.vec[j] <  strptime(as.character(test$Timestamp[i]),format = "%m-%d-%Y %H:%M:%OS") 
       && time.vec[j + 1] >  strptime(as.character(test$Timestamp[i]), format = "%m-%d-%Y %H:%M:%OS")){ # Execute trade
      if(bid.vol > ask.vol){ # We sell the difference
        x <- bid.vol - ask.vol
        test$position[i] <- test$position[i - 1] - x
        test$p_l[i] <- test$p_l[i - 1] + ((test$ask[j] - test$avg_open[j]) * x)
      } else { # ask must be greater than bid, so we buy the diff
        y <- ask.vol - bid.vol
        test$position[i] <- test$position[i - 1] + y
        test$avg_open[i] <- (y * test$bid[i] + test$avg_open[i - 1] * test$position[i - 1]) / test$position[i]
      }
      # These would be reset after each trade
      bid.vol <- 0
      ask.vol <- 0
    } else {
      bid.vol <- bid.vol + test$Bid.volume[i]
      ask.vol <- ask.vol + test$Ask.volume[i]
      test$p_l[i] <- test$p_l[i - 1]
      test$position[i] <- test$position[i - 1]
      test$avg_open[i] <- test$avg_open[i - 1]
    }
  }
}

# ============================== Actual functions =====================================

# Once I finish logic for the half second intervals, then i can create functions
# A function for calculating bid and ask prices
# A profit loss function, with the buy and sell orders








