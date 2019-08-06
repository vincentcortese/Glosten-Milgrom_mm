
setwd("C:/Users/Vincent/Documents/Bitkub/glosten-milgrom_mm")
btcusd <- read.csv(file = "BTCUSD.csv")

btcusd$spead <- btcusd$Ask.price - btcusd$Bid.price
summary(btcusd$spead)
summary(btcusd$Ask.price)

v0 <- mean(btcusd$Bid.price[1], btcusd$Ask.price[1])
sd <- sd(btcusd$Ask.price)

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
    temp <- (I*(1 - pnorm(x, (lst2[i] - Bid_Price), .1)) + 
               (1 - I)*trade_prob)*dnorm(lst2[i], v0, sd)
    count1 <- count1 + temp
  }
  return(count + count1)
}

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
    temp <- (I*(1 - pnorm(x, (lst2[i] - Bid_Price), .1)) + 
               (1 - I)*trade_prob)*lst2[i]*dnorm(lst2[i], v0, sd)
    count1 <- count1 + temp
  }
  return (1/P_noise_sell(I, v0, sd, Bid_Price, trade_prob, noise_sd) * (count + count1))
}

# Reducing the amount of I, decreases spread
# v0 plays a significant role in determining the price
# increasing trade_prob, decreases spread


P_noise_b(I = .5, 
          v0 = v0, 
          sd = sd, 
          Bid_Price = btcusd$Bid.price[1], 
          trade_prob = .5, 
          noise_sd = .0005)
btcusd$Bid.price[1]

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
P_noise_b(I = .5, 
          v0 = v0, 
          sd = sd, 
          Bid_Price = btcusd$Bid.price[1], 
          trade_prob = .5, 
          noise_sd = .00005)
P_noise_ask(I = .5, 
            v0 = btcusd$Ask.price[1], 
            sd = sd(btcusd$Ask.price, na.rm = T), 
            Ask_Price = btcusd$Ask.price[1], 
            trade_prob = .5, 
            noise_sd = .00005)
btcusd$Ask.price[1]
btcusd$Bid.price[1]


# ================================= test with the equations ============
summary(btcusd$Bid.price)
sd <- sd(btcusd$Bid.price, na.rm = T)
lst <- seq(from = v0 - 4 * sd, to = v0 + 4*sd, by = sd)
test <- btcusd[c(-10000:-nrow(btcusd)), ]
test$calc <- NA
for(j in 1:nrow(test)){
  if(v0 > max(lst) || v0 < min(lst)){
    lst <- seq(from = v0 - 4 * sd, to = v0 + 4*sd, by = sd)
  if(count >= 6){
    v0 <- mean(c(test$bid[j], test$ask[j]))
    count <- 0
  } else {
    tryCatch({
      test$bid[j] <- P_noise_b(I = .3, v0 = v0 - 20, sd = sd, 
                               Bid_Price = btcusd$Bid.price[j], trade_prob = .5, noise_sd = .00025)
      test$ask[j] <- P_noise_ask(I = .3, v0 = v0 + 30, sd = sd, 
                                 Ask_Price = btcusd$Ask.price[j], trade_prob = .5, noise_sd = .00025)
      count <- count + 1
    }, error = function(e){})
  }
  }
}
test$Calc_spread <- test$ask - test$bid

