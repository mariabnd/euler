# First by hand, we find that using the coins
# 1, 2, and 5
# we could obtain the value of 5 in four
# unique ways namely
# 5 = 5
# 5 = 2 + 2 + 1
# 5 = 2 + 1 + 1 + 1
# 5 = 1 + 1 + 1 + 1 + 1

pe_31 <- function(coins, target){
  
  # Create empty matrix to populate with values
  mixes <- matrix(NA, nrow = length(coins), ncol = target + 1)
  # Set target 0 to 1 as there is one way to make up no money
  mixes[1, 1] <- 1
  
  for (i in 1 : length(coins)){
    if (i > 1) # Cannot add from row above very first row
      mixes[i, 1 : coins[i]] <- mixes[i - 1, 1 : coins[i]] # Skip lower levels
    for (j in max(2, coins[i]) : dim(mixes)[2]){
      ifelse(j - 1 >= coins[i],
             # If there is a remainder add previous
             ifelse(i > 1, # Cannot add from row above very first row
                    mixes[i, j] <- mixes[i, j - coins[i]] + mixes[i - 1, j],
                    mixes[i, j] <- mixes[i, j - coins[i]]),
             if (i > 1) # Cannot add from row above very first row
               mixes[i, j] <- mixes[i - 1, j])
    }
  }
  # Get the value of interest
  return(mixes[length(coins), target + 1])
}

pe_31(coins = c(1, 2, 5), target = 5) # 4
