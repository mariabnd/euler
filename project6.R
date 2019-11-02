# Wrap this in a function
pe_6 <- function(x){
  a <- sum(x ** 2)
  b <- sum(x) ** 2
  return(b - a)
}

# Example calculations
pe_6(1 : 10) # 2640
pe_6(1 : 100) # 25164150
