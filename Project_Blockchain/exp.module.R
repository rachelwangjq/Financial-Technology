exp.module <- function(M, N , d){
# This function calculates the value of (M^d) mod (N). The function is specifically
# Designed in order to overcome the difficulty that M^d could be huge and it might 
# be computationally infeasible to compute the module we want.
  
  # The first element of chain_mod is (M^1) mod (N)  
  chain_mod <- c(M%%N)
  new <- c()
  k <- 2
  
  while (1) {
    new <- (chain_mod[k-1] * M) %% N
    if (new != chain_mod[1]) {
      chain_mod[k] <- new
      k <- k+1
    } else {
      break
    }
  }
  
  chain_length <- length(chain_mod)
  
  ind <- ((d-1) %% chain_length) + 1
  
  c <- chain_mod[ind]
  return(c)
}