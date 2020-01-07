
# quick function to calculate the standardized z-scores
z_std <- function(x){
  result <- (x - mean(x)) / sd(x)
  return(result)
}