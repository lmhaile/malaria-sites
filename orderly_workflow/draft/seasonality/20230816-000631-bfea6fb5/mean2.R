mean2 <- function(x, na.rm = TRUE){
  return(mean(x[x>=0], na.rm = na.rm))
}
