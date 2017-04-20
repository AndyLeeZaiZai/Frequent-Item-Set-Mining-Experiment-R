#function that calculate d-bound of a dataset
calDbound <- function(data)
{
  dbound <- 0
  
  for(i in 1:length(data))
  {
    list <- unlist(strsplit(data[i], split=" "))
    
    if(length(list)>dbound)
      dbound <- length(list)
  }
  
  dbound
}


#function that calculate d-bound sample size
calDboundSize <- function(d, epsi, delt)
{
  x <- 1/delt
  temp = d + log(x, base = exp(1))
  temp = 4/(epsi * epsi) * temp
  temp
}
  
  

#function that calculate  Toivonen's bound sample size
calTboundSize <- function(epsi, delt)
{
  x <- 2/delt
  temp <- (1/(2 * epsi)) * log(x, base = exp(1))
  temp
}

#function that calculate lower threshold
calLowerT <- function(n, mu)
{
  x <- 1/mu
  temp <- (1/(2 * n)) * log(x, base = exp(1))
  temp <- sqrt(temp)
  temp
}



