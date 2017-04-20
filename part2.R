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
  temp <- (1/(2 * epsi * epsi)) * log(x, base = exp(1))
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

#find match numbers using hash
hashCompare <- function(itemdf, h)
{
  matchNum <- 0
  for(i in 1:length(itemdf$items))
  {
  temp <- as.character(itemdf$items[i])
  if(has.key(temp, h))
    matchNum <- matchNum + 1
  }
  matchNum
}


#Read datasets from online location
library("arules")

Dataset1 <- readLines("http://fimi.ua.ac.be/data/T10I4D100K.dat")
Dataset2 <- read.transactions("http://fimi.ua.ac.be/data/T10I4D100K.dat")

DataK1 <- readLines("C:\\Users\\icmonkey\\Desktop\\Big Data\\Experiment\\kosarak.dat")
DataK2 <- read.transactions("C:\\Users\\icmonkey\\Desktop\\Big Data\\Experiment\\kosarak.dat")

epsilon <- 0.005
delta <- 0.01
mu1 <- 0.0001
mu2 <- 0.00001

d <- 443

dSampleSize <- calDboundSize(d, epsilon, delta)
tSampleSize <- calTboundSize(epsilon, delta)
lowerT1 <- calLowerT(tSampleSize, mu1)
lowerT2 <- calLowerT(tSampleSize, mu2)

lowerT1
lowerT2
tSampleSize
dSampleSize

itemsets <- eclat(DataK2B1, parameter = list(supp = 0.01, maxlen = 15))

#dSample <- sample(DataK2B1, dSampleSize, replace = TRUE)
tSample <- sample(DataK2B1, tSampleSize, replace = TRUE)

#itemsetsd <- eclat(dSample, parameter = list(supp = 0.01, maxlen = 15))

itemsetst1 <- eclat(tSample, parameter = list(supp = 0.01-lowerT1, maxlen = 15))

itemsetst2 <- eclat(tSample, parameter = list(supp = 0.01-lowerT2, maxlen = 15))

h <- hash(as(itemsets, "data.frame")$items, 1:length(as(itemsets, "data.frame")$items))

itemsetddf0 <- as(itemsetsd, "data.frame")
matchNum0 <- hashCompare(itemsetddf0,h)
macthPercent0 <- matchNum0/length(itemsets)

itemsetddf1 <- as(itemsetst1, "data.frame")
matchNum1 <- hashCompare(itemsetddf1,h)
macthPercent1 <- matchNum1/length(itemsets)

itemsetddf2 <- as(itemsetst2, "data.frame")
matchNum2 <- hashCompare(itemsetddf2,h)
macthPercent2 <- matchNum2/length(itemsets)

macthPercent0
macthPercent1
macthPercent2


