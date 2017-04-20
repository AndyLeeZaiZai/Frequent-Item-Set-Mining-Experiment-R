## Create transaction data set.
data <- list(
  c("a","b","c"),
  c("a","b"),
  c("a","b","d"),
  c("b","e"),
  c("b","c","e"),
  c("a","d","e"),
  c("a","c"),
  c("a","b","d"),
  c("c","e"),
  c("a","b","d","e")
)

t <- as(data, "transactions")

## Mine itemsets with tidLists.
f <- eclat(data, parameter = list(support = 0.3, maxlen = 1, tidLists = TRUE))

## Get dimensions of the tidLists.
dim(tidLists(f))

## Coerce tidLists to list.
as(tidLists(f), "list")

## Inspect visually.
image(tidLists(f))

##Show the Frequent itemsets and respectives supports
inspect(f)


tlist <- as(t, "data.frame")


hash(as(itemsetsd1, "data.frame")$items, 1:length(as(itemsetsd1, "data.frame")$items))
has.key("{108,438,684,8,958}", h2)


Dataset2B <- do.call("rbind", replicate(20, as(Dataset2, "list"), simplify = FALSE))
Dataset2B1 <- as(Dataset2B, "list")
Dataset2B1 <- as(Dataset2B1, "transactions")


as.vector(Dataset11)

hashCompare <- function(itemdf, h)
{
  matchNum <- 0
  #for(i in 1:length(itemdf$items))
  #{
    temp <- as.character(itemdf$items[1])
    if(has.key(temp, h))
      matchNum <- matchNum + 1
    matchNum
  }
  
}



