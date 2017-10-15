library(tidyr)
x=rnorm(20)
y=rep(c('a','b'),2)
z=data.frame(x,y)
z
spread(z,y,x)
?spread

##################################

library(dplyr)
stocks <- data.frame(
  time = as.Date('2009-01-01') + 0:9,
  X = rnorm(10, 0, 1),
  Y = rnorm(10, 0, 2),
  Z = rnorm(10, 0, 4)
)
stocks
stocksm <- stocks %>% gather(stock, price, -time)
stocksm
stocksm %>% spread(stock, price)
?row.names
