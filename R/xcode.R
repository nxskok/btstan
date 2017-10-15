#' (a) Enter the values as shown:
x=c(10,11,13,15,16,17,19,24,32)
x
#' This demonstrates that the values entered were indeed correct.
#' 
#' (b) This gets both mean and median in one go:
summary(x)
#' The mean is 17.44, and the median is 16. The mean is noticeably bigger.
#' This suggests that the data are right-skewed. 
#' 
#' (c)
boxplot(x)
#' On the boxplot, the upper whisker is a little longer and there is an upper outlier. 
#' This supports our conclusion that the data are right-skewed.
