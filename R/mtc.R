#' R has many built-in data sets, such as `mtcars`, which contains information about a number of cars:
mtcars
#' Perhaps better is to display the structure:
str(mtcars)
#' how about plotting gas mileage `mpg` against number of cylinders
#' `cyl`?
library(ggplot2)
ggplot(mtcars,aes(x=cyl,y=mpg))+geom_point()+geom_smooth(method="lm")
#' The gas mileage goes down as the number of cylinders goes up
#' that is, a bigger engine uses more gas.