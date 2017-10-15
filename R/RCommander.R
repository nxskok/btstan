
load("/home/ken/teaching/btstan/R/stuff.txt")
load("/home/ken/teaching/btstan/R/xy.txt")
library(relimp, pos=18)
editDataset(xy)
showData(xy, placement='-20+200', font=getRcmdr('logFont'), 
  maxwidth=80, maxheight=10)
scatterplot(y~x, reg.line=FALSE, smooth=FALSE, spread=FALSE, 
  boxplots=FALSE, span=0.5, ellipse=FALSE, levels=c(.5, .9), 
  data=xy)
with(xy, lineplot(x, y))
scatterplot(y~x, reg.line=lm, smooth=FALSE, spread=FALSE, 
  boxplots='xy', span=0.5, ellipse=FALSE, levels=c(.5, .9), data=xy)
editDataset(xy)
scatterplot(y~x, reg.line=lm, smooth=FALSE, spread=FALSE, 
  boxplots=FALSE, span=0.5, ellipse=FALSE, levels=c(.5, .9), 
  data=xy)
showData(xy, placement='-20+200', font=getRcmdr('logFont'), 
  maxwidth=80, maxheight=10)
editDataset(xy)
showData(xy, placement='-20+200', font=getRcmdr('logFont'), 
  maxwidth=80, maxheight=10)
scatterplot(y~x, reg.line=lm, smooth=FALSE, spread=FALSE, 
  boxplots=FALSE, span=0.5, ellipse=FALSE, levels=c(.5, .9), 
  data=xy)
RegModel.1 <- lm(y~x, data=xy)
summary(RegModel.1)
Anova(RegModel.1, type="II")

