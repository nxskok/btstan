library(ggplot2)
x=rnorm(100,50,10)
x
d=data.frame(x)
m=mean(x)
s=sd(x)

f=function(x,mean,sd,n,bw) {dnorm(x=x,mean=mean,sd=sd)*n*bw}
ggplot(d,aes(x=x))+geom_histogram(binwidth=5)+
  stat_function(fun=f, 
    args=list(mean=mean,sd=sd,n=length(x),bw=5))

x=rnorm(100,20,5)
y=rnorm(100,40,5)
v=c(x,y)
g=c(rep("a",100),rep("b",100))
d=data.frame(v,g)    
d
ggplot(d,aes(x=v,colour=g))+geom_histogram(bins=20)

d2=data.frame(x,y)
d2
ggplot(d2,aes(x=x),colour="red")+geom_histogram(bins=20)+
  geom_histogram(aes(x=y),bins=20)
