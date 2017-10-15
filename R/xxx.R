post=function(theta)
{
  theta^3*(1-theta^3)*pbinom(2,10,theta)
}

my.theta=seq(0,1,0.01)
pp=post(my.theta)
plot(pp~my.theta)

post2=function(theta)
{
  theta^3*(1-theta^3)*dbinom(2,10,theta)  
}

pp2=post2(my.theta)
points(my.theta,pp2)
