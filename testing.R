library(rstan)
bto.sc=compile.model()
fname="/home/ken/sports/scoresway/soccer.dat"
fname="/home/ken/sports/scoresway/ix.dat"

fname="/home/ken/sports/sx.dat"
fname="/home/ken/teaching/stan/cfl.dat"

ans=run.rating(fname,bto.sc,10)

head(ans$tab)
ans$tab
ans$cc

xy=make.clean(fname,ng)
gc=game.count(xy$x)
res=estimate(xy,bto.sc)
pred(1,2,res)
res
outer(1:4,1:4,pred,res)

pred(1:2,3,res)
# doesn't work: pred needs to be vectorized

i=1
j=3
res$beta
eta=res$beta[i]-res$beta[j];
eta
cc=c(-res$cc,res$cc)
cc
xx=outer(eta,cc,"-")
xx
p=plogis(xx)
p
p=cbind(1,p,0)
p
# this converts rows to columns but is otherwise kosher
-apply(p,1,diff)

res
pred.all(res)
pred(1,2,res)
eta=res$beta[1:2]-res$beta[3:4];
eta
cc=c(-res$cc,res$cc)
cc
xx=outer(eta,cc,"-")
xx
p=plogis(xx)
p=cbind(1,p,0)
p
t(-apply(p,1,diff))
-diff(p)
pred(1:3,2:4,res)

res
pp=pred.all(res)
pp[1,3,]

pred.all=function(ll)
{
  n=length(ll$beta)
  ans=array(0,c(n,n,3))
  for (i in 1:n)
  {
    for (j in 1:n)
    {
      ans[i,j,]=pred(i,j,ll)
    }
  }
  ans
}

fname="latvia.dat"
fname
ans=run.rating(fname,bto.sc,10)
names(ans)
ans$tab
head(ans$tab)
ans$cc
ans$tab
ans$pred
pred(ans,1,1)
pred.all(ans)
ans
ng=0
xy=make.clean(fname,ng)
gc=game.count(xy$x)
gc
res=estimate(xy,bto.sc)
df=data.frame(team=names(res$beta),gc,elo.scale(res$beta))

#make up some data for testing pred.all

res=list(beta=c(2,1.5,1,0.5),cc=0.8)
ans=pred.all(res)
ans[2,4,]

pp=numeric(0)
for (i in 1:4)
{
  for (j in 1:4)
  {
    pp=rbind(pp,c(i,j,ans[i,j,]))
  }
}
pp

# international soccer stuff
library(rstan)

fname="/home/ken/sports/scoresway/ix.dat"
xylist=make.clean(fname,10)
gc=game.count(xylist$x)
xx=xylist[[1]]
yy=xylist[[2]]
nn=xylist[[3]]
N=length(yy)
K=3
D=max(xx)
v=sampling(bto.sc,list(N=length(yy),K=3,D=max(xx),y=yy,x=xx))
#        v=optimizing(bto.sc,c("N","K","D","y","x"))
ss=extract(v)
ss$cc

which.cut=function(i,r,b)
{
  w=cut(r[i],breaks=b[i,])
  as.numeric(w)
}

probs=function(t1,t2,nn,ss)
{
  n1=which(nn==t1)
  n2=which(nn==t2)
  eta=ss$beta[,n1]-ss$beta[,n2]
  cmeta=cbind(ss$c-eta,-ss$c-eta)
  n=nrow(cmeta)
  p=cbind(1,plogis(cmeta),0)
  r=runif(n)
  table(sapply(1:n,which.cut,r,p))/n
}

probs("Costa Rica", "England", nn, ss)
probs("Italy","Uruguay",nn,ss)
probs("Greece",nn[42],nn,ss)
probs("Japan","Colombia",nn,ss)

probs("Bosnia-Herzegovina","Iran",nn,ss)
probs("Nigeria","Argentina",nn,ss)
probs("Ecuador","France",nn,ss)
probs("Honduras","Switzerland",nn,ss)
probs("Portugal","Ghana",nn,ss)
probs("United States","Germany",nn,ss)
probs("Algeria","Russia",nn,ss)
probs("Korea Republic","Belgium",nn,ss)

probs("France","Germany",nn,ss)
probs("Brazil","Colombia",nn,ss)
probs("Argentina","Belgium",nn,ss)
probs("Netherlands","Costa Rica",nn,ss)
nn
nn[42]
178 137
probs("Brazil","Chile",nn,ss)
probs("Colombia","Uruguay",nn,ss)
probs("Netherlands","Mexico",nn,ss)
probs("Costa Rica","Greece",nn,ss)
probs("France","Nigeria",nn,ss)
probs("Germany","Algeria",nn,ss)
probs("Argentina","Switzerland",nn,ss)
probs("Belgium","United States",nn,ss)

probs("Brazil","Germany",nn,ss)
probs("Netherlands","Argentina",nn,ss)
probs("Brazil","Netherlands",nn,ss)
probs("Germany","Argentina",nn,ss)

1/c(2.23,3.825,3.475)
1/c(2.43,3.275,3.575)

summary(ss$beta[,178])
summary(ss$beta[,137])
# posterior means
mm=apply(ss$beta,2,mean)
mm
o=order(-mm)
relo=1500+172.73*mm
data.frame(name=nn[o],rat=mm[o],elo=relo[o])
