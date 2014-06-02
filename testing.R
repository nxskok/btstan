bto.sc=compile.model()
#fname="/home/ken/sports/sx.dat"
fname="/home/ken/teaching/stan/cfl.dat"
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



ans=run.rating(fname,bto.sc,0,T)
head(ans$tab)
ans$cc
ans$tab
ans$pred
pred(ans,1,1)
ng=0
xy=make.clean(fname,ng)
gc=game.count(xy$x)
gc
res=estimate(xy,bto.sc)
df=data.frame(team=names(res$beta),gc,elo.scale(res$beta))
