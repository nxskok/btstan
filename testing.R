bto.sc=compile.model()
fname="/home/ken/teaching/stan/sx.dat"
ans=run.rating(fname,bto.sc,20)
head(ans$tab)

ng=0
xy=make.clean(fname,ng)
gc=game.count(xy$x)
gc
res=estimate(xy,bto.sc)
df=data.frame(team=names(res$beta),gc,elo.scale(res$beta))
