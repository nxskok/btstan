
compile.model=function()
{
  model='
data
{
  int<lower=2> N; // number of games
int<lower=1> D; // number of teams
int<lower=1,upper=3> y[N]; // results
int x[N,2]; // indices of teams playing
}

parameters
{
vector[D] beta;
real<lower=0> cc;
}

model
{
real nu;
int y1;
int y2;
vector[2] d;
beta~normal(0,2);
cc~normal(0,10);
for (i in 1:N)
{
y1 <- x[i,1];
y2 <- x[i,2];
nu <- beta[y1]-beta[y2];
d[1] <- -cc;
d[2] <- cc;
y[i] ~ ordered_logistic(nu,d);
}
}'
  stan_model(model_code=model)
}


get.res=function(fname)
{
  res=read.table(fname,sep=":",quote="\"",stringsAsFactors=F)
  res
}

makexy=function(res)
{
  f=factor(c(res[,2],res[,3]))
  ll=split.half(f)
  x=cbind(as.numeric(ll[[1]]),as.numeric(ll[[2]]))
  y=res[,4]+1
  list(x=x,y=y,names=levels(f))
}

estimate=function(xylist,bto.sc)
{
  stopifnot(is.list(xylist))
  xx=xylist[[1]]
  yy=xylist[[2]]
  N=length(yy)
  K=3
  D=max(xx)
  v=optimizing(bto.sc,list(N=length(yy),K=3,D=max(xx),y=yy,x=xx))
  #        v=optimizing(bto.sc,c("N","K","D","y","x"))
  theta=v$par
  n=length(theta)
  beta=theta[1:(n-1)]
  names(beta)=xylist[[3]]
  cc=theta[n]
  ll=list(beta=beta,cc=cc)
  ll
}

game.count=function(x,tf=rep(T,nrow(x)))
{
  gc=table(c(x[tf,1],x[tf,2]))
  gc
  gg=numeric(max(xy$x))
  gg[as.numeric(names(gc))]=gc
  gg
}

keep1=function(x,ng,tf=rep(T,nrow(x)))
{
  # indicate which rows of x have both teams playing
  # at least ng games for which tf is T (not prev eliminated)
  gp=game.count(x,tf)
  c1=gp[x[,1]]
  c2=gp[x[,2]]
  pp=(pmin(c1,c2)>=ng)
  pp=ifelse(is.na(pp),F,pp)
  pp
}

keep.n=function(x,ng)
{
  tf=rep(T,nrow(x))
  for (i in 1:3)
  {
    tf=keep1(x,ng,tf)
    print(table(tf))
  }
  tf
}

make.clean=function(fname,ng)
{
  res=get.res(fname)
  xy=makexy(res)
  tf=keep.n(xy$x,ng)
  res=res[tf,]
  xy=makexy(res)
  xy
}

split.half=function(x)
{
  sf=ceiling(2*seq_along(x)/length(x))
  split(x,sf)
}



elo.scale=function(rat)
{
  round(1500+173.7178*rat)
}


run.rating=function(fname,model,ng=0,predictions=T)
{
  # eliminate teams with fewer than ng games
  xy=make.clean(fname,ng)
  gc=game.count(xy$x)
  res=estimate(xy,model)
  df=data.frame(team=names(res$beta),gc,elo.scale(res$beta))
  o=order(-res$beta)
  dg=df[o,]
  row.names(dg)=1:length(gc)
  names(dg)=c("Team","Games","Rating")
  # predictions
  if (predictions)
  {
    pp=pred.all(res)
  }
  
  list(tab=dg,cc=res$cc,pred=pp)
}


pred=function(i,j,ll)
{
  eta=ll$beta[i]-ll$beta[j];
  cc=c(-ll$cc,ll$cc)
  xx=outer(eta,cc,"-")
  p=plogis(xx)
  p=cbind(1,p,0)
  t(-apply(p,1,diff))
}


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
